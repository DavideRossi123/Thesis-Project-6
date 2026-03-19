import warnings
warnings.filterwarnings("ignore")

import os
import sys
import numpy as np
import pandas as pd
import xarray as xr
import dask

dask.config.set(scheduler="threads")

import copernicusmarine as cm
from scipy.spatial import cKDTree

# -------------------------------------------------------------------
# CONFIG (defaults can be overridden via environment variables)
# -------------------------------------------------------------------
# If running as a script, set POINTS_CSV and OUTPUT_CSV
POINTS_CSV = os.getenv("POINTS_CSV", "")
OUTPUT_CSV = os.getenv("OUTPUT_CSV", "")

# Optional: override year/month for all rows
FORCE_YEAR = os.getenv("FORCE_YEAR", "")
FORCE_MONTH = os.getenv("FORCE_MONTH", "")

# Optional: override column names
COL_LON = None
COL_LAT = None
COL_YEAR = None
COL_MONTH = None

# Auto-detect candidates (EPSG:4326 decimal degrees)
LON_CANDIDATES = ["longitude", "lon", "long", "shooting_longitude"]
LAT_CANDIDATES = ["latitude", "lat", "shooting_latitude"]
YEAR_CANDIDATES = ["year", "yyyy", "yr"]
MONTH_CANDIDATES = ["month", "mm", "mon"]

# Build monthly timestamp using this day-of-month
MONTH_DAY = 15

# Pad bounding box around points (degrees)
PAD_DEG = 0.25

# Optional: fill NaNs using nearest neighbor in space
USE_KDTREE_IMPUTE = True

# Optional: SST is often in Kelvin
SST_KELVIN_TO_C = True

# Allowed year range (inclusive) for validation
MIN_YEAR = int(os.getenv("MIN_YEAR", "2000"))
MAX_YEAR = int(os.getenv("MAX_YEAR", "2021"))

# -------------------------------------------------------------------
# Dataset IDs
# -------------------------------------------------------------------
# Bottom temperature: try monthly, fallback to daily
BOTTOM_TEMP_MONTHLY_DATASET_ID = "cmems_mod_glo_phy_my_0.083deg_P1M-m"
BOTTOM_TEMP_DAILY_DATASET_ID = "cmems_mod_glo_phy_my_0.083deg_P1D-m"
BOTTOM_TEMP_VAR = "bottomT"

# Surface temperature: build monthly from daily analyses
SST_VAR = "analysed_sst"
SST_DATASET_ID_ESACCI = "ESACCI-GLO-SST-L4-REP-OBS-SST"  # 1981-2016
SST_DATASET_ID_C3S = "C3S-GLO-SST-L4-REP-OBS-SST"        # 1982-2024
SST_SWITCH_YEAR = 2017


# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------

def resolve_col(df, explicit, candidates, label):
    if explicit:
        if explicit not in df.columns:
            raise ValueError(f"{label} column '{explicit}' not found")
        return explicit

    for c in candidates:
        if c in df.columns:
            return c

    lower_map = {c.lower(): c for c in df.columns}
    for c in candidates:
        if c.lower() in lower_map:
            return lower_map[c.lower()]

    raise ValueError(
        f"Could not detect {label} column. Set COL_{label.upper()} explicitly."
    )


def parse_month(series):
    if pd.api.types.is_categorical_dtype(series):
        series = series.astype(str)

    month_num = pd.to_numeric(series, errors="coerce")
    name_abbrev = pd.to_datetime(series, format="%b", errors="coerce").dt.month
    name_full = pd.to_datetime(series, format="%B", errors="coerce").dt.month
    month_num = month_num.fillna(name_abbrev).fillna(name_full)
    return month_num


def impute_nearest(values, coords):
    # Copy to a writable ndarray because pandas/reticulate can expose read-only views.
    values = np.asarray(values, dtype=float).copy()
    nan_idx = np.isnan(values)
    if not nan_idx.any():
        return values
    valid_idx = ~nan_idx
    if not valid_idx.any():
        return values
    tree = cKDTree(coords[valid_idx])
    _, nearest_idx = tree.query(coords[nan_idx], k=1)
    values[nan_idx] = values[valid_idx][nearest_idx]
    return values


def report_nans(series, label):
    nan_count = series.isna().sum()
    total_count = len(series)
    if total_count == 0:
        print(f"{label}: empty")
        return
    print(f"{label} NaN proportion: {nan_count / total_count:.4%} ({nan_count} / {total_count})")


def open_dataset(dataset_id, variables, lon_min, lon_max, lat_min, lat_max, start, end):
    return cm.open_dataset(
        dataset_id=dataset_id,
        variables=variables,
        minimum_longitude=float(lon_min),
        maximum_longitude=float(lon_max),
        minimum_latitude=float(lat_min),
        maximum_latitude=float(lat_max),
        start_datetime=pd.Timestamp(start),
        end_datetime=pd.Timestamp(end),
    )


def _available_periods_from_ds(ds):
    if "time" not in ds.coords:
        return set()
    times = pd.to_datetime(ds["time"].values)
    return set(pd.PeriodIndex(times, freq="M"))


def _validate_year_month(year_num, month_num, enforce_range=True):
    if year_num.isna().any():
        bad = year_num[year_num.isna()].index[:10].tolist()
        raise ValueError(f"Year parse failed for rows (showing up to 10): {bad}")
    if month_num.isna().any():
        bad = month_num[month_num.isna()].index[:10].tolist()
        raise ValueError(f"Month parse failed for rows (showing up to 10): {bad}")

    if enforce_range:
        bad_year = ~year_num.between(MIN_YEAR, MAX_YEAR)
        bad_month = ~month_num.between(4, 12)
        if bad_year.any():
            bad_vals = sorted(year_num[bad_year].unique().tolist())
            raise ValueError(f"Year must be within {MIN_YEAR}-{MAX_YEAR}. Invalid: {bad_vals}")
        if bad_month.any():
            bad_vals = sorted(month_num[bad_month].unique().tolist())
            raise ValueError(f"Month must be within Apr-Dec (4-12). Invalid: {bad_vals}")


def add_monthly_temps(
    df,
    year=None,
    month=None,
    lon_col=None,
    lat_col=None,
    year_col=None,
    month_col=None,
    pad_deg=PAD_DEG,
    month_day=MONTH_DAY,
    use_kdtree_impute=USE_KDTREE_IMPUTE,
    sst_kelvin_to_c=SST_KELVIN_TO_C,
    enforce_range=True,
    keep_month_date=False,
):
    """
    Add bottom_temperature and surface_temperature to a DataFrame.

    Parameters
    ----------
    df : pandas.DataFrame
        Must contain lon/lat columns and year/month columns, unless year/month are provided.
    year, month : optional
        If provided, will override all rows with this year/month.
    """
    if not isinstance(df, pd.DataFrame):
        df = pd.DataFrame(df)

    df = df.copy()

    col_lon = resolve_col(df, lon_col, LON_CANDIDATES, "longitude")
    col_lat = resolve_col(df, lat_col, LAT_CANDIDATES, "latitude")

    if year is not None and str(year) != "":
        col_year = year_col or "year"
        df[col_year] = year
    else:
        col_year = resolve_col(df, year_col, YEAR_CANDIDATES, "year")

    if month is not None and str(month) != "":
        col_month = month_col or "month"
        df[col_month] = month
    else:
        col_month = resolve_col(df, month_col, MONTH_CANDIDATES, "month")

    lon_vals = pd.to_numeric(df[col_lon], errors="coerce")
    lat_vals = pd.to_numeric(df[col_lat], errors="coerce")
    if lon_vals.isna().any() or lat_vals.isna().any():
        raise ValueError("Longitude/latitude contain invalid values.")

    year_num = pd.to_numeric(df[col_year], errors="coerce")
    month_num = parse_month(df[col_month])
    _validate_year_month(year_num, month_num, enforce_range=enforce_range)

    df["month_date"] = pd.to_datetime(
        dict(year=year_num, month=month_num, day=month_day),
        errors="coerce",
    )
    if df["month_date"].isna().any():
        raise ValueError("Failed to build valid month_date for one or more rows.")

    if len(df) == 0:
        raise ValueError("No rows to process.")

    lon_min = lon_vals.min() - pad_deg
    lon_max = lon_vals.max() + pad_deg
    lat_min = lat_vals.min() - pad_deg
    lat_max = lat_vals.max() + pad_deg

    t_start = df["month_date"].min()
    t_end = df["month_date"].max()

    coords = np.column_stack([lon_vals.values, lat_vals.values])

    # -------------------------------------------------------------------
    # Bottom temperature (monthly mean with daily fallback)
    # -------------------------------------------------------------------
    df["month_period"] = df["month_date"].dt.to_period("M")
    bottom_vals = np.full(len(df), np.nan)

    ds_bottom = None
    available_periods = set()
    try:
        ds_bottom = open_dataset(
            BOTTOM_TEMP_MONTHLY_DATASET_ID,
            variables=[BOTTOM_TEMP_VAR],
            lon_min=lon_min,
            lon_max=lon_max,
            lat_min=lat_min,
            lat_max=lat_max,
            start=t_start,
            end=t_end,
        )
        available_periods = _available_periods_from_ds(ds_bottom)
    except Exception as e:
        print(f"Bottom monthly dataset unavailable, falling back to daily: {e}")
        ds_bottom = None

    for period, idx in df.groupby("month_period").groups.items():
        idx = np.asarray(idx)
        lon_da_m = xr.DataArray(lon_vals.iloc[idx].values, dims="points", name="longitude")
        lat_da_m = xr.DataArray(lat_vals.iloc[idx].values, dims="points", name="latitude")

        if ds_bottom is not None and period in available_periods:
            t_da_m = xr.DataArray(df.loc[idx, "month_date"].values, dims="points", name="time")
            bt_vec = ds_bottom[BOTTOM_TEMP_VAR].sel(
                time=t_da_m, longitude=lon_da_m, latitude=lat_da_m, method="nearest"
            )
            bottom_vals[idx] = np.asarray(bt_vec.compute().values, dtype=float)
        else:
            start = period.to_timestamp(how="start")
            end = period.to_timestamp(how="end")
            ds_bottom_daily = open_dataset(
                BOTTOM_TEMP_DAILY_DATASET_ID,
                variables=[BOTTOM_TEMP_VAR],
                lon_min=lon_min,
                lon_max=lon_max,
                lat_min=lat_min,
                lat_max=lat_max,
                start=start,
                end=end,
            )
            bt_daily = ds_bottom_daily[BOTTOM_TEMP_VAR].sel(
                longitude=lon_da_m, latitude=lat_da_m, method="nearest"
            )
            bt_monthly = bt_daily.mean(dim="time", skipna=True)
            bottom_vals[idx] = np.asarray(bt_monthly.compute().values, dtype=float)
            ds_bottom_daily.close()

    if ds_bottom is not None:
        ds_bottom.close()

    df["bottom_temperature"] = bottom_vals
    report_nans(df["bottom_temperature"], "Bottom temperature")

    if use_kdtree_impute:
        df["bottom_temperature"] = impute_nearest(
            df["bottom_temperature"].to_numpy(dtype=float, copy=True), coords
        )

    # -------------------------------------------------------------------
    # Surface temperature (monthly mean computed from daily analyses)
    # -------------------------------------------------------------------
    sst_vals = np.full(len(df), np.nan)

    for period, idx in df.groupby("month_period").groups.items():
        idx = np.asarray(idx)
        if period.year < SST_SWITCH_YEAR:
            sst_dataset_id = SST_DATASET_ID_ESACCI
        else:
            sst_dataset_id = SST_DATASET_ID_C3S

        start = period.to_timestamp(how="start")
        end = period.to_timestamp(how="end")

        ds_sst = open_dataset(
            sst_dataset_id,
            variables=[SST_VAR],
            lon_min=lon_min,
            lon_max=lon_max,
            lat_min=lat_min,
            lat_max=lat_max,
            start=start,
            end=end,
        )

        lon_da_m = xr.DataArray(lon_vals.iloc[idx].values, dims="points", name="longitude")
        lat_da_m = xr.DataArray(lat_vals.iloc[idx].values, dims="points", name="latitude")

        sst_daily = ds_sst[SST_VAR].sel(
            longitude=lon_da_m, latitude=lat_da_m, method="nearest"
        )
        sst_monthly = sst_daily.mean(dim="time", skipna=True)
        sst_vals[idx] = np.asarray(sst_monthly.compute().values, dtype=float)
        ds_sst.close()

    df["surface_temperature"] = sst_vals
    report_nans(df["surface_temperature"], "Surface temperature")

    if use_kdtree_impute:
        df["surface_temperature"] = impute_nearest(
            df["surface_temperature"].to_numpy(dtype=float, copy=True), coords
        )

    if sst_kelvin_to_c:
        df["surface_temperature"] = df["surface_temperature"] - 273.15

    # Cleanup
    df = df.drop(columns=["month_period"])
    if not keep_month_date:
        df = df.drop(columns=["month_date"])

    return df


def add_monthly_temps_as_dict(
    df,
    year=None,
    month=None,
    lon_col=None,
    lat_col=None,
    year_col=None,
    month_col=None,
    pad_deg=PAD_DEG,
    month_day=MONTH_DAY,
    use_kdtree_impute=USE_KDTREE_IMPUTE,
    sst_kelvin_to_c=SST_KELVIN_TO_C,
    enforce_range=True,
    keep_month_date=False,
):
    """
    Reticulate-safe wrapper: returns a plain dict of column -> list.
    This avoids pandas -> R conversion issues.
    """
    df_out = add_monthly_temps(
        df,
        year=year,
        month=month,
        lon_col=lon_col,
        lat_col=lat_col,
        year_col=year_col,
        month_col=month_col,
        pad_deg=pad_deg,
        month_day=month_day,
        use_kdtree_impute=use_kdtree_impute,
        sst_kelvin_to_c=sst_kelvin_to_c,
        enforce_range=enforce_range,
        keep_month_date=keep_month_date,
    )
    return df_out.to_dict(orient="list")


def main():
    if not POINTS_CSV:
        print("POINTS_CSV is not set. Provide a CSV path via env var.")
        sys.exit(2)

    out_csv = OUTPUT_CSV
    if not out_csv:
        base, ext = os.path.splitext(POINTS_CSV)
        out_csv = f"{base}_with_temps.csv"

    year = FORCE_YEAR if FORCE_YEAR != "" else None
    month = FORCE_MONTH if FORCE_MONTH != "" else None

    df = pd.read_csv(POINTS_CSV)
    df_out = add_monthly_temps(df, year=year, month=month)
    df_out.to_csv(out_csv, index=False)
    print(f"Saved: {out_csv}")


def _should_run_main():
    if os.getenv("RUN_MAIN", "") == "1":
        return True
    return POINTS_CSV != ""


if __name__ == "__main__" and _should_run_main():
    main()
