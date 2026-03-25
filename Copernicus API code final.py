
import warnings
warnings.filterwarnings("ignore")
import numpy as np
import pandas as pd
import xarray as xr
import dask
dask.config.set(scheduler="threads")
import copernicusmarine as cm
from scipy.spatial import cKDTree

# SST dataset windows:
# - Reprocessed historical SST for past years
# - NRT SST for most recent years
SST_VAR = "analysed_sst"
SST_DATASET_ID_HIST = "C3S-GLO-SST-L4-REP-OBS-SST"
SST_DATASET_ID_NRT = "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2"
SST_HIST_START = pd.Timestamp("1982-01-01")
SST_NRT_START = pd.Timestamp("2024-01-17")


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


# store MEDITS_MERL_MOD dataset
URL = "https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/MEDITS_MERL_MOD.csv"
MEDITS_MERL_MOD = pd.read_csv(URL)

MEDITS_MERL_MOD["haul_date"] = pd.to_datetime(
    dict(year=MEDITS_MERL_MOD["year"], month=MEDITS_MERL_MOD["month"], day=MEDITS_MERL_MOD["day"]), errors="coerce")

#Keep valid rows
MEDITS_MERL_MOD = MEDITS_MERL_MOD.dropna(subset=["shooting_latitude","shooting_longitude","haul_date"]).reset_index(drop=True)  

#Bottom temperature from Copernicus
pad = 0.25
lon_min, lon_max = MEDITS_MERL_MOD["shooting_longitude"].min() - pad, MEDITS_MERL_MOD["shooting_longitude"].max() + pad
lat_min, lat_max = MEDITS_MERL_MOD["shooting_latitude"].min() - pad, MEDITS_MERL_MOD["shooting_latitude"].max() + pad
t_start, t_end   = MEDITS_MERL_MOD["haul_date"].min(), MEDITS_MERL_MOD["haul_date"].max()

ds = cm.open_dataset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables=["bottomT"],                     
    minimum_longitude=float(lon_min),
    maximum_longitude=float(lon_max),
    minimum_latitude=float(lat_min),
    maximum_latitude=float(lat_max),
    start_datetime=pd.Timestamp(t_start),
    end_datetime=pd.Timestamp(t_end),
)




#Build vectorized indexers
t_da  = xr.DataArray(MEDITS_MERL_MOD["haul_date"].values, dims="points", name="time")
lon_da = xr.DataArray(MEDITS_MERL_MOD["shooting_longitude"].values,   dims="points", name="longitude")
lat_da = xr.DataArray(MEDITS_MERL_MOD["shooting_latitude"].values,   dims="points", name="latitude")

#Vectorized nearest selection
bt_vec = ds["bottomT"].sel(time=t_da, longitude=lon_da, latitude=lat_da, method="nearest")

#Compute and assign
MEDITS_MERL_MOD["bottom_temperature"] = bt_vec.compute().values

nan_count = MEDITS_MERL_MOD["bottom_temperature"].isna().sum()
nan_count
#262 NAn

#Nearest neighbour imputation for bottom temperature for NaN values
bottom_temperature = MEDITS_MERL_MOD["bottom_temperature"].to_numpy(dtype=float, copy=True)
coords = MEDITS_MERL_MOD[["shooting_longitude", "shooting_latitude"]].values

bottom_temperature = impute_nearest(bottom_temperature, coords)

#Assign back to dataframe
MEDITS_MERL_MOD["bottom_temperature"] = bottom_temperature

#Count NaNs after imputation
nan_count = MEDITS_MERL_MOD["bottom_temperature"].isna().sum()
nan_count
#0 Nan

#Preview and/or save
print(MEDITS_MERL_MOD[["shooting_latitude","shooting_longitude","haul_date","bottom_temperature"]])



#Surface temperature from Copernicus
MEDITS_MERL_MOD["surface_temperature"] = np.nan

def extract_sst_for_mask(row_mask, dataset_id):
    if not row_mask.any():
        return

    idx = MEDITS_MERL_MOD.index[row_mask]

    t_da_m = xr.DataArray(MEDITS_MERL_MOD.loc[idx, "haul_date"].values, dims="points", name="time")
    lon_da_m = xr.DataArray(MEDITS_MERL_MOD.loc[idx, "shooting_longitude"].values, dims="points", name="longitude")
    lat_da_m = xr.DataArray(MEDITS_MERL_MOD.loc[idx, "shooting_latitude"].values, dims="points", name="latitude")

    ds_sst = cm.open_dataset(
        dataset_id=dataset_id,
        variables=[SST_VAR],
        minimum_longitude=float(lon_min),
        maximum_longitude=float(lon_max),
        minimum_latitude=float(lat_min),
        maximum_latitude=float(lat_max),
        start_datetime=pd.Timestamp(MEDITS_MERL_MOD.loc[idx, "haul_date"].min()),
        end_datetime=pd.Timestamp(MEDITS_MERL_MOD.loc[idx, "haul_date"].max()),
    )

    sst_vec = ds_sst[SST_VAR].sel(
        time=t_da_m,
        longitude=lon_da_m,
        latitude=lat_da_m,
        method="nearest"
    )
    MEDITS_MERL_MOD.loc[idx, "surface_temperature"] = np.asarray(sst_vec.compute().values, dtype=float)

# Historical rows (1982-01-01 to 2024-01-16): use reprocessed SST
hist_mask = (MEDITS_MERL_MOD["haul_date"] >= SST_HIST_START) & (MEDITS_MERL_MOD["haul_date"] < SST_NRT_START)
extract_sst_for_mask(hist_mask, SST_DATASET_ID_HIST)

# Recent rows (>= 2024-01-17): use NRT SST
nrt_mask = MEDITS_MERL_MOD["haul_date"] >= SST_NRT_START
extract_sst_for_mask(nrt_mask, SST_DATASET_ID_NRT)

#Diagnostics
nan_count = MEDITS_MERL_MOD["surface_temperature"].isna().sum()
nan_count
#Only 28 NAN

# Nearest neighbour extraction for surface temperature for NaN values
surface_temperature = MEDITS_MERL_MOD["surface_temperature"].to_numpy(dtype=float, copy=True)
coords = MEDITS_MERL_MOD[["shooting_longitude", "shooting_latitude"]].values

surface_temperature = impute_nearest(surface_temperature, coords)

#Assign back to dataframe
MEDITS_MERL_MOD["surface_temperature"] = surface_temperature

#Count NaNs after imputation
nan_count = MEDITS_MERL_MOD["surface_temperature"].isna().sum()
nan_count
#0 Nan

#Let's convert surface temperature from Kelvin to Celsius
MEDITS_MERL_MOD["surface_temperature"] = (MEDITS_MERL_MOD["surface_temperature"] - 273.15)


#Create dataframe with only two added environmental columns:
#bottom_temperature and surface_temperature
print(MEDITS_MERL_MOD)
