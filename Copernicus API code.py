
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import xarray as xr
import dask
dask.config.set(scheduler="threads")

import copernicusmarine as cm


#store MEDITS MERL MOD dataset
URL = "https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/MEDITS_MERL_MOD_no_temp.csv"
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
    variables=["bottomT"],                     # <-- crucial: request bottomT
    minimum_longitude=float(lon_min),
    maximum_longitude=float(lon_max),
    minimum_latitude=float(lat_min),
    maximum_latitude=float(lat_max),
    start_datetime=pd.Timestamp(t_start),
    end_datetime=pd.Timestamp(t_end),
)


# Build vectorized indexers
t_da  = xr.DataArray(MEDITS_MERL_MOD["haul_date"].values, dims="points", name="time")
lon_da = xr.DataArray(MEDITS_MERL_MOD["shooting_longitude"].values,   dims="points", name="longitude")
lat_da = xr.DataArray(MEDITS_MERL_MOD["shooting_latitude"].values,   dims="points", name="latitude")

# Vectorized nearest selection (no Python loop over days)
bt_vec = ds["bottomT"].sel(time=t_da, longitude=lon_da, latitude=lat_da, method="nearest")

# Compute and assign
MEDITS_MERL_MOD["bottom_temperature"] = bt_vec.compute().values

nan_count = MEDITS_MERL_MOD["bottom_temperature"].isna().sum()
total_count = len(MEDITS_MERL_MOD)
nan_proportion = nan_count / total_count
print(f"NaN proportion: {nan_proportion:.4%} ({nan_count} out of {total_count})")

# Preview and/or save
print(MEDITS_MERL_MOD[["shooting_latitude","shooting_longitude","haul_date","bottom_temperature"]])



# Surface temperature from Copernicus
ds_sst = cm.open_dataset(
    dataset_id="METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2",
    variables=["analysed_sst"],
    minimum_longitude=float(lon_min),
    maximum_longitude=float(lon_max),
    minimum_latitude=float(lat_min),
    maximum_latitude=float(lat_max),
    start_datetime=pd.Timestamp(t_start),
    end_datetime=pd.Timestamp(t_end),
)


# Vectorized indexers (same as before)
t_da  = xr.DataArray(MEDITS_MERL_MOD["haul_date"].values, dims="points", name="time")
lon_da = xr.DataArray(MEDITS_MERL_MOD["shooting_longitude"].values, dims="points", name="longitude")
lat_da = xr.DataArray(MEDITS_MERL_MOD["shooting_latitude"].values, dims="points", name="latitude")

# Nearest neighbour extraction
sst_vec = ds_sst["analysed_sst"].sel(
    time=t_da,
    longitude=lon_da,
    latitude=lat_da,
    method="nearest"
)

# Assign to dataframe
MEDITS_MERL_MOD["surface_temperature"] = sst_vec.compute().values

# Diagnostics
nan_count = MEDITS_MERL_MOD["surface_temperature"].isna().sum()
total_count = len(MEDITS_MERL_MOD)
print(f"SST NaN proportion: {nan_count/total_count:.4%} ({nan_count} / {total_count})")
#Only 28 NAN

#Let's convert surface temperature from Kelvin to Celsius
MEDITS_MERL_MOD["surface_temperature"] = (MEDITS_MERL_MOD["surface_temperature"] - 273.15)

#Surface salinity from Copernicus
ds_sss = cm.open_dataset(
    dataset_id="cmems_obs-mob_glo_phy-sss_my_multi_P1D",
    dataset_version="202311",
    variables=["sos"],
    minimum_longitude=float(lon_min),
    maximum_longitude=float(lon_max),
    minimum_latitude=float(lat_min),
    maximum_latitude=float(lat_max),
    start_datetime=pd.Timestamp(t_start),
    end_datetime=pd.Timestamp(t_end),
)

sss_vec = (
    ds_sss["sos"]
    .sel(time=t_da, longitude=lon_da, latitude=lat_da, method="nearest"))

MEDITS_MERL_MOD["surface_salinity"] = sss_vec.compute().values
nan_count = MEDITS_MERL_MOD["surface_salinity"].isna().sum()

#Create csv file that is MEDITS_MERL_MOD with bottom temperature, surface temperature and surface salinity
print(MEDITS_MERL_MOD)
MEDITS_MERL_MOD.to_csv("MEDITS_MERL_MOD_TEMP.csv", index=False)


