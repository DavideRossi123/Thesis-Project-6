
import warnings
warnings.filterwarnings("ignore")
import numpy as np
import pandas as pd
import xarray as xr
import dask
dask.config.set(scheduler="threads")
import copernicusmarine as cm
from scipy.spatial import cKDTree


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
bottom_temperature = MEDITS_MERL_MOD["bottom_temperature"].values
coords = MEDITS_MERL_MOD[["shooting_longitude", "shooting_latitude"]].values

nan_idx = np.isnan(bottom_temperature)
valid_idx = ~nan_idx

#Build KDTree and query nearest neighbours
tree = cKDTree(coords[valid_idx])
distances, nearest_idx = tree.query(coords[nan_idx], k=1)

#Fill NaN values
bottom_temperature[nan_idx] = bottom_temperature[valid_idx][nearest_idx]

#Assign back to dataframe
MEDITS_MERL_MOD["bottom_temperature"] = bottom_temperature

#Count NaNs after imputation
nan_count = MEDITS_MERL_MOD["bottom_temperature"].isna().sum()
nan_count
#0 Nan

#Preview and/or save
print(MEDITS_MERL_MOD[["shooting_latitude","shooting_longitude","haul_date","bottom_temperature"]])



#Surface temperature from Copernicus
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


#Vectorized indexers (same as before)
t_da  = xr.DataArray(MEDITS_MERL_MOD["haul_date"].values, dims="points", name="time")
lon_da = xr.DataArray(MEDITS_MERL_MOD["shooting_longitude"].values, dims="points", name="longitude")
lat_da = xr.DataArray(MEDITS_MERL_MOD["shooting_latitude"].values, dims="points", name="latitude")

sst_vec = ds_sst["analysed_sst"].sel(
    time=t_da,
    longitude=lon_da,
    latitude=lat_da,
    method="nearest"
)

#Assign to dataframe
MEDITS_MERL_MOD["surface_temperature"] = sst_vec.compute().values

#Diagnostics
nan_count = MEDITS_MERL_MOD["surface_temperature"].isna().sum()
nan_count
#Only 28 NAN

# Nearest neighbour extraction for surface temperature for NaN values
surface_temperature = MEDITS_MERL_MOD["surface_temperature"].values
coords = MEDITS_MERL_MOD[["shooting_longitude", "shooting_latitude"]].values

nan_idx = np.isnan(surface_temperature)
valid_idx = ~nan_idx

#Build KDTree and query nearest neighbours
tree = cKDTree(coords[valid_idx])
distances, nearest_idx = tree.query(coords[nan_idx], k=1)

#Fill NaN values
surface_temperature[nan_idx] = surface_temperature[valid_idx][nearest_idx]

#Assign back to dataframe
MEDITS_MERL_MOD["surface_temperature"] = surface_temperature

#Count NaNs after imputation
nan_count = MEDITS_MERL_MOD["surface_temperature"].isna().sum()
nan_count
#0 Nan

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
nan_count

#Nearest neighbour extraction for surface salinity for NaN values
salinity = MEDITS_MERL_MOD["surface_salinity"].values
coords = MEDITS_MERL_MOD[["shooting_longitude", "shooting_latitude"]].values

nan_idx = np.isnan(salinity)
valid_idx = ~nan_idx

#Build KDTree and query nearest neighbours
tree = cKDTree(coords[valid_idx])
distances, nearest_idx = tree.query(coords[nan_idx], k=1)

#Fill NaN values
salinity[nan_idx] = salinity[valid_idx][nearest_idx]

#Assign back to dataframe
MEDITS_MERL_MOD["surface_salinity"] = salinity

#Count NaNs after imputation
nan_count = MEDITS_MERL_MOD["surface_salinity"].isna().sum()
nan_count

#Create csv file that is MEDITS_MERL_MOD with bottom temperature, surface temperature and surface salinity
print(MEDITS_MERL_MOD)

