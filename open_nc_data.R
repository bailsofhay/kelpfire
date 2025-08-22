library(terra)
library(rhdf5)
library(sp)
library(raster)
library(lubridate)

nc_file = "C://Users/bdmor/Box/KelpFire/data/LTER_kelpBiomass/CAkelpCanopyEnv_2021_final.nc"
nc = h5ls(nc_file) # not actually a netcdf file. It's an HF5 shaped like a netcdf.
vars = nc$name # vars in .nc file
h5closeAll()

# Area data
# environmental data is 332640x152
area = h5read(nc_file, "area")
biomass = h5read(nc_file, "biomass")
depth = h5read(nc_file, "depth")
hsmax = h5read(nc_file, "hsmax")
nitrate = h5read(nc_file, "nitrate")
npp = h5read(nc_file, "npp")
parbttm_max = h5read(nc_file, "parbttm_max")
parbttm_mean = h5read(nc_file, "parbttm_mean")
temp = h5read(nc_file, "temperature")
# time data is 1x152 --> # 152 is unique dates (quarter+year)
quarter = h5read(nc_file, "quarter")
year = h5read(nc_file, "year")
# Spatial data is 1x332640 --> pixels (no time)
# station = h5read(nc_file, "station")
x = h5read(nc_file, "utm_x")
y = h5read(nc_file, "utm_y")
utm_zone = h5read(nc_file, "utm_zone")
h5closeAll()

date_dat = data.frame(year = year, quarter = quarter)

base_dat = data.frame(utm_zone = utm_zone, x = x, y = y)

date = paste(date_dat$year, date_dat$quarter, sep = "_")

############ bring in hydrological point sources ############
#############################################################
pt_source = vect("C://Users/bdmor/Box/KelpFire/data/NHD_products/NHD_flowline_StreamRiver_Coast_NObarrier_POINT.shp")
pt_source = terra::project(pt_source, "+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +type=crs")

kelp_pts_10 = base_dat[base_dat$utm_zone == 10,]
kelp_pts_11 = base_dat[base_dat$utm_zone == 11,]
coordinates(kelp_pts_10) = ~x+y
projection(kelp_pts_10) = "EPSG:26910" # .nc points in utm 10n

coordinates(kelp_pts_11) = ~x+y
projection(kelp_pts_11) = "EPSG:26911" # .nc points in utm 11n

# reproject all pts to CA aea since we're dealing with the whole coastline
kelp_10_2_aea = spTransform(kelp_pts_10, "+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +type=crs")
kelp_11_2_aea = spTransform(kelp_pts_11, "+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +type=crs")
detach("package:raster", unload = T) # detach raster package so there is no mix up with terra

kelp_pts = rbind(kelp_10_2_aea, kelp_11_2_aea)
kelp_pts = vect(kelp_pts)

# calculate distance to nearest pt source and identify
near = terra::nearest(kelp_pts, pt_source)
kelp_pts$Dist = near$distance
kelp_pts$from_id = near$from_id
kelp_pts$to_id = near$to_id
writeVector(kelp_pts, "C://Users/bdmor/Box/KelpFire/data/NHD_products/kelpDist2PtSource", filetype = "ESRI Shapefile")

############### Identify fire dates ###############
###################################################
fires = vect("C://Users/bdmor/Box/KelpFire/data/fire_perimeters/fire_perimeters_1984-2021.shp")
fires$quarter = NA
fires$ALARM_DATE = lubridate::as_date(fires$ALARM_DATE, 
                                      tz = "UTC")
fires$quarter = lubridate::quarter(fires$ALARM_DATE, type = "quarter")
fires$date = paste(fires$YEAR_, fires$quarter, sep = "_")
fire_date = fires$date


############ Combine data by year ############
##############################################
xy = crds(kelp_pts, df = T)
xy$crs = "EPSG:3310"

for (i in 1:length(date)){
  working = print(paste0("Working on: ", date[i]))
  if (date[i] %in% fire_date){
    fire = fires[fires$date == date[i],]
    near_fire = nearest(kelp_pts, fire)
    dat = data.frame(ID = 1:nrow(xy), xy, date = date[i], area = area[,i],
                     biomass = biomass[,i], depth = depth,
                     hsmax = hsmax[,i], nitrate = nitrate[,i],
                     npp = npp[,i], parbttm_max = parbttm_max[,i],
                     parbttm_mean = parbttm_mean[,i],
                     temp = temp[,i], dist = kelp_pts$Dist,
                     dist_toID = kelp_pts$to_id, fireDist = near_fire$distance,
                     fire_toID = near_fire$to_id,
                     fire_acres = fire$GIS_ACRES[near_fire$to_id],
                     fire_length = fire$Shape_Leng[near_fire$to_id],
                     fire_area = fire$Shape_Area[near_fire$to_id])
  } else {
    dat = data.frame(ID = 1:nrow(xy), xy, date = date[i], area = area[,i],
                     biomass = biomass[,i], depth = depth,
                     hsmax = hsmax[,i], nitrate = nitrate[,i],
                     npp = npp[,i], parbttm_max = parbttm_max[,i],
                     parbttm_mean = parbttm_mean[,i],
                     temp = temp[,i], dist = kelp_pts$Dist,
                     dist_toID = kelp_pts$to_id, 
                     fireDist = NA,
                     fire_toID = NA,
                     fire_acres = NA,
                     fire_length = NA,
                     fire_area = NA)
  }
  
  dat$area[dat$area == -1] = NA
  # dat[dat[,] == -1] = NA
  d = dat[complete.cases(dat), ]
  write.csv(d, 
            file = paste0("C://Users/bdmor/Box/KelpFire/data/LTER_kelpBiomass/dataframe_by_year/LTER_kelp_ptSource_fire_", date[i], ".csv"),
            row.names = FALSE)
}
