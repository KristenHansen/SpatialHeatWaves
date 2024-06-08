#install.packages("ncdf")
library(ncdf4)
library(ncdf)

ncpath <- "~/Downloads/"
ncname <- "agg_met_rmax_1979_CurrentYear_CONUS"
ncfname <- paste0(ncpath,ncname, ".nc")

ncin <- nc_open(ncfname)
print(ncin)

lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

day <- ncvar_get(ncin,"day")
day

tunits <- ncatt_get(ncin,"day","units")
tunits

library("CFtime")
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", day) # convert time to CFtime class
cf
timestamps <- as_timestamp(cf) # get character-string times
timestamps
time_cf <- CFparse(cf, timestamps) # parse the string into date components
time_cf

library(raster)
#test_rh <- rasterObjectFromCDF(ncin)
test_rh <- brick("~/Downloads/agg_met_rmax_1979_CurrentYear_CONUS.nc", dims = c(3,2,1))
#Aggregate into zctas by day
zips <- read.csv("~/Downloads/zip_gps.csv")
coordinates(zips)= ~ FinalLon+ FinalLat
library(sf)
zip_shape <- read_sf("~/Downloads/ShapeFileZCTA/cb_2016_us_zcta510_500k.shp")
r <- stack("~/Downloads/agg_met_rmax_1979_CurrentYear_CONUS.nc",varname = "daily_maximum_relative_humidity")

rasValue=extract(r, zips)

combinePointValue=cbind(zips,rasValue)
write.table(combinePointValue,file="combinedPointValue.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

#COnvert the column names to the dates so we can convert to long data frame
library(tidyverse)
combinePointValue <- as.data.frame(combinePointValue)
cpv_long <- combinePointValue %>%
  pivot_longer(cols = X37985:X41637, 
               names_to = "day", 
               values_to = "rh")
write.csv(cpv_long, "rh_long.csv")

cpv_long$day <- as.numeric(stringr::str_remove(cpv_long$day, "X"))
cpv_long$date <- as.Date("1900-01-01") + cpv_long$day

write.csv(cpv_long,"rh_long_with_date.csv")
ziptemp <- read.csv("~/Documents/OzoneHeatJoint/ca_zip_temp_04t13.csv", header = TRUE)
ziptemp$ZCTA5 <- ziptemp$zipnum
ziptemp$date = as.Date(ziptemp$date)
library(dplyr)
test = left_join(ziptemp, cpv_long, by = c("ZCTA5", "date"))


test$heatIndex = heat_index(temperature = test$maxtemp,rh = test$rh, dewpoint = NA, dp = FALSE, t_f = FALSE, rh_original = FALSE)$hi
summary(test$heatIndex)

write.csv(test, "ca_zip_temp_hi.csv")

View(test)

