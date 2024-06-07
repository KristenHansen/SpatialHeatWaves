setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documents - Kristen’s MacBook Pro/EnviroResearchArmin")
dataPrep <- function(){
  allResp <- read.csv("Whole\ CA\ all\ ICD.csv", header = T)
  allResp$date3 <- as.Date(allResp$date, format = "%d%b%Y")
  allResp$date <- allResp$date3
  
  correctAll <- allResp[which(allResp$date >= "2004-01-01"),]
  
  correctAll$aggcode <- correctAll$cardio_disease + correctAll$dehydration + correctAll$acute_renal_failure + correctAll$heat_illness + correctAll$resp_disease
  correctAll[1:10,]
  
  respir <- read.csv("Whole\ CA\ Respi.csv", header = TRUE)
  ziptemp <- read.csv("ca_zip_temp_04t13.csv", header = TRUE)
  cz <- read.csv("ClimateZones.csv", header = TRUE)
  
  respir$date3 <- as.Date(respir$date, format = "%d%b%Y")
  respir$date <- respir$date3
  ziptemp$date3 <- as.Date(ziptemp$date, format = "%Y-%m-%d")
  ziptemp$date <- ziptemp$date3
  popZCTA <- read.csv("2010Census_DemoProfile.csv", header = TRUE)
  
  
  
  respir$zipnum <- respir$patzip
  respir2 <- respir[which(respir$date >= as.Date("2004-01-01")),]
  library("dplyr")
  newtest <- inner_join(respir2, ziptemp, by = c("date", "zipnum"))
  rows_sampled <- sample(1:nrow(newtest), 5000)
  
  
  
  #library(zipcode)
  library(tidyr)
  library(ggplot2)
  
  #data(zipcode)
  #zipcode$zip = readr::parse_number(zipcode$zip)
  
  #testset <- left_join(newtest, zipcode, by = c("patzip" = "zip"))
  secondTest <- left_join(newtest, popZCTA, by = c("patzip" = "zipnum"))
  testset <- left_join(secondTest, cz, by = c("patzip" = "Zip.Code"))
  
  tojoin <- testset[,c("patzip", "date", "mintemp", "maxtemp", "Total.population")]
  correctAll = left_join(tojoin, correctAll, by = c("patzip", "date"))
  
  
  correctAll$weekday <- weekdays(correctAll$date)
  
  correctAll$month <- format(correctAll$date,"%B")
  
  
  #Subsetting 
  
  newtest <- subset(correctAll, correctAll$month == "May" | correctAll$month == "June" | correctAll$month == "July" | correctAll$month == "August" | correctAll$month == "September")
  newtest$month <- as.factor(newtest$month)
  newtest$weekday <- as.factor(newtest$weekday)
  newtest <- within(newtest, month <- relevel(month, ref = "May"))
  newtest <- within(newtest, weekday <- relevel(weekday, ref = "Monday"))
  library(dplyr)
  
  source("hwdef.R")
  newtest = hw_definitions(newtest)
  #Get data above 75 percentile
  # pct75_maxtemp = newtest %>% 
  #   dplyr::group_by(patzip) %>% 
  #   dplyr::summarize(pct75_maxtemp = quantile(maxtemp, probs = 0.75))
  # 
  # newtest <- left_join(newtest, pct75_maxtemp, by = "patzip")
  # 
  # 
  
  
  #Get dataset for match finding
  #dataMatch <- newtest[which(newtest$maxtemp >= newtest$pct75_maxtemp),]
  
  dataMatch = newtest
  #Xformatch <- dataMatch[,c("patzip", "month", "weekend","hw95", "hw975", "hw99", "aggcode", "latitude.x", "longitude.x", "Total.population")]
  dataMatch$Total.population = as.numeric(gsub(",", "", dataMatch$Total.population))
  
  return(dataMatch)
}
dataMatch = dataPrep()
dataMatch$year = substring(dataMatch$date,1,4)
dataMatch = dataMatch[which(dataMatch$patzip < 92200 & dataMatch$patzip > 91900),]
dates_list = split(dataMatch, dataMatch$year)
# XHeat <- Xformatch[which(Xformatch$heatwave == 1),]
# 
# XNon <- Xformatch[which(Xformatch$heatwave == 0),]

KernelDistanceMatching <- function(case, control_dataset){
  weightedAvgDiff = numeric(0)
  print(nrow(case))
  for(i in 1:nrow(case)){
    if(i %% 1000 == 0) print(i)
    controls <- control_dataset[which(control_dataset$patzip == case$patzip[i] & control_dataset$year == case$year[i]),]
    controls$distance = as.numeric(abs(case$date[i] - controls$date), units = "days")
    controls$weight = 1/controls$distance
    weightedAvgDiff[i] = case$aggcode[i] - sum(controls$aggcode*controls$weight)/sum(controls$weight)
  }
  return(weightedAvgDiff)
}



MatchProcedure <- function(dataMatch, hw){
  #dataMatch <- x
  dataMatch$month <- as.numeric(dataMatch$month)
  dataMatch$year <- format(as.Date(dataMatch$date, format="%Y-%m-%d"),"%Y")
  x <- dataMatch

  #Get Contrast for Joint Days
  XJoint <- dataMatch[which(hw == 1),]
  print(nrow(XJoint))
  
  zipcodesJoint <- unique(XJoint$patzip)
  XNon <- dataMatch[which(hw == 0),]
  
  
  zipcodesTotal <- unique(XNon$patzip)
  
  
  
  XJoint$diffMean <- KernelDistanceMatching(XJoint, XNon)
  
  #Get contrast for heat wave only days
  
  return(XJoint)
}
matchData_maxhw95 = MatchProcedure(dataMatch, dataMatch$maxhw95)
#Perform Matching procedure
# j = 1
# matchedData <- data.frame(matrix(ncol=8 , nrow= 407727))
# names(matchedData) = c("patzip", "month", "weekend","heatwave", "aggcode", "latitude.x", "longitude.x", "Total.population")
# for(i in 1:nrow(XHeat)){
#   if (i %% 1000 == 0) print(i)
#   potentialMatch <- XNon[which(XNon$patzip == XHeat$patzip[i]),]
#   indices <- ifelse(potentialMatch$month == XHeat$month[i] & potentialMatch$weekend == XHeat$weekend[i], 1,0)
#   xMatched <- potentialMatch[which(indices == 1),]
#   xMatched <- xMatched[sample(nrow(xMatched), 3, replace = TRUE),]
#   matchedData[j:(j+2),] <- xMatched
#   j = j + 3
# }
# 
# #Get median hospitalizations for matched days
# XHeat$nonheatmatch = NA
# XHeat$nonheatmedian = NA
# for(i in 1:nrow(XHeat)){
#   j = i*3
#   temp = mean(matchedData[j-2:j,5])
#   XHeat$nonheatmatch[i] = temp
#   temp = median(matchedData[j-2:j,5])
#   XHeat$nonheatmedian[i] = temp
#   if(i %% 1000 == 0) print(i)
# }
# 
# matchedDataset = XHeat
# matchedDataset$heat_count <- as.numeric(as.character(matchedDataset$aggcode))
# matchedDataset$nonheatmatch <- as.numeric(as.character(matchedDataset$nonheatmatch))
# 
# matchedDataset$difference <- (matchedDataset$heat_count - matchedDataset$nonheatmatch)
# matchedDataset$diffMed <- (matchedDataset$heat_count - matchedDataset$nonheatmedian)
# matchedDataset$diffRatio <- matchedDataset$diffMed/matchedDataset$Total.population
geoNeed <- unique(matchData[,c("latitude", "longitude", "patzip", "Total.population")])

#Make sure the column number is correct for the outcome of interest
col_num = 12 
diffByZipHeatWave<- aggregate(matchData[[1]][,8], list(matchData[[1]]$patzip), mean)
#diffByZipHeatWave[,2] <- ifelse(diffByZipHeatWave[,2] < 0, 0 , diffByZipHeatWave[,2])
diffByZipHeatWave$ZCTA5CE10 = diffByZipHeatWave[,1]
diffByZipHeatWave$latitude = geoNeed$latitude
diffByZipHeatWave$longitude = geoNeed$longitude
diffByZipHeatWave$population = geoNeed$Total.population
summary(diffByZipHeatWave[,2])

path = "~/Downloads/ShapefileZCTA/cb_2016_us_zcta510_500k.shp"
#Path is the path to the shapefile for ZCTAs in california (shp)
library(sf)
shape <- st_read(dsn = path)
m <- merge(shape, diffByZipHeatWave, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$x), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(1.5, "cm")) + labs(title = "Difference in Hospitalizations between\n 99% Heat Day\n and Matched Days", fill = "Difference\n")


#Kulldorff Methodology
library(SpatialEpi)

shapePoly = as(shape, 'Spatial')
diffByZipHeatWave = na.omit(diffByZipHeatWave)
geo = latlong2grid(diffByZipHeatWave[,4:5])
kull <- kulldorff(geo = geo, cases = diffByZipHeatWave[,2], 
                  population = diffByZipHeatWave[,6], expected.cases = rep( 0.45, nrow(diffByZipHeatWave)), 
                  pop.upper.bound = 0.2, n.simulations = 100, alpha.level = 0.05, plot = T)

diffByZipHeatWave$cluster = NA
for(i in 1:nrow(diffByZipHeatWave)){
  if(i %in% kull$most.likely.cluster$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "One"
  } else if(i %in% kull$secondary.clusters[[1]]$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "Two"
  } else if(i %in% kull$secondary.clusters[[2]]$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "Three"
  } else if (i %in% kull$secondary.clusters[[3]]$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "Four"
  }else if (i %in% kull$secondary.clusters[[4]]$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "Five"
  }else if (i %in% kull$secondary.clusters[[5]]$location.IDs.included){
    diffByZipHeatWave$cluster[i] = "Six"
  } else {
    diffByZipHeatWave$cluster[i] = NA
  }
}

m <- merge(shape, diffByZipHeatWave, by = "ZCTA5CE10")
p <- ggplot(data = m) + geom_sf(aes(fill = m$cluster), colour = NA) #+ scale_fill_manual(values = c("Blue", "Red", "Green", "Black", "Yellow", "Violet"))
p

p <- ggplot(data = m) + geom_sf(aes(fill = m$population), colour = NA)
p + scale_fill_gradient2(low = "blue", mid = "black", high="red") + theme(legend.justification = "top", legend.key.size = unit(2, "cm"))+ labs(fill = "population") + ggtitle("Population by Zipcode")
                                                                                         
