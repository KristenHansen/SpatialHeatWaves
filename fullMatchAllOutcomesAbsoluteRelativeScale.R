dataPrep <- function(){
  allResp <- read.csv("Whole\ CA\ all\ ICD.csv", header = T)
  allResp$date3 <- as.Date(allResp$date, format = "%d%b%Y")
  allResp$date <- allResp$date3
  
  correctAll <- allResp[which(allResp$date >= "2004-01-01"),]
  
  #correctAll$aggcode <- correctAll$cardio_disease + correctAll$dehydration + correctAll$acute_renal_failure + correctAll$heat_illness + correctAll$resp_disease
  correctAll[1:10,]
  
  respir <- read.csv("Whole\ CA\ Respi.csv", header = TRUE)
  ziptemp <- read.csv("ca_zip_temp_04t13.csv", header = TRUE)
  cz <- read.csv("ClimateZones.csv", header = TRUE)
  
  respir$date3 <- as.Date(respir$date, format = "%d%b%Y")
  respir$date <- respir$date3
  ziptemp$date3 <- as.Date(ziptemp$date, format = "%Y-%m-%d")
  ziptemp$date <- ziptemp$date3
  popZCTA <- read.csv("2010Census_DemoProfile.csv", header = TRUE)
  
  
  #ziptemp$zipnum
  respir$zipnum <- respir$patzip
  respir2 <- respir[which(respir$date >= as.Date("2004-01-01")),]
  library("dplyr")
  newtest <- inner_join(respir2, ziptemp, by = c("date", "zipnum"))
  rows_sampled <- sample(1:nrow(newtest), 5000)
  
  
  
  library(zipcode)
  library(tidyr)
  library(ggplot2)
  data(zipcode)
  
  zipcode$zip = readr::parse_number(zipcode$zip)
  
  testset <- left_join(newtest, zipcode, by = c("zipnum" = "zip"))
  secondTest <- left_join(testset, popZCTA, by = c("zipnum" = "zipnum"))
  testset <- left_join(secondTest, cz, by = c("zipnum" = "Zip.Code"))
  
  tojoin <- testset[,c("patzip", "date", "mintemp", "maxtemp", "latitude", "longitude", "Total.population")]
  correctAll = left_join(tojoin, correctAll, by = c("patzip", "date"))
  #correctAll <- if(!is.na(correctAll$maxtemp), correctAll, NA)
  #Subsetting
  #correctAll$date <- as.Date(correctAll$date, format = "%Y-%m-%d")
  correctAll$weekday <- weekdays(correctAll$date)
  
  correctAll$month <- format(correctAll$date,"%B")
  
  
  
  newtest <- subset(correctAll, correctAll$month == "May" | correctAll$month == "June" | correctAll$month == "July" | correctAll$month == "August" | correctAll$month == "September")
  newtest$month <- as.factor(newtest$month)
  newtest$weekday <- as.factor(newtest$weekday)
  newtest <- within(newtest, month <- relevel(month, ref = "May"))
  newtest <- within(newtest, weekday <- relevel(weekday, ref = "Monday"))
  library(dplyr)
  pct95_maxtemp = newtest %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct95_maxtemp = quantile(maxtemp, probs = 0.95))
  
  newtest <- left_join(newtest, pct95_maxtemp, by = ("patzip"))
  newtest$heatwave <- ifelse(newtest$maxtemp >= newtest$pct95_maxtemp, 1, 0)
  
  newtest$weekend <- ifelse(newtest$weekday == "Saturday" | newtest$weekday == "Sunday", 1, 0)
  
  #Get data above 75 percentile
  pct75_maxtemp = newtest %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct75_maxtemp = quantile(maxtemp, probs = 0.75))
  
  newtest <- left_join(newtest, pct75_maxtemp, by = "patzip")
  return(newtest)
}

newtest <- dataPrep()
matchProc <- function(){
  dataMatch <- newtest[which(newtest$maxtemp >= newtest$pct75_maxtemp),]
  
  
  Xformatch <- dataMatch[,c("patzip", "month", "weekend","heatwave", "cardio_disease", "resp_disease", "heat_illness", "dehydration", "acute_renal_failure", "latitude", "longitude", "Total.population")]
  Xformatch$Total.population = as.numeric(gsub(",", "", Xformatch$Total.population))
  XHeat <- Xformatch[which(Xformatch$heatwave == 1),]
  #XHeat <- XHeat[,c("patzip", "month", "weekday")]
  XNon <- Xformatch[which(Xformatch$heatwave == 0),]
  #XNon <- XNon[,c("patzip", "month", "weekday")]
  
  #m.out <- matchit(heatwave ~ patzip + month + weekday, data = Xformatch, method = "exact", ratio = 4)
  j = 1
  matchedData <- data.frame(matrix(ncol=12 , nrow= 407727))
  names(matchedData) = c("patzip", "month", "weekend","heatwave", "cardio_disease", "resp_disease", "heat_illness", "dehydration", "acute_renal_failure", "latitude", "longitude", "Total.population")
  for(i in 1:nrow(XHeat)){
    if (i %% 1000 == 0) print(i)
    potentialMatch <- XNon[which(XNon$patzip == XHeat$patzip[i]),]
    indices <- ifelse(potentialMatch$month == XHeat$month[i] & potentialMatch$weekend == XHeat$weekend[i], 1,0)
    xMatched <- potentialMatch[which(indices == 1),]
    xMatched <- xMatched[sample(nrow(xMatched), 3, replace = TRUE),]
    matchedData[j:(j+2),] <- xMatched
    j = j + 3
  }
  
  XHeat$nonheatcard = XHeat$nonheatresp = XHeat$nonheatrenal = XHeat$nonheathi = XHeat$nonheatdehyd = NA
  XHeat$nonheatcardmed = XHeat$nonheatrespmed = XHeat$nonheatrenalmed = XHeat$nonheathimed = XHeat$nonheatdehydmed = NA
  for(i in 1:nrow(XHeat)){
    j = i*3
    #
    cardtemp = mean(matchedData[(j-2):j,5])
    XHeat$nonheatcard[i] = cardtemp
    cardtemp = median(matchedData[(j-2):j,5])
    XHeat$nonheatcardmed[i] = cardtemp
    #
    resptemp = mean(matchedData[(j-2):j,6])
    XHeat$nonheatresp[i] = resptemp
    resptemp = median(matchedData[(j-2):j,6])
    XHeat$nonheatrespmed[i] = resptemp
    #
    hitemp = mean(matchedData[(j-2):j,7])
    XHeat$nonheathi[i] = hitemp
    hitemp = median(matchedData[(j-2):j,7])
    XHeat$nonheathimed[i] = hitemp
    #
    detemp = mean(matchedData[(j-2):j,8])
    XHeat$nonheatdehyd[i] = detemp
    detemp = median(matchedData[(j-2):j,8])
    XHeat$nonheatdehydmed[i] = detemp
    
    #
    renaltemp = mean(matchedData[(j-2):j,9])
    XHeat$nonheatrenal[i] = renaltemp
    renaltemp = median(matchedData[(j-2):j,9])
    XHeat$nonheatrenalmed[i] = renaltemp
    if(i %% 1000 == 0) print(i)
  }
  
  matchedDataset = XHeat
  #write.csv(file = "matchedDataset.csv", x = matchedDataset)
  matchedDataset$heat_count_card <- as.numeric(as.character(matchedDataset$cardio_disease))
  matchedDataset$heat_count_resp <- as.numeric(as.character(matchedDataset$resp_disease))
  matchedDataset$heat_count_renal <- as.numeric(as.character(matchedDataset$acute_renal_failure))
  matchedDataset$heat_count_hi <- as.numeric(as.character(matchedDataset$heat_illness))
  matchedDataset$heat_count_dehyd <- as.numeric(as.character(matchedDataset$dehydration))
  #matchedDataset$nonheatcard <- as.numeric(as.character(matchedDataset$nonheatcard))
  #matchedDataset$nonheatresp <- as.numeric(as.character(matchedDataset$nonheatresp))
  
  matchedDataset$differencecard <- (matchedDataset$heat_count_card - matchedDataset$nonheatcard)
  matchedDataset$diffMedcard <- (matchedDataset$heat_count_card - matchedDataset$nonheatcardmed)
  matchedDataset$differenceresp <- (matchedDataset$heat_count_resp - matchedDataset$nonheatresp)
  matchedDataset$diffMedresp <- (matchedDataset$heat_count_resp - matchedDataset$nonheatrespmed)
  matchedDataset$differencerenal <- (matchedDataset$heat_count_renal - matchedDataset$nonheatrenal)
  matchedDataset$diffMedrenal <- (matchedDataset$heat_count_renal - matchedDataset$nonheatrenalmed)
  matchedDataset$differencehi <- (matchedDataset$heat_count_hi - matchedDataset$nonheathi)
  matchedDataset$diffMedhi <- (matchedDataset$heat_count_hi - matchedDataset$nonheathimed)
  matchedDataset$differencedehyd <- (matchedDataset$heat_count_dehyd - matchedDataset$nonheatdehyd)
  matchedDataset$diffMeddehyd <- (matchedDataset$heat_count_dehyd - matchedDataset$nonheatdehydmed)
  matchedDataset$aggheat <- matchedDataset$heat_count_card + matchedDataset$heat_count_resp + matchedDataset$heat_count_renal + matchedDataset$heat_count_hi + matchedDataset$heat_count_dehyd
  matchedDataset$aggNonheat <- matchedDataset$nonheatcardmed + matchedDataset$nonheatrespmed +  matchedDataset$nonheatrenalmed + matchedDataset$nonheathimed + matchedDataset$nonheatdehydmed 
  matchedDataset$diffaggmed <- matchedDataset$aggheat - matchedDataset$aggNonheat
  write.csv(file = "matchedDataset.csv", x = matchedDataset)
  return(matchedDataset)
}
matchedDataset = matchProc()

geoNeed <- unique(matchedDataset[,c("latitude", "longitude", "patzip", "Total.population")])

#Double check columns numbers before each run
diffByZipHeatWave <- aggregate(matchedDataset[,37], list(XHeat$patzip), mean)
diffByZipHeatWave$dehydDiff <- diffByZipHeatWave$x
cardDiff <- aggregate(matchedDataset[,29], list(XHeat$patzip), mean)
diffByZipHeatWave$cardDiff <- cardDiff[,2]
respDiff <- aggregate(matchedDataset[,31], list(XHeat$patzip), mean)
diffByZipHeatWave$respDiff <- respDiff[,2]

renalDiff <- aggregate(matchedDataset[,33], list(XHeat$patzip), mean)
diffByZipHeatWave$renalDiff <- renalDiff[,2]

hiDiff <- aggregate(matchedDataset[,35], list(XHeat$patzip), mean)
diffByZipHeatWave$hiDiff <- hiDiff[,2]

aggDiff <- aggregate(matchedDataset[,40], list(XHeat$patzip), mean)
diffByZipHeatWave$aggDiff <- aggDiff[,2]



diffByZipHeatWave$ZCTA5CE10 = diffByZipHeatWave[,1]
diffByZipHeatWave$latitude = geoNeed$latitude
diffByZipHeatWave$longitude = geoNeed$longitude
diffByZipHeatWave$population = geoNeed$Total.population
diffByZipHeatWave <- na.omit(diffByZipHeatWave)

#Make plots and Absolute Scale is complete
#Make plots for each outcome by changing m$aggDiff in line 196
library(sf)
shape <- st_read(dsn = path)
m <- merge(shape, diffByZipHeatWave, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$aggDiff), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm"))


#Relative Scale Analysis
ggplot(data = cardDiff, aes(x = population, y = cardDiff[,2])) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")

ggplot(data = diffByZipHeatWave, aes(x = population, y = cardDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")
ggplot(data = diffByZipHeatWave, aes(x = population, y = respDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")
ggplot(data = diffByZipHeatWave, aes(x = population, y = renalDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")
ggplot(data = diffByZipHeatWave, aes(x = population, y = hiDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")
ggplot(data = diffByZipHeatWave, aes(x = population, y = dehydDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")
ggplot(data = diffByZipHeatWave, aes(x = population, y = aggDiff)) + geom_point() +  geom_smooth(method='lm', se = FALSE, color = "red")



modAgg <- lm(aggDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$Aggresiduals <- modAgg$residuals
plot(modAgg)
library(car)
outlierTest(modAgg)
influencePlot(modAgg, id.method="identify")
ncvTest(modAgg)

modCard <- lm(cardDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$cardresiduals <- modCard$residuals
plot(modCard)
outlierTest(modCard)
influencePlot(modCard, id.method="identify")
ncvTest(modCard)

modResp <- lm(respDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$respresiduals <- modResp$residuals
plot(modResp)
plot(modResp)
outlierTest(modResp)
influencePlot(modResp, id.method="identify")
ncvTest(modResp)

modrenal <- lm(renalDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$renalresiduals <- modrenal$residuals
plot(modrenal)

modDehyd <- lm(dehydDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$dehydresiduals <- modDehyd$residuals
plot(modDehyd)


modHi <- lm(hiDiff ~ population, diffByZipHeatWave)
diffByZipHeatWave$hiresiduals <- modHi$residuals
plot(modHi)


library(sf)
#ZCTA shapefile read in
path = "CaliforniaZCTA/cb_2016_us_zcta510_500k.shp"
shape <- st_read(dsn = path)
m <- merge(shape, diffByZipHeatWave, by = ("ZCTA5CE10"))
# p <- ggplot(data = m) + geom_sf(aes(fill = m$hiDiff), colour = NA)
# p + scale_fill_gradient2(low = "blue",mid = "white", high="red") +
#   theme(legend.justification = "top", legend.key.size = unit(2, "cm")) +
#   labs(fill = "Excess count") +
#   ggtitle("Heat Illness Count Difference")






library(sf)
shape <- st_read(dsn = "~/Downloads/ShapefileZCTA/cb_2016_us_zcta510_500k.shp")
pct95_maxtemp$ZCTA5CE10 <- pct95_maxtemp$patzip
m <- merge(shape, pct95_maxtemp, by = ("ZCTA5CE10"))
p <- ggplot(data = m)+ geom_sf(aes(fill = m$pct95_maxtemp), colour = NA) + geom_polygon(data = ca, aes(x=long, y=lat, group = group),fill = NA,colour ="black", size=0.25)
p + coord_sf(xlim = c(-117.6, -116.1),ylim = c(32.4, 33.5))+scale_fill_gradient2(low = "steelblue4",mid = "white", high="red", midpoint = 35, na.value = "grey30") +
  theme(legend.justification = "top", legend.key.size = unit(2, "cm")) +
  labs(fill = "95 quantile temp") +
  ggtitle("95% Quantile of max temperature for each zip code") 
  
ca <- counties("California", cb = T)
