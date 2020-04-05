#####################3###########
#Data Generating Process
# Author: Kristen Hansen
#############################


nzip <- 100
#Time period in years
timePeriod <- 1

#May through September has 153 days

nrows <- nzip*timePeriod*153
hw_thres <- 0.95
all_zips = list(0)
for(zip in 1:nzip){
  fulldays <- timePeriod*153
  nmat <- as.data.frame(matrix(nrow = fulldays))

  nmat$zip = rep(zip,fulldays)
  
  nmat$maxtemp = rnorm(fulldays, mean = 30*(1 + zip/200), sd = 8)
  
  nmat$population = rep(50000*(1 - (zip/100 - zip/200)),fulldays)
  
  #nmat$agg_hosp = rpois(fulldays, nmat$population[1]/4000)
  nmat$agg_hosp =ifelse(rbinom(fulldays, size = 1, prob = 0.40) > 0, 0, rpois(fulldays, lambda = nmat$population[1]/7000))
  nmat$V1 = NULL
  
  all_zips[[zip]] = nmat
}

fulldata = do.call('rbind', all_zips)
fulldata$zip = as.factor(as.character(fulldata$zip))


