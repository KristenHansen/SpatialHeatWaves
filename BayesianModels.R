#Bayesian Hierarchical Model Extension


#matchedDataset is the resulting dataset after running the matching procedure
matchedData <- read.csv("matchedDataset.csv", header = T) # Take matched dataset

#Get aggregated hospitalization counts
d <- aggregate(matchedData$diffaggmed, by = list(zipcode = matchedData$patzip), FUN = mean)
names(d) <- c("id", "Y")

population <- as.data.frame(unique(cbind(matchedData$Total.population, matchedData$patzip)))
names(population) <- c("population", "patzip")
cases <- d$Y
d <- left_join(d, population, by =c("id" = "patzip"))

d$ZCTA5CE10 <- d$id


#spBayes

library("spBayes")
library(MBA)
library(geoR)
library(fields)
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(lattice)
library(gstat) 
coords <- read.csv("~/Downloads/zip_gps.csv", header = T)
coords <- coords[which(coords$ZCTA5 >= 90001),]

coords$ZCTA5CE10 <- coords$ZCTA5
names(d)
bayesDF<- merge(d, coords, by = c("ZCTA5CE10"))

#Assumes Isotropy 


n.samples = 10000
bef.sp<-spLM(Y ~ 1, data = bayesDF, coords = as.matrix(bayesDF[,c("FinalLat", "FinalLon")]), starting = list("phi" = 1, "sigma.sq" = 0.05, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.02, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 1.5), "sigma.sq.IG" = c(2, 0.05)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)



#Interpolated Surface

y.residuals = residuals(lm(Y ~ 1, data = d))
par(mfrow = c(1,2))
surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
z.lim = range(surf[[3]], na.rm = T)
image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")
par(mfrow = c(1,1))
surf <- mba.surf(cbind(bayesDF[,c(7:8)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")

library(tigris)
library("rgeos")
caloutline <- states() %>% filter_state("California")
#caloutline <- st_as_sf(caloutline)
plot(caloutline, add = T)



#Relative Scale


TheVariogram=variogram(Y ~ 1, data=bayesDF)
plot(TheVariogram)
TheVariogramModel <- vgm(psill=0.05, model="Sph", nugget=0.0, range=1)
plot(TheVariogram, model=TheVariogramModel) 
#Assumes Isotropy Clearly that doesn't follow here
#Tau..sq = 0.0
#sg.sq = 0.05
#range = 1
n.samples = 10000
bayesDF=bayesDF[!is.na(bayesDF$population), ]
bef.sp<-spLM(Y ~ population, data = bayesDF, coords = as.matrix(bayesDF[,c("FinalLat", "FinalLon")]), starting = list("phi" = 1, "sigma.sq" = 0.05, "tau.sq" = 0), tuning = list("phi" = .10, "sigma.sq" = 0.02, "tau.sq" = 0), priors = list("phi.Unif" = c(0.001, 1.5), "sigma.sq.IG" = c(2, 0.05)), cov.model = "spherical", n.samples = n.samples)
round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3)
burn.in <- floor(0.75*n.samples)
bef.sp <- spRecover(bef.sp, start = burn.in)

beta.samples <- bef.sp$p.beta.recover.samples
w.samples = bef.sp$p.w.recover.samples

w.hat.mu = apply(w.samples, 1, mean)
w.hat.sd = apply(w.samples, 1, sd)


#Interpolated Surface

y.residuals = residuals(lm(Y ~ population, data = d))
par(mfrow = c(1,2))
surf <- mba.surf(cbind(bayesDF[,c(7:8)], y.residuals), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
z.lim = range(surf[[3]], na.rm = T)
image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "LM.residuals")

par(mfrow = c(1,1))
surf <- mba.surf(cbind(bayesDF[,c(7:8)], w.hat.mu), no.X = 300, no.Y = 300, extend = FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, main = "Mean spatial effects")

library(tigris)
library("rgeos")
caloutline <- states() %>% filter_state("California")

plot(caloutline, add = T)









