
#Read in matched dataset
matched <- read.csv("matchedDataset.csv", header = T)
#CVD
B = 1000
k = 1
pivot.ci.cvd = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMedcard
  mean.x = mean(x)
  sd.x = sd(x)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot]) - mean.x)/sd(x[boot])
  }
  z <- z[is.finite(z)]
  pivot.ci.cvd[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.cvd$mean[k] = mean.x
  pivot.ci.cvd$sd[k] = sd.x
  k = k + 1
}
  
#Respiratory
B = 1000
k = 1
pivot.ci.resp = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMedresp
  mean.x = mean(x, na.rm = T)
  sd.x = sd(x, na.rm = T)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot], na.rm = T) - mean.x)/sd(x[boot], na.rm = T)
  }
  z <- z[is.finite(z)]
  pivot.ci.resp[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.resp$mean[k] = mean.x
  pivot.ci.resp$sd[k] = sd.x
  k = k + 1
}


#Renal
B = 1000
k = 1
pivot.ci.renal = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMedrenal
  mean.x = mean(x, na.rm = T)
  sd.x = sd(x, na.rm = T)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot], na.rm = T) - mean.x)/sd(x[boot], na.rm = T)
  }
  z <- z[is.finite(z)]
  pivot.ci.renal[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.renal$mean[k] = mean.x
  pivot.ci.renal$sd[k] = sd.x
  k = k + 1
}

#Dehyd
B = 1000
k = 1
pivot.ci.de = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMeddehyd
  mean.x = mean(x, na.rm = T)
  sd.x = sd(x, na.rm = T)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot], na.rm = T) - mean.x)/sd(x[boot], na.rm = T)
  }
  z <- z[is.finite(z)]
  pivot.ci.de[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.de$mean[k] = mean.x
  pivot.ci.de$sd[k] = sd.x
  k = k + 1
}

#Heat illness
B = 1000
k = 1
pivot.ci.hi = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMedhi
  mean.x = mean(x, na.rm = T)
  sd.x = sd(x, na.rm = T)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot], na.rm = T) - mean.x)/sd(x[boot], na.rm = T)
  }
  z <- z[is.finite(z)]
  pivot.ci.hi[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.hi$mean[k] = mean.x
  pivot.ci.hi$sd[k] = sd.x
  k = k + 1
}

#Aggregate
matched$diffMedagg <- matched$diffMedcard + matched$diffMedresp + matched$diffMedrenal + matched$diffMedhi + matched$diffMeddehyd

#
B = 1000
k = 1
pivot.ci.agg = data.frame(matrix(ncol = 2, nrow = 1772))
for(i in unique(matched$patzip)){
  
  dat = matched[which(matched$patzip == i),]
  x = dat$diffMedagg
  mean.x = mean(x, na.rm = T)
  sd.x = sd(x, na.rm = T)
  n = length(x)
  
  z = numeric(B)
  for (j in 1:B) {
    boot = sample(n,replace=TRUE)
    z[j] = (mean(x[boot], na.rm = T) - mean.x)/sd(x[boot], na.rm = T)
  }
  z <- z[is.finite(z)]
  pivot.ci.agg[k,] = mean.x + sd.x * quantile(z,c(.025,.975), na.rm = T)
  pivot.ci.agg$mean[k] = mean.x
  pivot.ci.agg$sd[k] = sd.x
  k = k + 1
}
pivot.ci.agg$ZCTA5CE10 = pivot.ci.cvd$ZCTA5CE10 = pivot.ci.resp$ZCTA5CE10 = pivot.ci.renal$ZCTA5CE10 =pivot.ci.de$ZCTA5CE10 = pivot.ci.hi$ZCTA5CE10 = unique(matched$patzip)
pivot.ci.agg$ZCTA5CE10 = unique(matched$patzip)
pivot.ci.agg$se.est <- (pivot.ci.agg$mean - pivot.ci.agg$X1)/2

library(sf)
library("ggplot2")
shape <- st_read(dsn = "~/Downloads/ShapefileZCTA/cb_2016_us_zcta510_500k.shp")
m <- merge(shape, pivot.ci.agg, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$mean/m$se.est), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Mean/SE") + ggtitle("Mean/SE Estimate, Grey = NA")


library(sf)
shape <- st_read(dsn = "~/Downloads/ShapefileZCTA/cb_2016_us_zcta510_500k.shp")
m <- merge(shape, pivot.ci.cvd, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound CVD, Grey = NA")


m <- merge(shape, pivot.ci.resp, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound Respiratory, Grey = NA")


m <- merge(shape, pivot.ci.renal, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound Acute Renal Failure, Grey = NA")


m <- merge(shape, pivot.ci.de, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound Dehydration, Grey = NA")


m <- merge(shape, pivot.ci.hi, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound Heat Illness, Grey = NA")


m <- merge(shape, pivot.ci.agg, by = ("ZCTA5CE10"))
p <- ggplot(data = m) + geom_sf(aes(fill = m$X1), colour = NA)
p + scale_fill_gradient2(low = "blue",mid = "white",
                         high="red") + theme(legend.key.size = unit(2, "cm")) + labs(fill = "Lower Bound") + ggtitle("Lower Bound Aggregate Difference, Grey = NA")
