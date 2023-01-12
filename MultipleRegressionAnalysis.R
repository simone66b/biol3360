
#### ####

d1 <- read.csv("C3C4.csv")
head(d1)


# lat long and centering
plot(d1[,c(1,7,8)])
cor(d1[,c(1,7,8)])

m1 <- lm(lc3~lat*long,data=d1)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

library(car)
vif(m1)

d1$cent.long <- d1$long-mean(d1$long)
d1$cent.lat <- d1$lat-mean(d1$lat)
plot(d1[,c(1,15,16)])
cor(d1[,c(1,15,16)])

plot((d1$lat*d1$long)~d1$lat)
plot((d1$lat*d1$long)~d1$long)

plot((d1$cent.lat*d1$cent.long)~d1$lat)
plot((d1$cent.lat*d1$cent.long)~d1$long)

m2 <- lm(lc3~cent.lat*cent.long,data=d1)
par(mfrow=c(2,2))
plot(m2)

summary(m2)

library(car)
avPlots(m2)

# added variable plots
r.lc3 <- residuals(lm(lc3~cent.long + cent.lat:cent.long,data=d1))
r.lat <- residuals(lm(cent.lat ~ cent.long + cent.lat:cent.long,data=d1))
plot(r.lc3~r.lat)
abline(lm(r.lc3~r.lat))

r.lc3 <- residuals(lm(lc3~cent.lat + cent.lat:cent.long,data=d1))
r.long <- residuals(lm(cent.long ~ cent.lat + cent.lat:cent.long,data=d1))
plot(r.lc3~r.long)
abline(lm(r.lc3~r.long))

r.lc3 <- residuals(lm(lc3~cent.lat + cent.long,data=d1))
r.latlong <- residuals(lm((cent.long*cent.lat) ~ cent.lat + cent.long,data=d1))
plot(r.lc3~r.latlong)
abline(lm(r.lc3~r.latlong))

avPlots(m2)

# 3d plot. 
# depth calculation function (adapted from Duncan Murdoch at https://stat.ethz.ch/pipermail/r-help/2005-September/079241.html)
(ab <- coef(m2))
FUN <- function(x1,x2,x3){ab[1]+ab[2]*x1+ab[3]*x2+ab[4]*x1*x2}

latitude <- seq(min(d1$cent.lat),max(d1$cent.lat),length=50)
longitude <- seq(min(d1$cent.long),max(d1$cent.long),length=50)
lc3 <- outer(latitude,longitude,FUN)

depth3d <- function(x,y,z, pmat, minsize=0.2, maxsize=2) {
  
  # determine depth of each point from xyz and transformation matrix pmat
  tr <- as.matrix(cbind(x, y, z, 1)) %*% pmat
  tr <- tr[,3]/tr[,4]
  
  # scale depth to point sizes between minsize and maxsize
  psize <- ((tr-min(tr) ) * (maxsize-minsize)) / (max(tr)-min(tr)) + minsize
  return(psize)
}

par(mfrow=c(1,1))
persp(latitude,longitude,lc3,theta=225,phi=20,r=2,ltheta=-120,ticktype="detail",shade=0.3) -> res
psize = depth3d(d1$cent.lat,d1$cent.long,d1$lc3,pmat=res,minsize=0.5, maxsize = 2)
# from 3D to 2D coordinates
mypoints <- trans3d(d1$cent.lat,d1$cent.long,d1$lc3,pmat=res)
# plot in 2D space with pointsize related to distance
points(mypoints, pch=16, cex=psize, col=4)


### modeling all predictors, and using a model selection framework on this one. 
head(d1)
d2 <- d1[,c(9,3,4,5,6,11,12)]
m3 <- lm(lc3~(map+mat+jjamap+djfmap+clong+clat)^6,data=d2)
d2$lat.long <- d2$clat*d2$clong

d3 <- d2[,c(2:8,1)]
head(d3)
plot(d3)

# install.packages("bestglm")
library(bestglm)
?bestglm
m.sel <- bestglm(d3,IC="AIC",TopModels = 30)
m.sel$BestModels
plot(1:30,m.sel$BestModels$Criterion)

install.packages("MuMIn")
library(MuMIn)

m3 <- lm(lc3~(map+mat+jjamap+djfmap+clong+clat)^2,data=d3,na.action=na.fail)
model.list <- dredge(m3)
?sw
sw(model.list)
?dredge

av.model <- model.avg(model.list,fit=T)
av.model

coef(av.model)

install.packages("glmulti")
library(glmulti)
m4 <- lm(lc3~(map+mat+jjamap+djfmap+clong+clat),data=d3,na.action=na.fail)
modellist <- glmulti(m4,level=1,crit="aicc")
modellist
weightable(modellist)

library(MuMIn)
?MuMIn

