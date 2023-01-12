
install.packages("bookdown")
library(bookdown)

# simulate data to which you can then fit a model
(x1 <- runif(100, 0, 15))
(y1 <- rnorm(100,1000 + 200*x1,100))
df1 <- data.frame(x1, y1)
lm_fit1 <- lm(y1 ~ x1, data = df1)

par(mfrow=c(1,2))
plot(x1,y1)
abline(a = coef(lm_fit1)[1],b = coef(lm_fit1)[2],col="red",ylim=c(500,4500))

x <- x1
(y <- 1000 + 200*x + rnorm(100, 0, 100))
df <- data.frame(x, y)
lm_fit <- lm(y ~ x, data = df)

plot(x,y)
abline(a = coef(lm_fit)[1],b = coef(lm_fit)[2],col="blue",ylim=c(500,4500))

summary(lm_fit1)
summary(lm_fit)



for (i in c(2.5,5,7.5,10,12.5)){
points(i,coef(lm_fit)[1] + coef(lm_fit)[2]*i,pch=16,col="blue",cex=1.5)
dat <- cbind.data.frame(seq(coef(lm_fit)[1] + coef(lm_fit)[2]*i-400,coef(lm_fit)[1] + coef(lm_fit)[2]*i+400,by=1),dnorm(x = seq(coef(lm_fit)[1] + coef(lm_fit)[2]*i-400,coef(lm_fit)[1] + coef(lm_fit)[2]*i+400,by=1),mean = coef(lm_fit)[1] + coef(lm_fit)[2]*i, sd = 100))
names(dat) <- c("y","density")
dat$density2 <- dat$density*150 + i
lines(dat$density2,dat$y,lwd=2)
}

plot(x,y,type='n')
abline(a = coef(lm_fit)[1],b = coef(lm_fit)[2],col="red")

sd.begin=50

x.vals <- c(2.5,5,7.5,10,12.5)
sd.s <- c(50,75,100,125,150)
up.down <- c(200,300,400,500,600)
for (j in 1:5){
  i = x.vals[j]
  sd.actual=sd.s[j]
  up.down.act=up.down[j]
  points(i,coef(lm_fit)[1] + coef(lm_fit)[2]*i,pch=16,col="blue",cex=1.5)
  dat <- cbind.data.frame(seq(coef(lm_fit)[1] + coef(lm_fit)[2]*i-up.down.act,coef(lm_fit)[1] + coef(lm_fit)[2]*i+up.down.act,by=1),dnorm(x = seq(coef(lm_fit)[1] + coef(lm_fit)[2]*i-up.down.act,coef(lm_fit)[1] + coef(lm_fit)[2]*i+up.down.act,by=1),mean = coef(lm_fit)[1] + coef(lm_fit)[2]*i, sd = sd.actual))
  names(dat) <- c("y","density")
  dat$density2 <- dat$density*150 + i
  lines(dat$density2,dat$y,lwd=2)
}


#
x <- runif(100, 0, 15)
y <- 1000 + 200*x + rnorm(100, 0, 300)
df <- data.frame(x, y)
lm_fit <- lm(y ~ x, data = df)

par(mfrow=c(2,2))
plot(x,y)
abline(a = coef(lm_fit)[1],b = coef(lm_fit)[2],col="red")

?summary.lm

install.packages("tufte")
library(tufte)

?quote_footer()

?sigma

rm(list=ls())

d <- read.csv("iKung_HeightWeight.csv")
d2 <- d[d$age>=18,c(5,6)]
dim(d2)
d2 <- na.omit(d2)

d2

new <- cbind.data.frame(!is.na(d2$height),!is.na(d2$weight))
names(new) <- c("height","weight")
new$sum <- new$height + new$weight
d2$sum <- new$sum
d3 <- d2[d2$sum==2,]


m1 <- lm(formula = height ~ 1, data=d3)
m2 <- lm(formula = height ~ weight, data=d3) 

anova(m2)

(21040.2/351) / (9054.8/350)
anova(m2,m1)


assuming that the variance associated with the explanatory variable and with the absence of an explanatory variable are equal. 

anova(m1)
anova(m2)

11985.5/59.944

anova(m1,m2)

summary(m1)
summary(m2)

anova(m2)

anova(m2)

11985.5/(11985.5+9054.8)

summary(m2)

?sigma

print(anova(m1))

summary(m1)
anova(m1)

?anova

X <- seq(0,5,by=0.01)
plot(X,df(x=X,df1 = 1,df2 = 18))

summary(m2)
5.086^2

??lrtest

anova(m1,m2)
anova(m1)
anova(m2)

install.packages("lmtest")
library(lmtest)

lrtest(m1,m2)

?logLik

logLik(m1)

21040.2-9054.8

9054.8/350


anova(m2)


?aov
anova(m2)




par(mfrow=c(1,2))
# draw a standard normal distribution and shade probability density for values greater than 2
curve(dnorm,-4,4,n=1000,xlab="value of x (some observation, for example)",ylab="probability density",lwd=3,col="purple",yaxt="n",main="standard normal distribution")
xx <- c(seq(2,5,by=0.02),seq(5,2,by=-0.02))
yy <- c(dnorm(x=xx[1:(length(x)/2)]),rep(0,length(x)/2))
polygon(x = xx,y = yy,border = NA,col="purple")

# draw an F-distribution defined by certain values for the degrees of freedom, and shade the probability density for F-ratios greater than 2
df_regression = 1
df_residual = 12
curve(df(x,df1=df_regression,df2=df_residual),0,5,n=500,xlab="value of an F ratio (a ratio of variances)",ylab="probability density",lwd=3,col="purple",yaxt="n",main="central F distribution")
xx <- c(seq(2,5,by=0.02),seq(5,2,by=-0.02))
yy <- c(df(x=xx[1:(length(xx)/2)],df1=df_regression,df2=df_residual),rep(0,length(xx)/2))
polygon(x = xx,y = yy,border = NA,col="purple")

?df

1-pf(463.28,df1=1,df2=350)



d <- read.csv("iKung_HeightWeight.csv")
d2 <- d[d$age>=18,]
d2
m1 <- lm(formula = height ~ 1, data=d2)
m2 <- lm(formula = height ~ weight, data=d2)
summary(m1)
summary(m2)

anova(m2,m1)

?anova
anova(m2,m1,test="LRT")
anova(m2,m1,test="Chisq")
anova(m2,m1)

str(m1)

# Try to predict the outcome of the following expressions
#   
#   <details>
#   <summary>`NA + NaN`</summary>
#   ```{r echo = F, eval=T, error=T}
# NA + NaN
# ```
# </details>
#   
#   <details>
#   <summary>`Inf - Inf`</summary>
#   ```{r echo = F, eval=T, error=T}
# Inf - Inf
# ```
# </details>
#   
#   <details>
#   <summary>`c(NA, NaN, NULL)`</summary>
#   ```{r echo = F, eval=T, error=T}
# c(NA, NaN, NULL)
# ```
# </details>
#   
#   <details>
#   <summary>`NULL - Inf`</summary>
#   ```{r echo = F, eval=T, error=T}
# NULL - Inf
# ```
# </details>
#   
#   <details>
#   <summary>`c(NULL, NULL, NULL)`</summary>
#   ```{r echo = F, eval=T, error=T}
# c(NULL, NULL, NULL)
# ```
# </details>
# 

par(mfrow=c(4,1))
for (i in c(4,3,3.7,5)){
curve(dnorm(x,i,1),0,8,n=1000,xlab="observation",ylab="probability density",lwd=3,col="purple",yaxt="n",main= "",bty="n")
  abline(v=i,lty=3,lwd=2)
}

par(mfrow=c(1,1))
for (i in c(4)){
  curve(dnorm(x,i,1),0,8,n=1000,xlab="mean (or other 'statistic')",ylab="probability density",lwd=3,col="orange",yaxt="n",main= "",bty="n")
}

?confint



d <- read.csv("iKung_HeightWeight.csv")
d2 <- d[d$age>=18,c(5,6)] # subset the dataframe to only include the height and weight variables of adults over the age of 18
d2 <- na.omit(d2) # remove the rows of the new dataframe that contain NAs.

m1 <- lm(formula = height ~ weight, data=d2)

alpha <- 0.05 # where 1-alpha = 0.95 giving "95% confidence interval"
str(m1)

str(summary(m1))

summary(m1)$coefficients[2,2]

m1$df.residual
str(summary(m1))


m1$coef[2] - (qt(1-alpha/2,m1$df.residual)) * summary(m1)$coefficients[2,2] # lower
m1$coef[2] + (qt(1-alpha/2,m1$df.residual)) * summary(m1)$coefficients[2,2] # upper

confint(m1)

m1$coef[2] - (qt(1-alpha/2,m1$df.residual)) * summary(m1)$coefficients[2,2]

1-0.05

0.29/2

confint(m1)

predict(m1)

?data.frame

head(d2)

newdat <- data.frame(weight=seq(min(d2$weight),max(d2$weight),length.out=400))
head(newdat)
predCI <- predict(m1,newdata = newdat,interval = "c")








newdat <- data.frame(weight=seq(min(d2$weight),max(d2$weight),length.out=400))
head(newdat)


Now use the `predict()` command to predict new values of height (according to the fitted model), and to generate confidence intervals, using the argument `interval ="c"`
```{r eval=T, results='show',include=T,echo=T}
predCI <- as.data.frame(predict(m1,newdata = newdat,interval = "c"))
head(predCI)
```
\
And finally, plot the results:
  ```{r eval=T, results='show',include=T,echo=T}
plot(height~weight,data = d2)
lines(predCI$fit~newdat$weight,lwd=2) # add fitted/predicted line
lines(predCI$lwr~newdat$weight,lty=3,col="red",lwd=2) # add line for lower bound on CI
lines(predCI$upr~newdat$weight,lty=3,col="red",lwd=2) # add line for upper bound on CI
```

head(d)
m1 <- lm(height~weight,data=d3)
m2 <- glm(height~weight,data=d3)
m3 <- glm(height~1,data=d3)

anova(m1)
summary(m1)
summary(m2)

summary(m3)

anova(m2,m3,test="LRT")
anova(m2,test="LRT")

x <- rnorm(10,20,5)
m1 <- lm(x~1)

model.matrix(m1)

??stan_glm

number.in.each.group <- 100
group1 <- rnorm(number.in.each.group,0,1)
group2 <- rnorm(number.in.each.group,5,1)
response.var <- c(group1,group2)
d5 <- cbind.data.frame(rep(c("a","b"),each=number.in.each.group),response.var)
names(d5) <- c("group","response")
head(d5)

m5 <- lm(response~group,data=d5)
summary(m5)
model.matrix(m5)

par(mfrow=c(2,2))
plot(m5)

d <- read.csv("iKung_HeightWeight.csv")
d2 <- d[d$age>=18,c(4,5,6)] # subset the dataframe to only include the height and weight variables of adults over the age of 18
d2 <- droplevels(na.omit(d2)) 
d2$male_factor <- as.factor(d2$male)
str(d2)
head(d2)

plot(c(min(d2$height),max(d2$height))~c(-0.5,1.5),type="n",xaxt="n",xlab="male")
axis(1,at = c(0,1),labels = c("0","1"),)
points(d2$male,d2$height)
  


?text


install.packages("devtools")
devtools::install_github("JanEngelstaedter/boilrdata")

library(help="boilrdata")

??MASS

head(chem)

head(birthwt)
?birthwt

plot(birthwt$bwt~birthwt$smoke)
par(mfrow=c(2,2))
plot(lm(bwt~as.factor(smoke),data=birthwt))

summary(lm(bwt~ht,data=birthwt))

anova(lm(bwt~smoke,data=birthwt))
anova(lm(bwt~smoke,data=birthwt))

head(birthwt)
?birthwt

?jitter

m4 <- lm(bwt~as.factor(race)*as.factor(smoke),data=birthwt)
model.matrix(m4)
summary(m4)

coef(m4)

m5 <- lm(bwt~as.factor(race),data=birthwt)
model.matrix(m5)








d <- read.csv("iKung_HeightWeight.csv")
d1 <- d[d$age>=18,c(4,5,6)] # subset the dataframe to only include sex, height, and weight variables of adults over the age of 18
d1 <- droplevels(na.omit(d1)) # remove rows containing NAs
head(d1)
d1$male_factor <- as.factor(d1$male)
str(d1)
m1 <- lm(height~male_factor,data=d1)

head(d1)

summary(m1)

(CI <- confint(m1))

coef(m1)

str(summary(m1))
m1$effects


newdat <- as.data.frame(c(0,1))
dim(newdat)
newdat
class(newdat)
names(newdat) <- "male_factor"
newdat$male_factor <- as.factor(newdat$male_factor)
  
predict(m1,newdata = newdat,interval = "c")

plot(height~male_factor,data=d1)
text(1,145,labels=paste("y",round(summary(m1)$r.squared,digits = 3)))

?text
str(m1)

?bquote

require(graphics)
a <- 2

bquote(a == a)
quote(a == a)

bquote(expression(paste(a)) == .(a))

bquote(a == .(a))
substitute(a == A, list(A = a))

plot(1:10, a*(1:10), main = bquote(a == .(a)))

## to set a function default arg
default <- 1
bquote( function(x, y = .(default)) x+y )

exprs <- expression(x <- 1, y <- 2, x + y)
bquote(function() {..(exprs)}, splice = TRUE)

x <- 5
title(line = -17, main= expression(x==x))


plot(c(0,10),c(0,10))
(x <- round(summary(m1)$r.squared,3))
text(x = 5,y=5,bquote(~italic(r)^2==.(x)))


title(line = -18, main = bquote(x==.(x)))
title(line = -19, main = bquote(x==.(x)~mu*g~l^-1))

?text


?birthwt
str(birthwt)
summary(birthwt$ptl)
table(birthwt$ptl)
table(birthwt$race)
table(birthwt$ftv)


m4 <- lm(bwt~as.factor(race)*as.factor(smoke),data=birthwt)
model.matrix(m4)
summary(m4)

coef(m4)

table(birthwt$race)
m5a <- lm(bwt~as.factor(race),data=birthwt)
model.matrix(m5a)
anova(m5a)
summary(m5a)

m5b <- lm(bwt~0+as.factor(race),data=birthwt)
model.matrix(m5b)
anova(m5b)
summary(m5b)

coef(m5a)
coef(m5b)

anova(m5a)
anova(m5b)

dim(birthwt)


table(birthwt$race)
# linear (or 'factor') effects model (default in R and preferred by Quinn and Keough)
m5a <- lm(bwt~as.factor(race),data=birthwt)
model.matrix(m5a)
anova(m5a)
summary(m5a)

# cell means model
m5b <- lm(bwt~as.factor(race)-1,data=birthwt)
model.matrix(m5b)
anova(m5b)
summary(m5b)



?lm

table(birthwt$race2)
str(birthwt)
# linear (or 'factor') effects model (default in R and preferred by Quinn and Keough)
birthwt$race2 <- as.factor(birthwt$race)
m5a <- lm(bwt~race2,data=birthwt,contrasts=list(race2="contr.treatment"))
mat <- model.matrix(m5a)
attr(mat, "contrasts")
anova(m5a)
summary(m5a)

?birthwt

?lm

install.packages("ggplot2")
library(ggplot2)
?msleep

install.gg

head(msleep)

as.data.frame(msleep)

d1 <- as.data.frame(msleep)
str(d1)
d2 <- d1
d2$vore <- as.factor(d2$vore)
d3 <- d2
d3$carnivore <- ifelse(d2$vore == "herbi",0,1)
head(d3)

m1 <- lm(log(sleep_total)~log(brainwt)*carnivore,data=d3)
anova(m1)










par(mfrow=c(2,2))
plot(log(sleep_total)~log(brainwt),data=d1)
par(mfrow=c(2,2))
plot(lm(log(sleep_total)~log(brainwt),data=d1))

head(d1)
plot(lm(log(sleep_total)~vore,data=d1))

str(d1)
d1$vore2 <- as.factor(d1$vore)
str(d1)
table(d1$vore2)

m2 <- lm(log(sleep_total)~log(brainwt)*vore2,data=d1[d1$vore2!="insecti",])
plot(m2)
anova(m2)
summary(m2)

d2 <- d1[d1$vore2!="insecti",]
head(d2)
str(d2)

plot(log(sleep_total)~log(brainwt),data=d2,type="n")
points(log(sleep_total)~log(brainwt),data=d2[d2$vore2=="herbi",],col=1)
points(log(sleep_total)~log(brainwt),data=d2[d2$vore2=="carni",],col=2)
points(log(sleep_total)~log(brainwt),data=d2[d2$vore2=="omni",],col=3)


d2$vore3 <- 
  
  if(d1$vore)

newdat <- 
pred <- 



str(d1)
d1$vore2 <- as.factor(d1$vore)
str(d1)
table(d1$vore2)


require(ggplot2)
d1 <- msleep


rm(list=ls())
#### ####
d1 <- as.data.frame(msleep)
table(d1$vore)
d1$vore <- as.factor(d1$vore)
d1$carnivore <- ifelse(d1$vore == "herbi",0,1)
head(d1)
d1$sleep_ratio <- d1$sleep_total/d1$awake

head(d1)

plot(d1[,c(6,7,9:12)]) # the


# model with categorical and continuous. 
m1 <- lm(sleep_total~log10(bodywt)*as.factor(carnivore),data=d1)
summary(m1)
anova(m1)
par(mfrow=c(2,2))
plot(m1)

head(d1)
newdat <- data.frame(rep(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=500),2),rep(c(0,1),each=500))
names(newdat) <- c("bodywt","carnivore")
pred <- as.data.frame(predict(m1,newdata = newdat,interval = "c"))
newdat <- cbind.data.frame(newdat,pred)
head(newdat)

par(mfrow=c(1,1))
plot((d1$sleep_total)~log10(d1$bodywt),type='n')

with(d1[d1$carnivore==0,],points((sleep_total)~log10(bodywt),col="darkgreen",pch=1,lwd=2,cex=1.5))
with(d1[d1$carnivore==1,],points((sleep_total)~log10(bodywt),col="darkred",pch=1,lwd=2,cex=1.5))

with(newdat[newdat$carnivore==0,],lines(fit~log10(bodywt),col="darkgreen",pch=16,lwd=4))
with(newdat[newdat$carnivore==1,],lines(fit~log10(bodywt),col="darkred",pch=16,lwd=4))

with(newdat[newdat$carnivore==0,],lines(lwr~log10(bodywt),col="darkgreen",pch=16,lwd=1,lty=3))
with(newdat[newdat$carnivore==1,],lines(lwr~log10(bodywt),col="darkred",pch=16,lwd=1,lty=3))

with(newdat[newdat$carnivore==0,],lines(upr~log10(bodywt),col="darkgreen",pch=16,lwd=1,lty=3))
with(newdat[newdat$carnivore==1,],lines(upr~log10(bodywt),col="darkred",pch=16,lwd=1,lty=3))


# model with categorical
m2 <- lm((sleep_total)~as.factor(carnivore),data=d1)
anova(m2)
summary(m2)
par(mfrow=c(2,2))
plot(m2)

(newdat2 <- data.frame(carnivore = c(0,1)))
(pred2 <- predict(m2,newdata=newdat2,interval="c"))
(newdat2 <- cbind.data.frame(newdat2,pred2))

par(mfrow=c(1,1))
plot(c(-0.25,1.25),c(min((na.omit(d1$sleep_total))),max((na.omit(d1$sleep_total)))),type="n",xaxt="n",xlab=expression(paste("'...vore'")),ylab="sleep_total")
axis(side=1,at=c(0,1),labels=c("herbivore","carnivore"))
with(d1[d1$carnivore==0,],points(jitter(carnivore,factor = 1.05),(sleep_total),col="darkgreen",pch=1))
with(d1[d1$carnivore==1,],points(jitter(carnivore,factor = 1.05),(sleep_total),col="darkred",pch=1))
with(newdat2,points(carnivore,fit,cex=3,pch=16,col=c("darkgreen","darkred")))
with(newdat2,arrows(x0=carnivore,x1=carnivore,y0=lwr,y1=upr,angle = 90,length = 0.1,code = 3,lwd=2,col=c("darkgreen","darkred")))
     
# model with just continuous
m3 <- lm((sleep_total)~log10(bodywt),data=d1)
par(mfrow=c(2,2))
plot(m3)

summary(m3)
anova(m3)

newdat3 <- data.frame(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=500))
names(newdat3) <- c("bodywt")
head(newdat3)
pred <- as.data.frame(predict(m3,newdata = newdat3,interval = "c"))
head(pred)
newdat3 <- cbind.data.frame(newdat3,pred)
head(newdat3)

par(mfrow=c(1,1))
with(newdat3,plot(fit~log10(bodywt),pch=1,lwd=2,col=1,type='n',xlab="log10(bodywt)"))
polygon(x=c(log10(newdat3$bodywt),rev(log10(newdat3$bodywt))),y=c(newdat3$lwr,rev(newdat3$upr)),col = rgb(red=190/255,green=190/255,blue=190/255,alpha=80/255),border = F)
with(d1,points((sleep_total)~log10(bodywt),pch=1,lwd=2,col=1))
with(newdat3,lines(fit~log10(bodywt),pch=1,lwd=2,col=1))



?rgb
col2rgb("gray",alpha=T)



with(newdat3,lines(lwr~log10(bodywt),pch=1,lwd=1,col=1,lty=3))
with(newdat3,lines(upr~log10(bodywt),pch=1,lwd=1,col=1,lty=3))

log10(0.5)


newdat <- data.frame(bodywt=rep(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=1000),2),carnivorous=rep(c(0,1),each=1000))

m3 <- lm(sleep_total~as.factor(carnivorous)+log10(bodywt),data=d1)
summary(m3)

newdat <- data.frame(bodywt=rep(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=1000),2),carnivorous=rep(c(0,1),each=1000))
pred <- predict(m3,newdata=newdat,interval="c")
newdat <-cbind.data.frame(newdat,pred)


with(newdat,plot(fit~log10(bodywt),type="n",xlab="Body Weight (log10(kg))",ylab="Total Sleep Hours"))
with(d1[d1$carnivorous==0,],points(sleep_total~log10(bodywt),col="darkgreen"))
with(d1[d1$carnivorous==1,],points(sleep_total~log10(bodywt),col="darkred"))

with(newdat[newdat$carnivorous==0,],lines(fit~log10(bodywt),col="darkgreen"))
with(newdat[newdat$carnivorous==1,],lines(fit~log10(bodywt),col="darkred"))


with(newdat[newdat$carnivorous==0,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = "gray")) 
with(newdat[newdat$carnivorous==0,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = "gray")) 


####
m4 <- lm(sleep_total~as.factor(carnivorous)+log10(bodywt)+as.factor(carnivorous):log10(bodywt),data=d1)
summary(m4)
anova(m4)

newdat <- data.frame(bodywt=rep(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=1000),2),carnivorous=rep(c(0,1),each=1000))
pred <- predict(m4,newdata=newdat,interval="c")
newdat <-cbind.data.frame(newdat,pred)

with(d1,plot(sleep_total~log10(bodywt),type="n",xlab="Body Weight (log10(kg))",ylab="Total Sleep Hours"))

with(newdat[newdat$carnivorous==0,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = rgb(0,100/255,0,50/255),border=NA)) 
with(newdat[newdat$carnivorous==1,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = rgb(139/255,0/255,0,50/255),border=NA))

with(d1[d1$carnivorous==0,],points(sleep_total~log10(bodywt),col="darkgreen"))
with(d1[d1$carnivorous==1,],points(sleep_total~log10(bodywt),col="darkred"))

with(newdat[newdat$carnivorous==0,],lines(fit~log10(bodywt),col="darkgreen"))
with(newdat[newdat$carnivorous==1,],lines(fit~log10(bodywt),col="darkred"))

plot(log10(d1$bodywt)~log10(d1$brainwt))
summary(lm(log10(bodywt)~log10(brainwt),data=d1))

m5 <- lm(sleep_total~log10(brainwt)*log10(bodywt),data=d1)
anova(m5)
summary(m5)

require(ggplot2)
d1 <- msleep

write.csv(x = d1,file = "sleep.csv")

require(ggplot2)
d1 <- msleep # assign the mammal sleep dataframe to d1
d1$vore <- as.factor(d1$vore) # the variable 'vore' is stored as a character. Convert to a factor for analyses. 
d1$carnivorous <- ifelse(d1$vore == "herbi",0,1) # cr

d2 <- droplevels(d1[d1$vore!="insecti",])
str(d2)
table(d2$vore)

m4 <- lm(sleep_total~as.factor(carnivorous)+log10(bodywt)+as.factor(carnivorous):log10(bodywt),data=d1)
m5 <- lm(sleep_total~log10(bodywt),data=d1[d1$carnivorous==0,])
m6 <- lm(sleep_total~log10(bodywt),data=d1[d1$carnivorous==1,])

summary(m4)
summary(m5)
summary(m6)

m7 <- lm(lm(brainwt~as.factor(carnivorous)+log10(bodywt)+as.factor(carnivorous):log10(bodywt),data=d1))
anova(m7)

m8 <- lm(sleep_total~vore+log10(bodywt)+vore:log10(bodywt),data=d2)
summary(m8)
anova(m8)

d$characters2 <- round(d$words*runif(dim(d)[1],min = 4.25,max=6.25))

d <- d[,c(1,2,8,4,5,6,7)]
names(d)[3] <- "characters"
d
write.csv(d,file = "BotHelv_characters.csv")

?vif

?spline
zed <- plot(d1$sleep_total~log10(d1$bodywt),pch=16,cex=1)
mod.ss <-smooth.spline(log10(d1$bodywt),d1$sleep_total)
lines(x,mod.ss$y)
zed1 <- spline(zed)
lines(zed$y~zed$x)

d1
m4 <- lm(sleep_total~as.factor(carnivorous)*log10(bodywt),data=d1)
newdat <- data.frame(bodywt=rep(seq(min(na.omit(d1$bodywt)),max(na.omit(d1$bodywt)),length.out=1000),2),carnivorous=rep(c(0,1),each=1000))
pred <- predict(m4,newdata=newdat,interval="c")
newdat <-cbind.data.frame(newdat,pred)

with(d1,plot(sleep_total~log10(bodywt),type="n",xlab="Body Weight (log10(kg))",ylab="Total Sleep Hours"))

with(newdat[newdat$carnivorous==0,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = rgb(0,100/255,0,50/255),border=NA)) 
with(newdat[newdat$carnivorous==1,],polygon(x=c(log10(bodywt),rev(log10(bodywt))),y=c(lwr,rev(upr)),col = rgb(139/255,0/255,0,50/255),border=NA))

with(d1[d1$carnivorous==0,],points(sleep_total~log10(bodywt),col="darkgreen",pch=16))
with(d1[d1$carnivorous==1,],points(sleep_total~log10(bodywt),col="darkred",pch=16))

with(newdat[newdat$carnivorous==0,],lines(fit~log10(bodywt),col="darkgreen",lwd=2))
with(newdat[newdat$carnivorous==1,],lines(fit~log10(bodywt),col="darkred",lwd=2))





```{r eval=T, results='show',include=T,echo=T,message=F}
newdat2 <- data.frame(carnivorous = c(0,1)) # new data for predictions
pred2 <- predict(m2,newdata=newdat2,interval="c") # fitted values and 95% CIs
newdat2 <- cbind.data.frame(newdat2,pred2) # new data and predictions together, to help with plotting. 

par(mfrow=c(1,1))
plot(c(-0.25,1.25),c(min((na.omit(d1$sleep_total))),max((na.omit(d1$sleep_total)))),type="n",xaxt="n",xlab=expression(paste("'...vore'")),ylab="Total Sleep Hours")
axis(side=1,at=c(0,1),labels=c("herbivore","carnivore"))
with(d1[d1$carnivorous==0,],points(jitter(carnivorous,factor = 1.05),(sleep_total),col="darkgreen",pch=1,lwd=0.5)) # raw data
with(d1[d1$carnivorous==1,],points(jitter(carnivorous,factor = 1.05),(sleep_total),col="darkred",pch=1,lwd=0.5)) # raw data
with(newdat2,points(carnivorous,fit,cex=2,pch=16,col=c("darkgreen","darkred"))) # model estimates
with(newdat2,arrows(x0=carnivorous,x1=carnivorous,y0=lwr,y1=upr,angle = 90,length = 0.1,code = 3,lwd=2,col=c("darkgreen","darkred"))) # CIs
```


y <- rnorm(1000,100,20)
summary(y)
m1 <- glm(y~1,family = "poisson")
exp(predict(m1))
m2 <- glm(y~1,family = "gaussian")
predict(m2)

y <- rpois(1000,2)
hist(y)
summary(y)
m3 <- glm(y~1,family = "poisson")
exp(predict(m3))
m4 <- glm(y~1,family = "gaussian")
predict(m4)

hist(y)
