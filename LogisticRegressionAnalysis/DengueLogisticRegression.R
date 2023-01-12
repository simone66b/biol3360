
# logistic regression analysis
d1 <- na.omit(read.csv("dengue.csv"))
head(d1)

m1 <- glm(NoYes~humid,family = "binomial",data=d2) # non-shortcut way of specifying the model
summary(m1) # 

family(m1)$linkinv

summary(m2) # 
anova(m1,test="Chisq") # ratio of deviances

plot(m1$residuals~m1$fitted.values)

# 
# generate predictions on the link scale
newdat <- data.frame(humid=seq(min(d2$humid),max(d2$humid),length.out=400))
pred.link<- predict(m1,newdata = newdat,type="link",se.fit=T) # se fit equals T
head(pred.link)

# the inverse link function
(inv.link <- family(m1)$linkinv)

m1.fit <- inv.link(pred.link$fit)
m1.upr <- inv.link(pred.link$fit+1.96*pred.link$se.fit)
m1.lwr <- inv.link(pred.link$fit-1.96*pred.link$se.fit)

par(mfrow=c(1,1))
plot(newdat$humid,m1.fit,type='n',ylim=c(0,1),ylab="probability of occurrence (p), response scale")
polygon(c(newdat$humid,rev(newdat$humid)),y = c(m1.upr,rev(m1.lwr)),col = "lightblue",border=F)
lines(newdat$humid,m1.fit,type='l')
points(jitter(d1$NoYes,factor=0.05)~d1$humid)
axis(side=2,labels="probability of occurrence (p), response scale")
# lines(newdat$humid,m1.upr,lty=3)
# lines(newdat$humid,m1.lwr,lty=3)

pred.response <- predict(m1,newdata = newdat,type="response",interval='c')
head(pred.response)

par(mfrow=c(1,2))
plot(newdat$humid,pred.link$fit,type='l',lwd=3,xlab="humidity",ylab="log odds of occurrence")
plot(newdat$humid,pred.response,ylim=c(0,1),type='l',lwd=3,xlab="humidity",ylab="probability of occurrence")
points(jitter(d1$NoYes,factor=0.05)~d1$humid)



