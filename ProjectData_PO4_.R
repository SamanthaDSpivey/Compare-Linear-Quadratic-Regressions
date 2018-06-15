#New Project Data - 4-29-10 - Final - Phosphate


rm(list=ls())               #clears memory
setwd=("E:/2010 Spring Mines/Statistics II MATH531/Project")

##Phosphate original data points##
cal.x<-c(1.00,2.50,5.00,7.50,10.00)
cal.y<-c(26399,66994,138121,211074,288177)
scal.y<-(cal.y-mean(cal.y))/sd(cal.y)



##Linear Model##
cal.lin<-lm(scal.y~cal.x)
#abline(cal.lin,lwd=2)
summary(cal.lin)
#Call:
#lm(formula = scal.y ~ cal.x)
#Residuals:
#        1         2         3         4         5 
# 0.021977 -0.006358 -0.020902 -0.018240  0.023523 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.424195   0.020864  -68.26 6.93e-06 ***
#cal.x        0.273884   0.003398   80.60 4.21e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.02481 on 3 degrees of freedom
#Multiple R-squared: 0.9995,     Adjusted R-squared: 0.9994 
#F-statistic:  6496 on 1 and 3 DF,  p-value: 4.209e-06 

#y=(-1.424)+0.274*x



##Quadratic Model##
cal.quad<-lm(scal.y~cal.x+I(cal.x^2))
new.cal.x2<-seq(min(cal.x),max(cal.x),len=100)
cal.X2<-cbind(1,new.cal.x2,new.cal.x2^2)
new.cal.y2<-cal.X2%*%cal.quad$coef
#lines(new.cal.x2,new.cal.y2,lwd=2)
summary(cal.quad)
#Call:
#lm(formula = scal.y ~ cal.x + I(cal.x^2))
#Residuals:
#         1          2          3          4          5 
# 0.0005128 -0.0019512  0.0034538 -0.0029000  0.0008846 
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.3796904  0.0047548 -290.17 1.19e-05 ***
#cal.x        0.2485263  0.0021641  114.84 7.58e-05 ***
#I(cal.x^2)   0.0023171  0.0001927   12.02  0.00685 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.003549 on 2 degrees of freedom
#Multiple R-squared:     1,      Adjusted R-squared:     1 
#F-statistic: 1.588e+05 on 2 and 2 DF,  p-value: 6.298e-06 

#y=(-1.3797)+0.249*x+0.00232*x^2



##Plot of Calibration Data & True Linear & Quadratic Curves##
plot(cal.x,scal.y,pch=16,xlab="Amount (mg/L)", ylab="Area")
title("True Linear & Quadratic Curves for Phosphate",cex.main=1.5)
legend(1,1.4,c("Linear  y=(-1.424)+0.274*x", 
 "Quadratic y=(-1.3797)+0.249*x+0.00232*x^2", "ustarhat=0.05", "xstarhat=5.38",
 "xstarhat=5.47"),lty=c(2,3,4,4,4),lwd=c(2,2,1,1,1),col=c(2,4,3,2,4),bty="n",cex=.85)
abline(cal.lin,lty=2,lwd=2,col=2)               #linear
lines(new.cal.x2,new.cal.y2,lty=3,lwd=2,col=4)  #quadratic



##Determination of x.mid* value##
yy<-seq(min(-.55),max(.55),len=100)
l.b0hat<-cal.lin$coef[1]
#(Intercept) 
#  -1.424195
l.b1hat<-cal.lin$coef[2]
#   sim.x 
# 0.2738837    
l.xstarhat<-((yy-l.b0hat)/l.b1hat)

q.b0hat<-cal.quad$coef[1]
#(Intercept) 
#  -1.379690 
q.b1hat<-cal.quad$coef[2]
#    sim.x 
#  0.2485263  
q.b2hat<-cal.quad$coef[3]
#I(sim.x^2) 
#  0.006462414 
q.xstarhat<-((-q.b1hat+sqrt(q.b1hat^2-(4*q.b2hat*(q.b0hat-yy))))/(2*q.b2hat))

diff<-q.xstarhat-l.xstarhat
max(diff)
#  0.09080943 
max.diff<-which(diff>.09079)
#   54 55 56
diff[54]
#  0.09079654
diff[55]
# 0.09080943             #max diff b/w linear & quad cal
diff[56]
# 0.09079448
yy[55]
#  0.05                 #true ystar value-mid
l.xstarhat[55]
#  5.382559             #true xstar for linear model
q.xstarhat[55]
# 5.473369               #true xstar for quad model
abline(h=0.05,lty=4,lwd=1,col=3)
abline(v=5.382559,lty=4,lwd=1,col=2)
abline(v=5.473369,lty=4,lwd=1,col=4)







