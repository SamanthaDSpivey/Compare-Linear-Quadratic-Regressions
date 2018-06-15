#New Project Data - 4-29-10 - Final - Chloride


rm(list=ls())               #clears memory
setwd=("E:/2010 Spring Mines/Statistics II MATH531/Project")


##Chloride original data points##
cal.x<-c(1.00,2.50,5.00,7.50,10.00)
cal.y<-c(89380,235698,516525,848121,1192537)
scal.y<-(cal.y-mean(cal.y))/sd(cal.y)



##Linear Model##
cal.lin<-lm(scal.y~cal.x)
#abline(cal.lin,lwd=2)
summary(cal.lin)
#Call:
#lm(formula = scal.y ~ cal.x)
#Residuals:
#       1        2        3        4        5 
# 0.06628 -0.01879 -0.07846 -0.02532  0.05629 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.421902   0.058896  -24.14 0.000156 ***
#cal.x        0.273443   0.009592   28.51 9.48e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.07003 on 3 degrees of freedom
#Multiple R-squared: 0.9963,     Adjusted R-squared: 0.9951 
#F-statistic: 812.6 on 1 and 3 DF,  p-value: 9.478e-05

#y=(-1.422)+0.273*x


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
#        1         2         3         4         5 
# 0.006417 -0.006501 -0.010530  0.017460 -0.006847 
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.2977761  0.0221373 -58.624 0.000291 ***
#cal.x        0.2027198  0.0100757  20.120 0.002461 ** 
#I(cal.x^2)   0.0064624  0.0008972   7.203 0.018733 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.01652 on 2 degrees of freedom
#Multiple R-squared: 0.9999,     Adjusted R-squared: 0.9997 
#F-statistic:  7324 on 2 and 2 DF,  p-value: 0.0001365

#y=(-1.298)+0.2027*x+0.0064624*x^2


##Plot of Calibration Data & True Linear & Quadratic Curves##
plot(cal.x,scal.y,pch=16,xlab="Amount (mg/L)", ylab="Area")
title("True Linear & Quadratic Curves for Chloride",cex.main=1.5)
legend(1,1.4,c("Linear  y=(-1.422)+0.273*x", 
 "Quadratic y=(-1.298)+0.2027*x+0.0064624*x^2", "ustarhat=0.0056", "xstarhat=5.22",
 "xstarhat=5.47"),lty=c(2,3,4,4,4),lwd=c(2,2,1,1,1),col=c(2,4,3,2,4),bty="n",cex=.85)
abline(cal.lin,lty=2,lwd=2,col=2)               #linear
lines(new.cal.x2,new.cal.y2,lty=3,lwd=2,col=4)  #quadratic


##Determination of x.mid* value##
yy<-seq(min(-.55),max(.55),len=100)
l.b0hat<-cal.lin$coef[1]
#(Intercept) 
#  -1.421902
l.b1hat<-cal.lin$coef[2]
#   sim.x 
# 0.2734426   
l.xstarhat<-((yy-l.b0hat)/l.b1hat)

q.b0hat<-cal.quad$coef[1]
#(Intercept) 
#  -1.297776
q.b1hat<-cal.quad$coef[2]
#    sim.x 
#  0.2027198  
q.b2hat<-cal.quad$coef[3]
#I(sim.x^2) 
#  0.006462414 
q.xstarhat<-((-q.b1hat+sqrt(q.b1hat^2-(4*q.b2hat*(q.b0hat-yy))))/(2*q.b2hat))

diff<-q.xstarhat-l.xstarhat
max(diff)
#  0.2536800 
max.diff<-which(diff>.25363)
#   50 51 52
diff[50]
#  0.253645
diff[51]
# 0.2536800             #max diff b/w linear & quad cal
diff[52]
# 0.2536369
yy[51]
#  0.005555556          #true ystar value-mid
l.xstarhat[51]
#  5.220317             #true xstar for linear model
q.xstarhat[51]
# 5.473997               #true xstar for quad model
abline(h=0.005555556,lty=4,lwd=1,col=3)
abline(v=5.220317,lty=4,lwd=1,col=2)
abline(v=5.473997,lty=4,lwd=1,col=4)






