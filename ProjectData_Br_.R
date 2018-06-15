#New Project Data - 4-29-10 - Final - Bromide


rm(list=ls())               #clears memory
setwd=("E:/2010 Spring Mines/Statistics II MATH531/Project")


##Bromide original data points##
cal.x<-c(1.00,2.50,5.00,7.50,10.00)
cal.y<-c(38960,99745,210362,323949,449479)
scal.y<-(cal.y-mean(cal.y))/sd(cal.y)




##Linear Model##
cal.lin<-lm(scal.y~cal.x)
#abline(cal.lin,lwd=2)
summary(cal.lin)
#Call:
#lm(formula = scal.y ~ cal.x)
#Residuals:
#       1        2        3        4        5 
# 0.03457 -0.01071 -0.03022 -0.03189  0.03825 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.423686   0.033299  -42.75 2.82e-05 ***
#cal.x        0.273786   0.005423   50.48 1.71e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.03959 on 3 degrees of freedom
#Multiple R-squared: 0.9988,     Adjusted R-squared: 0.9984 
#F-statistic:  2549 on 1 and 3 DF,  p-value: 1.712e-05

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
# 0.0006243 -0.0037395  0.0082969 -0.0076227  0.0024410 
#Coefficients:
#              Estimate Std. Error  t value Pr(>|t|)    
#(Intercept) -1.3532960  0.0114967 -117.712 7.22e-05 ***
#cal.x        0.2336797  0.0052327   44.658 0.000501 ***
#I(cal.x^2)   0.0036648  0.0004659    7.866 0.015782 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#Residual standard error: 0.008581 on 2 degrees of freedom
#Multiple R-squared:     1,      Adjusted R-squared: 0.9999 
#F-statistic: 2.716e+04 on 2 and 2 DF,  p-value: 3.682e-05

#y=(-1.353)+0.234*x+0.00367*x^2



##Plot of Calibration Data & True Linear & Quadratic Curves##
plot(cal.x,scal.y,pch=16,xlab="Amount (mg/L)", ylab="Area")
title("True Linear & Quadratic Curves for Bromide",cex.main=1.5)
legend(1,1.4,c("Linear  y=(-1.424)+0.274*x", 
 "Quadratic y=(-1.353)+0.234*x+0.00367*x^2", "ustarhat=0.039", "xstarhat=5.34",
 "xstarhat=5.49"),lty=c(2,3,4,4,4),lwd=c(2,2,1,1,1),col=c(2,4,3,2,4),bty="n",cex=.85)
abline(cal.lin,lty=2,lwd=2,col=2)               #linear
lines(new.cal.x2,new.cal.y2,lty=3,lwd=2,col=4)  #quadratic



##Determination of x.mid* value##
yy<-seq(min(-.55),max(.55),len=100)
l.b0hat<-cal.lin$coef[1]
#(Intercept) 
#  -1.423686 
l.b1hat<-cal.lin$coef[2]
#   sim.x 
# 0.2737858   
l.xstarhat<-((yy-l.b0hat)/l.b1hat)

q.b0hat<-cal.quad$coef[1]
#(Intercept) 
#  -1.353296 
q.b1hat<-cal.quad$coef[2]
#    sim.x 
#  0.2336797 
q.b2hat<-cal.quad$coef[3]
#I(sim.x^2) 
#  0.003664760 
q.xstarhat<-((-q.b1hat+sqrt(q.b1hat^2-(4*q.b2hat*(q.b0hat-yy))))/(2*q.b2hat))

diff<-q.xstarhat-l.xstarhat
max(diff)
#  0.1436761
max.diff<-which(diff>.14363)
#   53 54 55
diff[53]
#  0.1436691
diff[54]
# 0.1436761             #max diff b/w linear & quad cal
diff[55]
# 0.1436391
yy[54]
#  0.03888889          #true ystar value-mid
l.xstarhat[54]
#  5.342041             #true xstar for linear model
q.xstarhat[54]
# 5.485717               #true xstar for quad model
abline(h=0.03888889,lty=4,lwd=1,col=3)
abline(v=5.342041,lty=4,lwd=1,col=2)
abline(v=5.485717,lty=4,lwd=1,col=4)







