##########################   AR MODEL   ############################################

##ACF analysis for IBM daily returns
df = read.csv("table.csv", header = T)
df[1,5]

#Get Returns from ClosePrice
for (i in 1:11741){
  df[i+1,8] = (df[i+1,5]-df[i,5])/df[i,5]
}

#Get simple returns
sibm = df[2:11742,8]

 #acf test
acf(sibm, type = c("correlation"),ylim = c(-0.05,0.05))
 ##Daily returns of IBM do not have significant serial correlations, indicating that rhos are
 ##not significantly different from zero at 5% level

 #portmanteau test
Box.test(sibm,lag = 5,type =c("Ljung-Box"))
 ##p-value = 0.93 > significance level 0.05, therefore we can not reject null hypothesis
 ##correlations are equal to zero(returns are uncorrelated)

#Get log return
libm = log(1+sibm)
 
 ##Test
acf(libm,type = c("correlation"),ylim=c(-0.05,0.05))
Box.test(libm,lag = 20, type = c("Ljung-Box"))
 ##p-value=0.7143 > 0.05, we can not reject null hypothesis, therefore returns have no 
 ##significant serial correlations

#Situations might introduce autocorrelations in the observed return series:
#           1.High frequency data            2.Value weighted index

##Autocorrelation analysis for High-frequency data 

da = read.csv("IF00C1.csv", header = T) #load raw data
da[1,]
da[1,7]

#Get returns from price
for (i in 1:65563){
  da[i+1,8] = (da[i+1,7] - da[i,7])/da[i,7]
}

#Get simple returns 
sIC00C1 = da[2:65564,8]

 #Test single autocorrelation
acf(sIC00C1,lag.max = NULL,type = c("correlation"),ylim = c(-0.04,0.04))
 ##Graph shows that some sample ACFs are more than their standard error limits(dashed lines),
 ##indicating that serial correlations (especially 1 lag) of minute-level IC00C1 returns exit
 ##and that they are significantly different from zero at 5% level.  

 #Portmanteau Test
Box.test(sIC00C1, lag = 5, type = c("Ljung-Box"),fitdf = 0)
 ##Reject H0:correlation(1~m) = 0, so for some i that belongs to 1~m, correlations 
 ##of returns are not 0, that is IC00C1 returns have significant serial correlations

#Get log returns
lIC00C1 = log(1+sIC00C1)

 #Test Single Autocorrelation
acf(lIC00C1,type = c("correlation"), ylim = c(-0.04,0.04))
 #Portmanteau Test
Box.test(lIC00C1,lag = 5,type =c("Ljung-Box"))
 ##Reject zero correlation hypothesis, IC00C1 returns in minute level are significantly 
 ##autocorrelated/serial correlated.


############################################################################################


#load data into a vector or a list
gnp = scan(file='dgnp82.txt')

#create time-series objects, add time vector to decribe numbers
gnp1 = ts(gnp, frequency = 4, start = c(1947,2))

##draw picture from time-series data
plot(gnp1)
points(gnp1,pch = '*')##add pionts to top and bottom

#1.fit autoregressive models to time series

##Method 1

m1 = ar(gnp,method = "mle")
m1$order
m1$ar
m1$x.mean
m1$var.pred ##variance of a4

##AR(3) Model is selected based on AIC, and the estimated coefficients for the fitted model is
##x4-0.00774 = 0.348(x3-0.00774) + 0.179(x2-0.00774) + (-0.142)(x1-0.00774) + a4

##Method 2  ##intercept denotes the mean of the series

m2 = arima(gnp,order = c(3,0,0))
m2
##The fitted AR(3) moderl is  
##x4-0.0077 = 0.3480(x3-0.0077) + 0.1793(x2-0.0077) + (-0.1423)(x1-0.0077) + a4

#In other version of AR model, r4 = theta0 + 0.3480*r3 + 0.1793*r2 +(-0.1423)*r1 + a4

theta0 = 0.0077 *(1-0.3480-0.1793+0.1423)
##The fitted AR(3) model is 
## x4 = 0.0047 + 0.3480*x3 + 0.1793*x2 + (-0.1423)*x1 +a4, a4~(0,0.009709)


#2.get characteristic equation
p1 = c(1,-m2$coef[1:3])

roots = polyroot(p1)
#get the roots of 0=1-0.348B-0.179B^2 +0.142B^3
#1.real number root shows an exponentially decaying correlations of AR Model
#2.complex characteristic roots indicate that the plot of ACF of rt would show a picture 
#of damping sine and cosine waves, which give rise to the behavior of business cycles.

Mod(roots) #Get absolute values
k = 2*pi/acos(1.590253/1.913308)
#average length of the stochastic cycles is 10.65 quarters, which is about 2.66 years.

############################################################################################

vw = read.table('m-ibm3dx2608.txt',header = T)[,3]

m3 = arima(vw,order = c(3,0,0))

#AR(3) Model is x4-0.0089 = 0.1158(x3-0.0089) + (-0.0187)(x2-0.0089) +(-0.1042)*(x1-0.0089)
# OR x4 = 0.1158*x3-0.0187*x2-0.1042*x1 + intercept + at

#Compute intercept and std of residuals
sum(c(1,-m3$coef[1:3]))*(m3$coef[4])
sqrt(m3$sigma2)

Box.test(m3$residuals,lag = 12,type=c("Ljung-Box"))  #12 degrees
pv = 1- pchisq(16.35,9) #compute p-value using 9 degrees


#fix AR(2) coef parameter values,NA denotes estimation and 0 means fixing the parameter to 0
m3 = arima(x=vw, order = c(3,0,0), fixed = c(NA,0,NA,NA)) 

#########################################Code##################################

#acf(sibm, type = c("correlation"),ylim = c(-0.05,0.05))
#Box.test(sibm,lag = 5,type =c("Ljung-Box"))
#gnp = scan(file='dgnp82.txt')
#gnp1 = ts(gnp, frequency = 4, start = c(1947,2))
#m1 = ar(gnp,method = "mle")
#m2 = arima(gnp,order = c(3,0,0))