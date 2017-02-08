##########################   UNIT ROOT MODEL   ############################################
library(fUnitRoots)
da = read.table('q-gdp4708.txt', header = T)

gdp = log(da[,4])
gdp1 = ts(gdp, frequency = 4, start = c(1947,1))

par(mfrow=c(2,2))
plot(gdp1, xlab ='Year', ylab = 'ln(GDP)')
plot(diff(gdp1))
acf(gdp1, xlab = 'Lag', ylab = 'ACF')
acf(diff(gdp1), type = c("partial"), ylim = c(-0.2,0.3))


m1 = ar(diff(gdp), method = 'mle')
adfTest(gdp,lags= m1$order, type = c("c"))

##P VALUE is 0.4569, therefore cannot reject the unit-root hypothesis 

library(fUnitRoots)

da1 = read.table('d-sp55008.txt', header = T)

lsp500 = log(da1[,7])

lsp5001 = ts(lsp500, frequency = 365, start = c(1950,1,3))

plot(lsp5001)

m2 =ar(diff(lsp5001),method = 'mle', ylim = c(-0.05,0.05))

adfTest(lsp5001,lags = 2,type = c("ct"))


##########################   Random walk with dfift   ###############################

m3 = read.table('m-3m4608.txt', header =T)
lm3 = log(1+m3[,2]) ###monthly log returns
mean(lm3)
stdev(lm3)
ai = lm3 - mean(lm3)  ##white noise series ai

lp3m = cumsum(lm3)  ##monthly log price
aim = cumsum(ai)    ##monthly log price

l3mt = ts(lp3m, frequency = 12, start = c(1946,2))
l3ma = ts(aim, frequency = 12, start = c(1946,2))


plot(l3mt,col = 'red')
lines(l3ma,col='green')
legend("topleft", legend=c("log price series:pt","white noiser series:at"),lty=1,col=c("red","green")) 
