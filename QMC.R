#MC method using Pseudo random sequence 

# Example: > AsianC_QMC(50,50,0.01,2,0.4,24,50000,Type = "Sobol",Plot=TRUE)
# [1] 6.838347

library(gsl)
library(randtoolbox)

Pseudo = function(NSteps,NRepl)
{
  mat <- matrix(runif(NSteps * NRepl),ncol = NSteps)
  qnorm(mat)
}
#QMC method using Halton random sequence 
Halton = function(NSteps,NRepl)
{
  matrix(qnorm(runif.halton(NSteps * NRepl)),ncol = NSteps)
}

#QMC method using Sobol random sequence 
Sobol = function(NSteps,NRepl)
{
  q <- qrng_alloc(type="sobol", NSteps)
  rt <- qrng_get(q,NRepl)
  qnorm(rt)
}

#AsianC_MC2(50,50,0.01,2,0.4,24,50000)
AsianC_QMC = function(S0,K,mu,T,sigma,NSteps,NRepl,Type=c("Pseudo","Sobol","Halton"),Plot=FALSE)
{
payoff <- vector(mode = "numeric",length = NRepl)
dt = T/NSteps
drift = (mu-0.5*sigma^2)*dt
vari = sigma*sqrt(dt)

if (Type == "Halton")
{
  Increments = drift + vari* t(Halton(NSteps,NRepl))
}
else if (Type == "Sobol")
{
  Increments = drift + vari* t(Sobol(NSteps,NRepl))
}
else
{
  Increments = drift + vari* t(Pseudo(NSteps,NRepl))
}

#Increments = drift + vari* t(Pseudo(NSteps,NRepl))
  #t(Pseudo(NSteps,NRepl))
  #t(Sobol(NSteps,NRepl))
  #t(mat)
  #t(Halton(NSteps,NRepl))

for(i in 1:NRepl)
{
  payoff[i] <- max(mean(S0*exp(cumsum(Increments[,i])))-K,0) * exp(-mu*T)
}

#plot 
if (Plot==TRUE){
  
  est_ar <- cumsum(payoff)/(1:length(payoff))
  #standard deviation 
  esterr_ar <- sqrt(cumsum((payoff-est_ar)^2))/(1:length(payoff))
  
  plot(est_ar,type="l",ylim=mean(payoff)+20*c(-esterr_ar[NRepl],esterr_ar[NRepl]))
  #confidence interval 
  lines(est_ar+2*esterr_ar,col="gold",lwd=2)  # mean + 2*sigma 
  lines(est_ar-2*esterr_ar,col="gold",lwd=2)  # mean - 2*sigma 
}


mean(payoff)
}
