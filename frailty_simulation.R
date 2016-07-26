"sim.data"<- function(g,m){
  set.seed(123)
  group <- rep(1:m,rep(g,m))
  Frailty <- rep(rgamma(m,100,1),rep(g,m))
  covariate <- rbinom(g*m,1,.05)
  stimes <-
    rweibull(g*m,1.1,1/(5*Frailty*exp((covariate)/.5)))
  cens <- 5 + 5*runif(25)
  times <- pmin(stimes, cens)
  censored <- as.numeric(cens > times)
  data.mat <-
    cbind(group,covariate,times,censored,Frailty)
  data.mat <-
    data.mat[rev(order(times)),1:length(data.mat[1,])]
  data.fr <- data.frame(data.mat)
  return(data.fr)
}
# ***************************************


# Example of 50 group each with 100 members
sim.fr<-sim.data(50,100)


library(survival)
fit.c <- coxph(Surv(times,censored) ~ covariate,data=
                 sim.fr)
# fit.c gives the Usual cox proportional hazards model
fit.gm.em <- coxph(Surv(times,censored) ~ covariate +
                     frailty(group, dist='gamma', method='em'), data=
                     sim.fr)
# fit.gm.em gives the gamma frailty model by EM
algorithm

fit.c # result of Cox PH model
fit.gm.em # result of gamma frailty model

#### Attempt 2
# http://splinesurv.r-forge.r-project.org/
# install.packages("splinesurv", repos="http://R-Forge.R-project.org")
# http://artax.karlin.mff.cuni.cz/r-help/library/splinesurv/html/sim.sample.html

library(splinesurv)
s <- sim.sample(m = 10, Ji = rep(10,10))
plot(survfit(coxph(Surv(time,delta) ~ Z + frailty(i), data = s$agdata)))
