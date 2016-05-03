#file://retnlso-nt0002/RIPOD1$/Desktop/survival_analysis.html
#http://www.petrkeil.com/?p=2425
seedlings <- read.table("http://goo.gl/chMvEo", header=TRUE)
sd = read_csv("seedlings.csv")

sd$status = 1*(sd$death > 0)

library(survival)

mp = survreg(Surv(sd$death) ~ 1, dist = "exponential" )
mp

mu = exp(mp$coefficients)

time = 0:25
S_t = exp(-time / mu)
f_t = exp(-time / mu) / mu

plot(1:length(S_t), S_t, type = "l")
plot(1:length(f_t), f_t, type = "l")


deaths = tapply(X = sd$status, INDEX = sd$death, FUN = sum)
# sd %>% group_by(death) %>% summarise(sum(status))
survs = ((sum(deaths) - cumsum(deaths)) / sum(deaths))
death.data <- data.frame(day=as.numeric(names(survs)), 
                         survs=as.numeric(survs))


par(mfrow=c(1,2))
plot(death.data, pch=19, ylab="S(t)", xlab="Weeks",
     main="Survival")
lines(time, S_t, col="red")
hist(sd$death, freq=FALSE, main="Failure density",
     xlab="Weeks", ylim=c(0,0.15))
lines(time, f_t, col="red")
par(mfrow=c(1,1))



library(runjags)
library(coda)

# Prediction interval
new_t = seq(0, 25, by = 0.5)

# JAGS Data List
surv_data = list(t_to_death = sd$death,
                 N = nrow(sd),
                 new_t = new_t,
                 new_N = length(new_t))

cat("
model
{
  # prior
  lambda ~ dgamma(0.01, 0.01)
  
  # likelihood
  for (t in 1:N)
  {
    t_to_death[t] ~ dexp(lambda)
  }
  
  # mean time to death
  mu = 1 / lambda
  
  # predictions
  for(i in 1:new_N)
  {
    S_t[i] <- exp(-new_t[i] / mu)
  }
}
", file = "survival_exp.jags"
)

mu = run.jags(data = surv_data,
              model = "survival_exp.jags",
              monitor = c("mu"),
              sample = 1e3, burnin = 1e3, n.chains = 3)


densplot(as.mcmc(mu))

S_t = run.jags(data = surv_data,
               model = "survival_exp.jags",
               monitor = c("S_t"),
               sample = 2e3, burnin = 1e3, n.chains = 3)

S_t = summary(S_t)



plot(death.data, pch=19, xlab="Weeks", ylab="S(t)",
     main="Survival", ylim=c(0,1))
lines(new_t, S_t[,'Lower95'], lty=2, col="red")
lines(new_t, S_t[,'Median'], col="red")
lines(new_t, S_t[,'Upper95'], lty=2, col="red")
legend("topright", legend=c("Median prediction","95 perc. prediction"), 
       lty=c(1,2), col=c("red", "red"))
