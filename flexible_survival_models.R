library(rjags) # To enable JAGS within R

JAGSlist <- list("N" = N,         # number of observations
                 "event" = event, # 1 for event, zero for censor
                 "T" = T          # Survival time
)

#Construct the model
my.jags.model <- jags.model("flexibile_survival_models.jags",
                            JAGSlist,n.chains = 1,
                            n.adapt = 100)

my.results = coda.samples(my.jags.model, 
                          c("alpha","beta"), # parameters to monitor
                          n.iter = 10000)

my.results <- my.results[[1]] #To unlist it
summary(my.results)