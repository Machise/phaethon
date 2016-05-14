## Data Generation By Model


N = 100
shape = 1   
scale = 300 # replace with Linear Model exp(- (beta_0 + beta_1 x_1 + ...) )

t_obs = rweibull(N, shape = shape, scale = scale)

lfe = list(N = N, t_obs = l_obs)

m1 = "
data {
  int<lower=0>  N;
real<lower=0> t_obs[N];
}

parameters {
real<lower=0> shape;
real<lower=0> scale;
}

model {
shape ~ cauchy(0,2);
scale ~ cauchy(0,2);
t_obs ~ weibull(shape, scale);
}
"

m1f = stan(model_code = sm1, data = lfe, cores = 3, chains = 2, iter = 1e4, warmup = 1e3)


#### Attempt 2 ####
# Adding noise

N = 100
shape = 1   
# replace with Linear Model exp(- (beta_0 + beta_1 x_1 + ...) )
t_avgfail = rnorm(N, 300, 1.5)
scale  = t_avgfail

t_obs = rweibull(N, shape = shape, scale = scale)

lfe = list(N = N, t_obs = t_obs)

m2 = "
data {
int<lower=0>  N;
real<lower=0> t_obs[N];
}

parameters {
real<lower=0> shape;
real<lower=0> scale;
}

model {
shape ~ cauchy(0,2);
scale ~ cauchy(0,2);
t_obs ~ weibull(shape, scale);
}
"

m2f = stan(model_code = m2, data = lfe, cores = 3, chains = 2, iter = 1e5, warmup = 1e3, control=list(adapt_delta=0.99))
print(m2f)




#### Attempt 3 ####
# Linear Model with Indicator Function

N = 1000
w_shape = 1   
# replace with Linear Model exp(- (beta_0 + beta_1 x_1 + ...) )
beta0 = 300 # Global Intercept
beta1 = -100  # Average Number of Days Decrease Per Alarm Count
x1 = rbinom(N, 1, 0.5) # With Alarm, or Without


# t_avgfail = rnorm(N, 300, 1.5)
w_scale  = beta0 + beta1 * x1

t_obs = rweibull(N, shape = w_shape, scale = exp(-w_scale))

lfe = list(N = N, t_obs = t_obs, x1 = x1)

m3 = "
data {
int<lower=0>  N;
real<lower=0> t_obs[N];
int x1[N];
}

parameters {
real<lower=0> shape;
//real<lower=0> scale;
real beta0;
real beta1;

}

model {
shape ~ cauchy(0,2);
beta0 ~ cauchy(0,2);
beta1 ~ cauchy(0,2);
for(i in 1:N){
  t_obs[i] ~ weibull(shape, exp( -(beta0 + beta1 * x1[i])));
}

}
"

m3f = stan(model_code = m3, data = lfe, cores = 3, chains = 2, iter = 1e4, warmup = 1e3, control=list(adapt_delta=0.99))
print(m3f)



#### Attempt 4 ####
# Linear Model with Count Function and Boolean Fatal Alarm

N = 1000
w_shape = 6
# replace with Linear Model exp(- (beta_0 + beta_1 x_1 + ...) )
beta0 = 300 # Average Number of Days until Failure
beta1 = -10  # Average Number of Days Decrease per Alarm Count
x1 = rpois(N, 10) # Set the average number of alarms
beta2 = -100  # Average Number of Days Decrease per Fatal Alarm
x2 = rbinom(N, 1, 0.05) # Fatal Alarm (Boolean)




# t_avgfail = rnorm(N, 300, 1.5)
w_scale  = beta0 + beta1 * x1 + beta2 * x2

t_obs = rweibull(N, shape = w_shape, scale = exp(-w_scale))
## Make sure this is greater than 0 for all values, call error otherwise

nl_t_obs = -log(t_obs)

lfe = list(N = N, t_obs = t_obs, x1 = x1, x2 = x2)

m4 = "
data {
int<lower=0>  N;
real<lower=0> t_obs[N];
int x1[N];
int x2[N];
}

parameters {
real<lower=0> shape;
//real<lower=0> scale;
real beta0;
real beta1;
real beta2;
}

model {
shape ~ cauchy(0,2);
beta0 ~ cauchy(0,2);
beta1 ~ cauchy(0,2);
beta2 ~ cauchy(0,2);
for(i in 1:N){
t_obs[i] ~ weibull(shape, exp( -(beta0 + beta1 * x1[i] + beta2 * x2[i] )));
}

}
"

m4f = stan(model_code = m4, data = lfe, cores = 7, chains = 2, iter = 1e4, warmup = 1e3, control=list(adapt_delta=0.99))
print(m4f)


plot(density(-log(t_obs)))
