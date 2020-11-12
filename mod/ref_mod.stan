data {
  int<lower=1> N; // Number of data
  int<lower=1> K; // Number of covariates
  matrix[K , N] x;
  real y[N];
}

parameters {
  vector[K] beta_tilde;
  vector<lower=0>[K] lambda;
  real<lower=0> tau_tilde;
  real alpha;
  real<lower=0> sigma;
}

transformed parameters{
vector[K] beta = beta_tilde .* lambda * sigma * tau_tilde;  
}
model {
  // tau ~ cauchy(0, sigma)
  // beta ~ normal(0, tau * lambda)
  beta_tilde ~ normal(0, 1);
  lambda ~ cauchy(0, 1);
  tau_tilde ~ cauchy(0, 1);
  
  alpha ~ normal(0, 2);
  sigma ~ normal(0, 2);
  
  y ~ normal(x' * beta + alpha, sigma);
}