data{
  int<lower=0> N; //number of observations
  int<lower=0> num_cov; //number of preidctors
  real Y[N]; // vector of y
  matrix[N,num_cov] x; 
}

transformed data{
  real slab_scale = 2;
  real nu_lambda = 1; 
  real nu_tau =1;
  real scale_tau = 1; 
  real slab_df = 4;
}

parameters {
  real<lower=0> tau;
  vector<lower=0>[num_cov] lambda;
  real<lower=0> caux;
  vector[num_cov] beta_tilde;
  real alpha;
  real<lower = 0> sigma;
}
transformed parameters {
  vector<lower=0>[num_cov] lambda_tilde;
  real<lower=0> cc;
  vector[num_cov] beta;
  vector[N] theta;

  cc = slab_scale * sqrt(caux);
  lambda_tilde = sqrt(cc^2 * square(lambda) ./ (cc^2 + tau^2 *square(lambda)));
  beta = beta_tilde .* lambda_tilde * tau;
  theta = alpha + x * beta;
}

model {
    beta_tilde ~ normal(0,1);
    lambda ~ student_t(nu_lambda, 0 ,1);
    tau ~ student_t(nu_tau, 0, scale_tau);
    caux ~ inv_gamma(0.5 *slab_df , 0.5 *slab_df);
    alpha ~ normal(0,5);
    Y ~ normal(theta,sigma);
}
