data{
  int<lower=0> N; //number of observations
  int<lower=0> numcov; //number of preidctors
  int<lower=0> numcountry; // number of countries
  int<lower=0> numregion; // number of regions
  int<lower=0> numsource; // num of source type
  int<lower=0> yearLength; //number of est year
  int<lower=0> ntrain;  //loo 
  int estyears[yearLength]; //vector of est years;
  real Y[N]; // vector of log sbr
  int<lower=0,upper=numcountry> getc_i[N];          // country for given obs
  int<lower=0,upper=numregion> getr_c[numcountry];  // vector of region given country
  int<lower=1,upper=yearLength> gett_i[N];       // time for given obs
  int<lower=0> getitrain_k[ntrain];              // loo or not
  
  //source type indicator variable
  int <lower=0,upper=1> datatype1_i[N];        // admin
  int <lower=0,upper=1> datatype2_i[N];        // hmis 
  int <lower=0,upper=1> datatype3_i[N];        //subnat.lr
  //int <lower=0,upper=1> datatype4_i[N];        //survey
  
  real covar_array[numcov,numcountry,yearLength];   //coariates array: super slow!!!!!!!
    
  //real<lower=0> var_i[N];              // sampling error^2
  
  //input data  spline
  //int<lower=0> K;                  //number of basis
  int<lower=0> H;
  matrix[yearLength,H] Z_th;
}

transformed data{
  //real var_admin = 0.0025;
}

parameters {
  
  vector[numcov] beta_tilde;
  //deviance part
  //real<upper=0> bias_dt;
  
  real<lower=0> sigma_j[numsource];
  
  real<lower=0> sigma_r;
  real<lower=0> sigma_c;
  
  //parameters: P spline 2 order
  real<lower=0,upper=3> tau_delta;      // sd for spline coefficients
  real gamma_w;
  vector[numregion] gamma_r_tilde;
  vector[numcountry] gamma_c_tilde;
  matrix[H,numcountry] delta_hc_tilde;
}

transformed parameters {
  vector[numcov] beta = beta_tilde *5;
  matrix[numcountry,yearLength] mu_ct;
  //real bias_dt_i[N];
  real sigma_i[N];
  matrix[numcountry,yearLength] delta_ct;
  real<lower=0> var_j[numsource];
  vector[numregion] gamma_r;
  vector[numcountry] gamma_c;
  matrix[H,numcountry] delta_hc;
  
  for(j in 1:(numsource)){
    var_j[j]= square(sigma_j[j]);}
  
  //P spline 2 order
  
  for(c in 1:numcountry){
    for(t in 1:yearLength){
      mu_ct[c,t] = to_row_vector(covar_array[,c,t]) * beta;
    }}
  // source type bias
  for(i in 1:N){

    sigma_i[i] = sqrt(var_j[1]*datatype1_i[i]+
                      var_j[2]*datatype2_i[i]+
                      var_j[3]*datatype3_i[i]);
                      //var_j[4]*datatype4_i[i]+
                      //var_i[i]);
  }
  
  for(r in 1:numregion){
    gamma_r[r] = gamma_w + gamma_r_tilde[r] * sigma_r;
  }
  for(c in 1:numcountry){
    gamma_c[c] = gamma_r[getr_c[c]] + gamma_c_tilde[c] * sigma_c;
  }
  
  for(h in 1:H){
    delta_hc[h,] = delta_hc_tilde[h,] * tau_delta;
  }
  
  for(c in 1:numcountry){
    for(t in 1:yearLength){
      delta_ct[c,t] = gamma_c[c]+
        dot_product(Z_th[t,],delta_hc[,c]);
    }}
}

model {
  
  // P spline
  sigma_r ~ normal(0,1);
  sigma_c ~ normal(0,1);
  gamma_w ~ normal(2.5, 2);
  gamma_r_tilde ~ normal(0,1);
  gamma_c_tilde ~ normal(0,1);
  
  
  beta_tilde ~ normal(0,1);   // covariates
  //bias_dt ~ normal(0,5);// source type bias part
  sigma_j ~ normal(0,1);// source type sd trun[0,5] Normal(0,1)
  for(h in 1:H){
    delta_hc_tilde[h,] ~ normal(0,1);
  }
  //main part

  for(k in 1:ntrain){
    Y[getitrain_k[k]] ~ normal(mu_ct[getc_i[getitrain_k[k]],gett_i[getitrain_k[k]]]
                               + delta_ct[getc_i[getitrain_k[k]],gett_i[getitrain_k[k]]]
                               ,
                               sigma_i[getitrain_k[k]]);
  }
}

generated quantities{
  vector[N] log_lik;
  vector[N] prep;
  for (i in 1:N) log_lik[i] = normal_lpdf(Y[i] |mu_ct[getc_i[i],gett_i[i]]
                                          + delta_ct[getc_i[i],gett_i[i]]
                                          , 
                                          sigma_i[i]);
  
  for (i in 1:N) prep[i] = normal_rng(mu_ct[getc_i[i],gett_i[i]]
                                      + delta_ct[getc_i[i],gett_i[i]]
                                      , 
                                      sigma_i[i]);             
  
}
