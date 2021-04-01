proj_pred <- function(modoutput,modinput,proj_cov){
  num_country <- modinput$numcountry
  year_length <- modinput$yearLength
  covar_array <- modinput$covar_array
  mcmc_array <- rstan::extract(modoutput)
  num_sample <- dim(mcmc_array$beta)[1]
  gamma_c <- mcmc_array$gamma_c
  proj_beta <- mcmc_array$beta
  proj_beta[,proj_cov] <- 0
  proj_pred_ct <- array(NA,c(num_sample,num_country,year_length))
  for(c in 1:num_country){
    for(t in 1:year_length){
      proj_pred_ct[,c,t] <- gamma_c[c] + proj_beta %*% covar_array[,c,t] 
    }
  }
  getc_i <- modinput$getc_i
  gett_i <- modinput$gett_i
  datatype1_i <- modinput$datatype1_i
  datatype2_i <- modinput$datatype2_i
  datatype3_i <- modinput$datatype3_i
  datatype_i <- cbind(datatype1_i,datatype2_i,datatype3_i)
  datatype_i[,2] <- ifelse(datatype_i[,2]==1,2,0)
  datatype_i[,3] <- ifelse(datatype_i[,3]==1,3,0)
  getj_i <- apply(datatype_i,1,sum)
  N <- modinput$N
  sigma_j <- mcmc_array$sigma_j
  proj_pred_y <- matrix(NA,nrow = num_sample,ncol = N)
  for(i in 1:N){
    proj_pred_mu <- proj_pred_ct[,getc_i[i],gett_i[i]]
    sigma_i <-sigma_j[,getj_i[i]]
    proj_pred_y[,i] <- rnorm(num_sample,proj_pred_mu,sigma_i)
    }
  return(proj_pred_y)
}
