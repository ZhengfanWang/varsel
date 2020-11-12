get_output <- function(model){
  lambda <- array(NA, dim = c(cov_for_each, num_simu, niter))
  beta_hat_s <- array(NA, dim = c(cov_for_each, num_simu, niter))
  tau <- matrix(NA,ncol = niter, nrow = num_simu)
  
  for(i in 1:num_simu){
    tau[i,] <- simu[[i]]$tau 
    for(j in 1:cov_for_each){
      lambda[j,i,] <- simu[[i]]$lambda[,j]
      beta_hat_s[j,i,]<- simu[[i]]$beta[,j]
    }
  }
  return(list(tau=tau,lambda=lambda,beta_hat_s=beta_hat_s))
}