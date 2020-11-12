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

select_covar <- function(model,cov,niter = 4000){
  set.seed(1234)
  priors <- rhcauchy(niter, sigma = 1)
  s.priors <- sort(priors)
  posts <- model[cov,]
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}



cal_prob <- function(simu_res,num_simu){
  prob <- matrix(NA,ncol=cov_for_each,nrow = num_simu)
  for(i in 1:num_simu){
    for(j in 1:cov_for_each){
      prob[i,j] <- select_covar(simu_res$lambda[,i,],j)    
    }
  }
  return(prob)
}

select_covar_lam_tau <- function(lambda,cov,tau,niter = 4000){
  set.seed(1234)
  priors.lam <- rhcauchy(niter, sigma = 1)
  priors.tau <- rhcauchy(niter,sigma = 1)
  s.priors <- sort(priors.lam*priors.tau)
  posts <- lambda[cov,]*tau
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}


cal_prob_lam_tau <- function(simu_res,num_simu){
  prob <- matrix(NA,ncol=cov_for_each,nrow = num_simu)
  for(i in 1:num_simu){
    for(j in 1:cov_for_each){
      prob[i,j] <- select_covar_lam_tau(simu_res$lambda[,i,],j,simu_res$tau[i,])    
    }
  }
  return(prob)
}

select_wasserstein <- function(lambda,cov,tau,niter = 4000){
  set.seed(1234)
  priors.lam <- rhcauchy(niter, sigma = 1)
  priors.tau <- rhcauchy(niter,sigma = 1)
  s.priors <- sort(priors.lam*priors.tau)
  posts <- lambda[cov,]*tau
  wasserstein1d(s.priors,posts)
  #wasserstein1d(priors.lam,lambda[cov,])
}


cal_wasserstein <- function(simu_res,num_simu){
  prob <- matrix(NA,ncol=cov_for_each,nrow = num_simu)
  for(i in 1:num_simu){
    for(j in 1:cov_for_each){
      prob[i,j] <- select_wasserstein(simu_res$lambda[,i,],j,simu_res$tau[i,])    
    }
  }
  return(prob)
}



acc_vs_threshold_plot <- function(mat,cov,truth = tru){
  decis <- sapply(mat[,cov],decision,seq(0,1,0.05))
  apply(decis == truth[cov],1,mean)
} 


get_accuracy_tab <- function(prob_mat){
  accuracy <- matrix(NA,nrow = cov_for_each, ncol = length(seq(0,1,0.05)))
  for(cov in 1:cov_for_each){
    accuracy[cov,] <- acc_vs_threshold_plot(prob_mat,cov)
  }
  colnames(accuracy) <- seq(0,1,0.05)
  return(accuracy)
}
