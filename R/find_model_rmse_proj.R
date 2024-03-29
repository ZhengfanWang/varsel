find_model_rmse_proj <- function(modoutput,modinput){
  numcov <- modinput$numcov
  N <- modinput$N
  proj_y <- proj_pred(modoutput,modinput,NULL)
  KL_matrix <- matrix(NA,ncol = numcov,nrow = numcov)
  rmse <- matrix(NA,ncol = numcov,nrow = numcov)
  KL_store <- rep(NA,N)
  rmse_store <- rep(NA,N)
  for(i in 1:numcov){
    proj_cov_y <- proj_pred(modoutput,modinput,c(1:15)[!(c(1:15) %in% c(i))])
    for(n in 1:N){
      #KL_store[n] <- mean(KL.divergence(proj_y[,n],proj_cov_y[,n],k=5))
     rmse_store[n] <- (mean(proj_y[,n]) - mean(proj_cov_y[,n]))^2
    }
    #KL_matrix[1,i] <- sum(KL_store)
    rmse[1,i] <- sum(rmse_store)
    print(i)
  }
  

  n_proj_cov <- c()
  guess_size <- 16
  for(k in 1:guess_size){
    #min_kl <- min(KL_matrix[1,])
    #sel_cov <- which(KL_matrix[1,] == min_kl)
    min_rmse <- min(rmse[k,],na.rm = T)
    sel_cov <- which(rmse[k,] == min_rmse)
    n_proj_cov <- c(n_proj_cov,sel_cov)
    proj_cov <- c(1:15)[!(c(1:15) %in% n_proj_cov)]
    for(i in proj_cov){
      proj_cov_y <- proj_pred(modoutput,modinput,c(1:15)[!(c(1:15) %in% c(n_proj_cov,i))])
      for(n in 1:N){
       #KL_store[n] <- mean(KL.divergence(proj_y[,n],proj_cov_y[,n],k=5))
       rmse_store[n] <- (mean(proj_y[,n]) - mean(proj_cov_y[,n]))^2
      }
      #KL_matrix[k+1,i] <- sum(KL_store)
      rmse[k+1,i] <- sum(rmse_store)
      print(i)
    }
  }
  res <- list(rmse = round(rmse),cov = n_proj_cov)
  #res <- list(kl = round(KL_matrix),cov = n_proj_cov)
  return(res)
}
