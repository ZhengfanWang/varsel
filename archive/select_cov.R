library(extraDistr)
library(rstan)
lambda3 <- readRDS("rdsoutput/new/regHS_nval_res202000311.rds")
lambda.array <- rstan::extract(lambda3)
dim(lambda.array$lambda)
print(lambda3,pars = c("bias_dt","sigma_j","beta"))

select_covar <- function(model,cov,niter = 4000){
  priors <- rhcauchy(niter, sigma = 1)
  #priors <- rht(niter, nu = 3, sigma = 1)
  s.priors <- sort(priors)
  posts <- model[,cov]
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}

prob <- c()
for(i in 1:16){
  prob[i] <- select_covar(lambda.array$lambda,i,niter = 12000)
}
round(prob,digits = 3)

