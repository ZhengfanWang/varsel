#2020.10.20

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


K <- 20 # number of covariates
N <- 2000 # number of obs
alpha <- 3 # intercept
sigma <- 1 #real sigma
beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          15,15,15,15,15,
          16,17,18,19,20)
NM <- 30 # number of runs

for(i in 19:NM){
#reference setting
set.seed(i)
  ### Truth
  x <- matrix(NA,ncol = K, nrow = N) 
  error <- y <- rep(NA,length = N)
  
  for(n in 1:N){
    for(k in 1:K){
      x[n,k] <- rnorm(1,0,1) }
    error[n] <- rnorm(1,0,sigma)  
  }
  y <- x%*%beta +error
  
  stan.data <- list( y = as.vector(y), x = t(x), K = K, N = N)
  
  fit<- rstan::stan(file= "mod/ref_mod.stan",data=stan.data,chains = 4,
                    warmup = 1000, iter = 2000,
                    control=list(adapt_delta=0.95, max_treedepth=10))
  
  save.to <- paste0("rdsoutput/set",i,".rds")
  saveRDS(fit,file = paste0(save.to))
}

