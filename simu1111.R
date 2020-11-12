#2020.11.11

library(MASS)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#reference setting
set.seed(1234)

K <- 50 # number of covariates
N <- 1000 # number of obs
#save.to <- "ref20201111.rds"

### Truth
alpha <- 3 # intercept
sigma <- 1 #real sigma
beta <- c(rep(c(0,0.2,0.4,0.6,0.8,5,6,7,8,9),2),rep(0,30))
rho <- rep(0.5,25)
block_mat <- matrix(rho,nrow = 5, ncol = 5)
diag(block_mat) <- 1
R <- matrix(0,
            nrow = 50, ncol =50)
for(i in 0:9){
  start <- i*5+1
  end <- i*5+5
  R[start:end,start:end] <- block_mat
}


x <- MASS::mvrnorm(N,mu = rep(0,K), Sigma = R)
error <- rnorm(N,0,sigma)  

y <- x%*%beta + error

stan.data <- list( y = as.vector(y), x = t(x), K = K, N = N)

fit<- rstan::stan(file= "mod/ref_mod.stan",data=stan.data,chains = 4,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.95, max_treedepth=10))

#make sure to change the file name to save.
saveRDS(fit,file = paste0(save.to))
