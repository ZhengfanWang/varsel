#2020.10.12

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#reference setting
set.seed(1234)

K <- 20 # number of covariates
N <- 2000 # number of obs
alpha <- 3 # intercept
sigma <- 1 #real sigma
beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          1,2,3,4,5,
          16,17,18,19,20)
save.to <- "ref.rds"
############ comparison setting
#set 1 
N <- 1000
save.to <- "N=1000.rds"

#set 2
N <- 100
save.to <- "N=100.rds"

#set 3
beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          1,2,3,4,5,
          90,92,94,96,98)
save.to <- "median_b1.rds"
save.to <- "median_b2.rds"

#set 4
beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          1,2,3,4,5,
          160,170,180,190,200)
save.to <- "largeb.rds"

#set 5 
beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          rep(100,10))
save.to <- "large10b.rds"

#set 6
sigma <- 5 #real sigma

save.to <- "sigma5.rds"

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

#make sure to change the file name to save.
saveRDS(fit,file = paste0(save.to))
