## variable selection simulation exercise 1
## date: july 13

# initionial setting 
setwd("~/sbr2018")
A <- as.numeric(commandArgs(TRUE))
save_stan <- paste0("stan_data/exe1/stan",A,".rds")
save_output <- paste0("rdsoutput/exe1/output",A,".rds")

#A <- 1
set.seed(A)
num_chain <- 4

N <- 1000
num_cov <- 20
true_b <- c(15,14,13,12,11,
            5,4,3,2,1,
            0.2,0.2,0.1,0.1,0.1,
            0,0,0,0,0)
length(true_b)
a <- 5 
sigma_e <- 1
# covariates data 

#I started with following setting, but I realized that there is no point to make it different. 
# and I need to standardized them. For simplity, 
#mean <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10)
#sd <- rep(c(0.5,0.6,0.7,0.8,0.9),4)

mean <- rep(0, 20)
sd <- rep(5, 20)
x_mat <- matrix(NA, ncol = num_cov, nrow = N)
for(cov in 1:num_cov){
x_mat[,cov] <-  rnorm(N, mean = mean[cov], sd = sd[cov])
}

## outcome
Theta <- as.vector(a + x_mat %*% true_b)
error <- rnorm(N,0,sigma_e)
Y <- Theta + error


## create stan data
stan_data <- list(N = N, Y = Y, num_cov = num_cov, x = x_mat,
                  true_b = true_b
                  , seed = A)
saveRDS(stan_data, file = save_stan)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


fit<- rstan::stan(file= "mod/exe1_mod.stan",data=stan_data,chains = num_chain,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.99, max_treedepth=15))

#make sure to change the file name to save.
saveRDS(fit,file = save_output)



