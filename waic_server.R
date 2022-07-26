
A=as.numeric(commandArgs(TRUE))

setwd("~/varsel")

library(rstan)
library(loo)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


input <- readRDS(paste0("input/standata/dat",A,".rds"))
numcov <- input$numcov

elpd_mat <- matrix(NA, ncol = numcov, nrow = numcov)
for(i in 1:numcov){
  stan_data <- input
  stan_data$covar_array <- input$covar_array[i,,]
  stan_data$numcov <- 1
  fit<- rstan::stan(file= "mod/onecovbase.stan",data=stan_data,chains = 4,
                    warmup = 1000, iter = 2000,
                    pars = c("log_lik","prep"),
                    control=list(adapt_delta=0.95, max_treedepth=10))
  log_lik <- extract_log_lik(fit)
  loo <- loo(log_lik, save_psis = TRUE, cores = 8)
  elpd_mat[1,i] <- loo$elpd_loo
}

n_sel_cov <- c()
guess_size <- 16

for(k in 1:guess_size){
  max_elpd <- max(elpd_mat[k,],na.rm = T)
  sel_cov <- which(elpd_mat[k,] == max_elpd)
  n_sel_cov <- c(n_sel_cov,sel_cov)
  sel_cov <- c(1:16)[!(c(1:16) %in% n_sel_cov)]
  for(i in sel_cov){
    stan_data <- input
    stan_data$covar_array <- input$covar_array[c(n_sel_cov,i),,]
    stan_data$numcov <- k
    fit<- rstan::stan(file= "mod/base.stan",data=stan_data,chains = 4,
                      warmup = 1000, iter = 2000,
                      pars = c("log_lik","prep"),
                      control=list(adapt_delta=0.95, max_treedepth=10))
    log_lik <- extract_log_lik(fit)
    loo <- loo(log_lik, save_psis = TRUE, cores = 8)
    elpd_mat[k+1,i] <- loo$elpd_loo
  }
}
res <- list(elpd_mat,n_sel_cov)
saveRDS(res,paste0("rdsoutput/waic/",A,".rds"))

