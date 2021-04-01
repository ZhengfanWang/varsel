A=as.numeric(commandArgs(TRUE))

setwd("~/varsel")
cov_vec <- c(1:15)[!(c(1:15) %in% c(1,4))]

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_data <- readRDS(paste0("input/waic14/fitdata",cov_vec[A],".rds"))
fitbase<- rstan::stan(file= "mod/base.stan",data=stan_data,chains = 4,
                  warmup = 1000, iter = 2000,
                  pars = c("beta","tau_delta","prep","log_lik","sigma_c","sigma_r","gamma_c","sigma_j"),
                  control=list(adapt_delta=0.95, max_treedepth=10))
saveRDS(fitbase,paste0("rdsoutput/boutput",cov_vec[A],".rds"))


