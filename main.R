## load funcs
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

source("0.load_packages.R")

stan_data <- generate_data()
saveRDS(stan_data,"data/sample.rds")
fit<- rstan::stan(file= "mod/hs_tau.stan",data=stan_data,chains = 1,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.95, max_treedepth=10))



