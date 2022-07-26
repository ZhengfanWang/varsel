## load funcs
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

source("0.load_packages.R")
source("0.iso.R")
source("covar_array.R")
set.seed(12345)
#step 1: generate data using function generate_data2()
#true beta <- c( 0.4, -0.2,  0.2, -0.15, 0.15,
#                0.08, 0.06, 0.04,  0.02,    0,
#                   0,    0,    0,     0,    0,
#                   0)
for(i in 1:30){
stan_data <- generate_data2()
#saveRDS(stan_data,paste0("output/ori_dat/dat",i,".rds"))
saveRDS(stan_data,paste0("output/set3/ori_dat/",i,".rds"))
print(i)
}


#step 2: using HSS-VS
num_dummy <- 20
#num_cov <- 16
num_cov <- 16
for(i in 1:30){
  stan_data <- readRDS(paste0("output/set3/ori_dat/",i,".rds"))
  covar_array_with_dummy <- add_dummy(covar_array = stan_data$covar_array, num_dummy = num_dummy)
  #covar_array_without_dummy <- stan_data$covar_array[1:num_cov,,]
  stan_data$covar_array <- covar_array_with_dummy
  #stan_data$covar_array <- covar_array_without_dummy
  stan_data$numcov <- num_cov + num_dummy
  #stan_data$numcov <- num_cov
  saveRDS(stan_data,paste0("output/set3/hss_vs/",i,".rds"))
  print(i)
}



# Step 3: using waic

source("waic_res.R")

# step 4: using proj
source("proj_res.R")

# step 5: using 95 ci
source("95CI_res.R")



fit<- rstan::stan(file= "mod/hs_tau.stan",data=stan_data,chains = 1,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.95, max_treedepth=10))



