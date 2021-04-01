set.seed(123)
sbr_data <- generate_data()
cov_vec <- c(1:15)[!(c(1:15) %in% c(1,2,3,4,5))]
for(i in cov_vec){
  fitdata <- select_cov(sbr_data,c(1,2,3,4,5,i))
  saveRDS(fitdata,paste0("data/waic12345/fitdata",i,".rds"))
}

fit<- rstan::stan(file= "mod/base.stan",data=fitdata,chains = 1,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.95, max_treedepth=10))
