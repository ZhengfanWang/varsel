num_simu <- 30

cal_pit <- function(stan_data_val, model){
  getitest <- stan_data$getitrain_k
  #getitest <- setdiff(seq(1,stan_data_val$N), stan_data_val$getitrain_k)
  ntest <- length(getitest)
  # PIT
  pit.j <- rep(NA, ntest)
  for (j in 1:ntest){
    i <- getitest[j]
    yrepi.s <- model$prep[,i]
    pit.j[j] <- mean(yrepi.s <= stan_data_val$Y[i])
  } 
  # coverage follows from pit
  c(mean(pit.j < 0.05), # % below 5% PI
    mean(pit.j < 0.1), # % below 10% PI
    mean(pit.j > 0.9), # % above 95% PI 
    mean(pit.j > 0.95)) # % above 90% PI 
}

summ_func <- function(){
  files <- list.files(file.path(paste0(getwd(),"/rdsoutput/set1/proj/")), ".rds") #gets names of rds files
  num_simu <- length(files)
  res <- matrix(ncol = 4, nrow = num_simu)
  err_mat <- matrix(ncol = 2000, nrow = num_simu)
for(i in 1:num_simu){
  stan_data <- readRDS(paste0("output/set1/ori_dat1/",files[i]))
  fit <- readRDS(paste0("rdsoutput/set1/proj/",files[i]))
  fit <- rstan::extract(fit)
  prep <- apply(fit$prep,2,median)
  err_mat[i,] <- prep - stan_data$Y 
  res[i,(1:4)] <- cal_pit(stan_data, fit)
}
  return(list <- list(res,err_mat))
}

list <- summ_func()
apply(list[[1]],2,mean)
mean(list[[2]])
mean(abs(list[[2]]))

saveRDS(list,"hss_res.rds")
saveRDS(list,"proj_res.rds")
saveRDS(list,"waic_res.rds")
saveRDS(list,"v2_95_res.rds")
saveRDS(list,"v2_max_res.rds")
saveRDS(list,"ci95_res.rds")
####################################
error <- c()
method <- c()
model <- c()
for(i in 1:20){
error <- c(error,proj_res[[2]][i,],waic_res[[2]][i,],v2_95_res[[2]][i,],v2_max_res[[2]][i,],ci95_res[[2]][i,])
method <- c(method,rep("proj",2000),rep("elpd",2000),rep("HSS95",2000),rep("HSSmax",2000),rep("95ci",2000))
model <- c(model,rep(i,10000))
}
df <- data.frame(error = error, method = method, model = model)
library(ggplot2)
library(tidyverse)
df %>% filter(model <=16) %>% 
ggplot( aes(y = error))+
  geom_boxplot(aes(x = method))+
  facet_wrap(~model) +
  coord_cartesian(y = c(-1,1))

df %>% group_by(method) %>% 
  summarise(mean_error = mean(error),
            abs_error = mean(abs(error)),
            mse = mean(error^2))
apply(proj_res[[1]],2,mean)
apply(waic_res[[1]],2,mean)
apply(v2_95_res[[1]],2,mean)
apply(ci95_res[[1]],2,mean)

#########################

