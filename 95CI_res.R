rdsfiles <- list.files(file.path(paste0(getwd(),"/rdsoutput/proj")), ".rds") #gets names of r files
stanfile <- list.files(file.path(paste0(getwd(),"/data/proj")), ".rds") #gets names of r files
num_simu <- length(rdsfiles)


# after fitting the model with all the variables we proceed to the variable selection. As a search heuristic, 
# we use forward searching, that is, starting from the empty model, we add variables one at a time, each time 
# choosing the variable that decreases the KL-divergence(increase the elpd) the most
out_res <- matrix(NA,nrow = num_simu,ncol = 15)
for(sim in 1:num_simu){
  output <- readRDS(paste0("rdsoutput/proj/hsoutput",sim,".rds"))
  mcmcarray <- rstan::extract(output)
  out_mat <- as.data.frame(t(apply(mcmcarray$beta,2,quantile,c(0.025,0.975))))
  colnames(out_mat) <- c("low","up")
  out_mat <- out_mat %>% mutate(res = ifelse(0<up&0>low, 0,1))
  out_res[sim,] <- out_mat$res
  print(paste0("simu=",sim))
}
out_res

sum(out_res[,1:5])/(30*5)
sum(out_res[,6:15])/(30*10)
sum(out_res[,6:15])/(30*15)
