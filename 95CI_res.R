rdsfiles <- list.files(file.path(paste0(getwd(),"/rdsoutput/set1/proj")), ".rds") #gets names of r files
stanfile <- list.files(file.path(paste0(getwd(),"/output/set1/ori_dat1")), ".rds") #gets names of r files
num_simu <- length(rdsfiles)



num_cov <- 15

out_res <- matrix(NA,nrow = num_simu,ncol = num_cov)
for(sim in 1:num_simu){
  output <- readRDS(paste0("rdsoutput/set1/proj/",sim,".rds"))
  mcmcarray <- rstan::extract(output)
  out_mat <- as.data.frame(t(apply(mcmcarray$beta,2,quantile,c(0.025,0.975))))
  colnames(out_mat) <- c("low","up")
  out_mat <- out_mat %>% mutate(res = ifelse(0<up&0>low, 0,1))
  out_res[sim,] <- out_mat$res
  print(paste0("simu=",sim))
}
sum(out_res[,6:15])/(30*10)

#true_beta <- c( 0.4, -0.2,  0.2, -0.15, 0.15,
#                0.08, 0.06, 0.04,  0.02,    0,
#                   0,    0,    0,     0,    0,
#                   0)

# false exclusion rate 
1- sum(out_res[,1:9])/(30*9)
# false inclusion rate
sum(out_res[,10:16])/(30*7)
# selection accuracy rate
(sum(out_res[,1:9]) + (sum(1-out_res[,10:16])))/(30*16)
0.89

saveRDS(out_res,"ci95_res.rds")
