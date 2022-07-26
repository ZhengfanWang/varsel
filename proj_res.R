rdsfiles <- list.files(file.path(paste0(getwd(),"/rdsoutput/ori")), ".rds") #gets names of r files
stanfile <- list.files(file.path(paste0(getwd(),"/output/ori_dat")), ".rds") #gets names of r files
rdsfiles
stanfile
num_simu <- length(rdsfiles)


# after fitting the model with all the variables we proceed to the variable selection. As a search heuristic, 
# we use forward searching, that is, starting from the empty model, we add variables one at a time, each time 
# choosing the variable that decreases the KL-divergence(increase the elpd) the most
rmse_list <- list()
cov_list <- list()
for(sim in 1:num_simu){
  output <- readRDS(paste0("rdsoutput/ori/ori",sim,".rds"))
  input <- readRDS(paste0("output/ori_dat/dat",sim,".rds"))
  res <- find_model_rmse_proj(output,input)
  rmse_list[[sim]] <- res$rmse 
  cov_list[[sim]] <- res$cov
  print(paste0("simu=",sim))
}
saveRDS(rmse_list,"proj_res_mat.rds")
saveRDS(cov_list,"proj_res_cov.rds")

cov <- c()
for(i in 1:num_simu){
  cov <- rbind(cov,proj_res_cov[[i]])
}
cov <- cbind(cov,rep(17,length = 30))
1- sum(cov[,1:9] %in% c(1:9))/(30*9)
1- sum(cov[,10:15] %in% c(10:15))/(30*6)
(sum(cov[,1:9] %in% c(1:9)) + sum(cov[,10:15] %in% c(10:15)))/(30*15)

# ## PLOTs
# size <- factor(c(rep(1,15),rep(2,15),rep(3,15),rep(4,15),rep(5,15),rep(6,15)),levels = 6:1)
# names <- "cov1"
# for(i in 2:15){
#   names <- c(names,paste0("cov",i))}
# cov <- factor(rep(names,6),levels = names)
# rmse1 <- as.vector(t(rmse_list[[1]]))[1:90] 
# rmse_sum <- data.frame(size=size, cov = cov,rmse =rmse1)
# library(ggplot2)
# ggplot(rmse_sum, aes_(x = ~cov, y = ~size)) +
#   geom_tile(aes_(fill = ~rmse, color = ~rmse),
#             width = 1, height = 0.9, size = 1) +
#   geom_text(aes_(label = ~round(rmse), fontface = ~rmse+1)) +
#   labs(y = "Model size", x = "variable",
#        title = "Forward proj Result") +
#   coord_cartesian(expand = FALSE) +
#   theme(legend.position = "none",
#         axis.text.y = element_text(angle = 45))


proj_res_mat[[1]]
proj_res_cov[[1]]
proj_res_rmse <- matrix(NA,ncol = 16, nrow = num_simu)
for(i in 1:num_simu){
for(k in 1:16){
  cov <- proj_res_cov[[i]][k]
  proj_res_rmse[i,k] <- proj_res_mat[[i]][k,cov]
  }
}

#coverage rate for proj
prep <- list()
y <- list()
for(sim in 1:num_simu){
  output <- readRDS(paste0("rdsoutput/ori/ori",sim,".rds"))
  input <- readRDS(paste0("output/ori_dat/dat",sim,".rds"))
  output <- rstan::extract(output)
  prep[[sim]] <- output$prep
  y[[sim]] <- input$Y
  print(paste0("simu=",sim))
}

cov_mat <- matrix(NA, ncol = 2000, nrow = 30)
err_mat <- matrix(NA, ncol = 2000, nrow = 30)
for(i in 1:num_simu){
bound <- apply(prep[[i]], 2, quantile, c(0.025,0.5,0.975))
cov_mat[i,] <- (y[[i]] > bound[1,]) * (y[[i]] < bound[3,])
err_mat[i,] <- y[[i]] - bound[2,]
}
mean(cov_mat)
sum(err_mat^2)/30
sum(abs(err_mat))/30

#coverage rate for hss
prep <- list()
y <- list()
for(sim in 1:num_simu){
  output <- readRDS(paste0("rdsoutput/hss/hss",sim,".rds"))
  input <- readRDS(paste0("output/ori_dat/dat",sim,".rds"))
  output <- rstan::extract(output)
  prep[[sim]] <- output$prep
  y[[sim]] <- input$Y
  print(paste0("simu=",sim))
}

cov_mat <- matrix(NA, ncol = 2000, nrow = 30)
err_mat <- matrix(NA, ncol = 2000, nrow = 30)
for(i in 1:num_simu){
  bound <- apply(prep[[i]], 2, quantile, c(0.025,0.5,0.975))
  cov_mat[i,] <- (y[[i]] > bound[1,]) * (y[[i]] < bound[3,])
  err_mat[i,] <- y[[i]] - bound[2,]
}
mean(cov_mat)
sum(err_mat^2)/30
sum(abs(err_mat))/30

for(i in 1:num_simu){
  stan_data <- readRDS(paste0("output/ori_dat/dat",i,".rds"))
  select_data1 <- select_cov(stan_data,proj_res_cov[[i]][1:6])
  saveRDS(select_data1, paste0("output/waic_select/dat",i,".rds"))
}
