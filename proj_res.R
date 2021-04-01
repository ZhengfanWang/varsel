rdsfiles <- list.files(file.path(paste0(getwd(),"/rdsoutput/proj")), ".rds") #gets names of r files
stanfile <- list.files(file.path(paste0(getwd(),"/data/proj")), ".rds") #gets names of r files
rdsfiles
stanfile
num_simu <- length(rdsfiles)


# after fitting the model with all the variables we proceed to the variable selection. As a search heuristic, 
# we use forward searching, that is, starting from the empty model, we add variables one at a time, each time 
# choosing the variable that decreases the KL-divergence(increase the elpd) the most
rmse_list <- list()
cov_list <- list()
for(sim in 24:num_simu){
  output <- readRDS(paste0("rdsoutput/proj/hsoutput",sim,".rds"))
  input <- readRDS(paste0("data/proj/sbr",sim,".rds"))
  res <- find_model_rmse_proj(output,input)
  rmse_list[[sim]] <- res$rmse 
  cov_list[[sim]] <- res$cov
  print(paste0("simu=",sim))
}
saveRDS(rmse_list,"proj_res_mat.rds")
saveRDS(cov_list,"proj_res_cov.rds")

cov_list
rmse_list


## PLOTs
size <- factor(c(rep(1,15),rep(2,15),rep(3,15),rep(4,15),rep(5,15),rep(6,15)),levels = 6:1)
names <- "cov1"
for(i in 2:15){
  names <- c(names,paste0("cov",i))}
cov <- factor(rep(names,6),levels = names)
rmse1 <- as.vector(t(rmse_list[[1]]))[1:90] 
rmse_sum <- data.frame(size=size, cov = cov,rmse =rmse1)
library(ggplot2)
ggplot(rmse_sum, aes_(x = ~cov, y = ~size)) +
  geom_tile(aes_(fill = ~rmse, color = ~rmse),
            width = 1, height = 0.9, size = 1) +
  geom_text(aes_(label = ~round(rmse), fontface = ~rmse+1)) +
  labs(y = "Model size", x = "variable",
       title = "Forward proj Result") +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 45))
