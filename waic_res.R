## load output


rdsfiles <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic")), ".rds") #gets names of r files
rdsfiles1 <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic1")), ".rds") #gets names of r files
rdsfiles14 <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic14")), ".rds") #gets names of r files
rdsfiles145 <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic145")), ".rds") #gets names of r files
rdsfiles1245 <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic1245")), ".rds") #gets names of r files
rdsfiles12345 <- list.files(file.path(paste0(getwd(),"/rdsoutput/waic12345")), ".rds") #gets names of r files
library(loo)
elpd0 <- c()
for(i in 1:length(rdsfiles)){
  dat <- readRDS(paste0("rdsoutput/waic/boutput",i,".rds"))
  loo <- loo(dat)
  elpd0[i] <- loo$elpd_loo
  print(i)
}
elpd0
rdsfiles[which(elpd0==max(elpd0))]
rdsfiles[order(-elpd0)[1:5]]


cov_vec <- c(1:15)[!(c(1:15) %in% c(1))]
elpd1 <- c()
for(i in cov_vec){
  dat <- readRDS(paste0("rdsoutput/waic1/boutput",i,".rds"))
  loo <- loo(dat)
  elpd1[i] <- loo$elpd_loo
  print(i)
}
elpd1
rdsfiles1[which(elpd1==max(elpd1))]
rdsfiles1[order(-elpd1)[1:4]]

cov_vec <- c(1:15)[!(c(1:15) %in% c(1,4))]
elpd14 <- c()
for(i in cov_vec){
  dat <- readRDS(paste0("rdsoutput/waic14/boutput",i,".rds"))
  loo <- loo(dat)
  elpd14[i] <- loo$elpd_loo
  print(i)
}
rdsfiles14[which(elpd14==max(elpd14))]
rdsfiles14[order(-elpd14)[1:3]]

cov_vec <- c(1:15)[!(c(1:15) %in% c(1,4,5))]
elpd145 <- c()
for(i in cov_vec){
  dat <- readRDS(paste0("rdsoutput/waic145/boutput",i,".rds"))
  loo <- loo(dat)
  elpd145[i] <- loo$elpd_loo
  print(i)
}
rdsfiles145[which(elpd145==max(elpd145))]
rdsfiles145[order(-elpd145)[1:2]]

cov_vec <- c(1:15)[!(c(1:15) %in% c(1,2,4,5))]
elpd1245 <- c()
for(i in cov_vec){
  dat <- readRDS(paste0("rdsoutput/waic1245/boutput",i,".rds"))
  loo <- loo(dat)
  elpd1245[i] <- loo$elpd_loo
  print(i)
}
rdsfiles1245[which(elpd1245==max(elpd1245))]
rdsfiles1245[order(-elpd1245)[1:2]]

cov_vec <- c(1:15)[!(c(1:15) %in% c(1,2,3,4,5))]
elpd12345 <- c()
for(i in cov_vec){
  dat <- readRDS(paste0("rdsoutput/waic12345/boutput",i,".rds"))
  loo <- loo(dat)
  elpd12345[i] <- loo$elpd_loo
  print(i)
}
rdsfiles12345[which(elpd12345==max(elpd12345))]
rdsfiles12345[order(-elpd12345)[1:2]]

elpd_sum <- rbind(elpd,elpd1,elpd14,elpd145,elpd1245,elpd12345)
names <- "cov1"
for(i in 2:15){
  names <- c(names,paste0("cov",i))}
colnames(elpd_sum) <- names
elpd_sum 




## PLOTs
size <- factor(c(rep(1,15),rep(2,15),rep(3,15),rep(4,15),rep(5,15),rep(6,15)),levels = 6:1)
cov <- factor(rep(names,6),levels = names)
elpd <- c(elpd0,elpd1,elpd14,elpd145,elpd1245,elpd12345)
elpd_sum <- data.frame(size=size, cov = cov,elpd =elpd)
library(ggplot2)
ggplot(elpd_sum, aes_(x = ~cov, y = ~size)) +
  geom_tile(aes_(fill = ~elpd, color = ~elpd),
            width = 1, height = 0.9, size = 1) +
  geom_text(aes_(label = ~round(elpd), fontface = ~elpd+1)) +
  labs(y = "Model size", x = "variable",
       title = "Forward ELPD Result") +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 45))