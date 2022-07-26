num_simu <- 30
num_cov <- 36
lam_list <- list()
tau_list <- list()
lam_list2 <- list()
tau_list2 <- list()
for(i in 1:num_simu){
  rds_cache <- readRDS(paste0("rdsoutput/set3/hss/",i,".rds"))
  #rds_cache2 <- readRDS(paste0("rdsoutput/set3/hss/hss",i,".rds"))
  rds <- rstan::extract(rds_cache)
  #rds2 <- rstan::extract(rds_cache2)
  for(a in 1:num_cov){
    lam_list[[i]] <- rds$lambda
    tau_list[[i]] <- rds$tau
    #lam_list2[[i]] <- rds2$lambda
    #tau_list2[[i]] <- rds2$tau
  }
  print(i)
}

pdf_name <- paste0("fig/hss_vs_2sim_res.pdf")
pdf(pdf_name, width = 12, height = 8)

mat1 <- matrix(NA, ncol = num_cov, nrow = num_simu)
phi_list <- list()
mat_cache <- matrix(NA,ncol = num_cov,nrow = 4000)
for(i in 1:num_simu){
  for(k in 1:num_cov){
  mat_cache[,k]<- lam_list[[i]][,k]*tau_list[[i]]}
  phi_list[[i]] <- mat_cache
  mat1[i,] <- apply(phi_list[[i]],2,median)
}

mat1 <- 1/(1+mat1)
threshold <- quantile(mat1[,17:36],0.05)
threshold_min <- min(mat1[,17:36])
res1 <- mat1[,1:16] < threshold
res1
res2 <- mat1[,1:16] < threshold_min


true_beta1 <- c(rep(5,5),rep(0,11))
tru1 <- c(rep(TRUE,5),rep(FALSE,11))
cal_acc(res1,tru1)
cal_acc(res2,tru1)
plot(NA, xlim = c(0, 1), ylim = c(1, 16), xlab = expression(paste("shrinkage parameters ",omega)), ylab="true beta",
     yaxt="n",
     main = paste0("setting 1","\n","Omega0.05=",round(threshold,digits = 4),"\n",
                   "Omega0=",round(threshold_min,digits = 4)))
axis(2, 1:16, paste(round(true_beta1,digits = 3)), las = 2)
for(i in 1:5){
  points(mat1[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1), col=ifelse(res1[,i]==tru1[i],'blue','red'),
         pch= ifelse(res1[,i]==tru1[i],1,16))
}

for(i in 6:16){
  points(mat1[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1), col=ifelse(res1[,i]==tru1[i],'chartreuse4','brown'),
         pch= ifelse(res1[,i]==tru1[i],1,16))
}
abline(v=threshold,col = "blue")
abline(v=threshold_min, col = "red")
legend("topleft", legend=c("Correct inclusion","Correct excluison", "False inclusion", "False exclusion"),
       col=c( "blue","chartreuse4","brown","red"), pch=c(1,1,16,16), cex=3)
#############################################################

mat2 <- matrix(NA, ncol = num_cov, nrow = num_simu)
phi_list2 <- list()
mat_cache <- matrix(NA,ncol = num_cov,nrow = 4000)
for(i in 1:num_simu){
  for(k in 1:num_cov){
    mat_cache[,k]<- lam_list2[[i]][,k]*tau_list2[[i]]}
  phi_list2[[i]] <- mat_cache
  mat2[i,] <- apply(phi_list2[[i]],2,median)
}

mat2 <- 1/(1+mat2)
threshold2 <- quantile(mat2[,17:36],0.05)
threshold2_min <- min(mat2[,17:36])
res2 <- mat2[,1:16] < threshold2
res2

true_beta2 <- c( 0.4, -0.2,  0.2, -0.15, 0.15,
                         0.08, 0.06, 0.04,  0.02,    0,
                         0,    0,    0,     0,    0,
                         0)
tru2 <- c(rep(TRUE,9),rep(FALSE,7))
cal_acc(res2,tru2)
plot(NA, xlim = c(0.5, 1), ylim = c(1, 16), xlab = expression(paste("shrinkage parameters ",omega)), ylab="true beta",
     yaxt="n",
     main = paste0("setting 2","\n","threshold=",round(threshold2,digits = 4)))
axis(2, 1:16, paste(round(true_beta2,digits = 3)), las = 2)
for(i in 1:9){
  points(mat2[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1), col=ifelse(res2[,i]==tru2[i],'blue','red'),
         pch= ifelse(res2[,i]==tru2[i],1,16))
}

for(i in 10:16){
  points(mat2[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1), col=ifelse(res2[,i]==tru2[i],'chartreuse4','brown'),
         pch= ifelse(res2[,i]==tru2[i],1,16))
}
abline(v=threshold2)
#abline(v=threshold2_min)
legend("topleft", legend=c("Correct inclusion","Correct excluison", "False inclusion", "False exclusion"),
       col=c( "blue","chartreuse4","brown","red"), pch=c(1,1,16,16), cex=3)

#######################################################################
d1_dummy <- density(mat1[,17:36])
d1_cov <- density(mat1[,1:17])

d2_dummy <- density(mat2[,17:36])
d2_cov <- density(mat2[,1:17])
# illustive fig 1

plot(d1_dummy, main = expression(paste("Reference distribution ",Omega)), yaxt = "n", col = "black" , lwd = 3, xlim = c(0.993,1),
     xlab = expression(paste("shrinkage parameters ", omega)))
text(0.99625,10,expression(alpha),cex = 2)
text(0.9967,-10,expression(Omega),cex = 2)
text(0.99682,-16,expression(alpha),cex = 1)
abline(v=threshold, lty = 3, col = "blue")

# illustive fig 2

plot(d1_dummy, main = expression(paste("Reference distribution ",Omega)), yaxt = "n", col = "black" , lwd = 3, xlim = c(0.993,1),
     xlab = expression(paste("shrinkage parameters ", omega)))
text(0.99625,10,expression(alpha),cex = 2)
text(0.9967,-10,expression(Omega),cex = 2)
text(0.99682,-16,expression(alpha),cex = 1)
abline(v=threshold, lty = 3, col = "blue")
points(c(0.995,0.998),c(70,70), col = c("blue","red"), pch = 3, cex = 1.5)
text(0.99515,60, expression(paste(omega,"1")))
text(0.99815,60, expression(paste(omega,"2")))
legend("topleft", legend=c("inclusion","exclusion"),
       col=c( "blue","red"), pch = 3, cex=3)
# simulation setting 1

plot(d1_dummy,col = "black" , lwd = 3,
     main = expression(paste("Reference distribution ",Omega, " in simulation setting 1")), yaxt = "n",
     xlab = expression(paste("shrinkage parameters ", omega)))
abline(v=threshold,lty = 3, col = "blue")
abline(v=threshold_min,lty = 3, col = "red")
text(0.9967,-5,expression(paste(Omega, "0.05")),cex = 2)
text(0.99,-5,expression(paste(Omega, "0")),cex = 2)
for(i in 1:5){
  points(mat1[,i],70+rnorm(num_simu,0,100), col=ifelse(res1[,i]==tru1[i],'blue','red'),
         pch= ifelse(res1[,i]==tru1[i],1,16))
}

for(i in 6:16){
  points(mat1[,i],70+rnorm(num_simu,0,100), col=ifelse(res1[,i]==tru1[i],'chartreuse4','brown'),
         pch= ifelse(res1[,i]==tru1[i],1,16))
}
legend("topleft", legend=c("Correct inclusion","Correct excluison", "False inclusion", "False exclusion"),
       col=c( "blue","chartreuse4","brown","red"), pch=c(1,1,16,16), cex=2)
# simulation setting 2
plot(d2_dummy,col = "black" , lwd = 3,
     main = expression(paste("Reference distribution ",Omega, " in SBR simulation settings")), yaxt = "n",
     xlab = expression(paste("shrinkage parameters ", omega)))
abline(v=threshold2,lty = 3, col = "blue")

# Reference distribution in 2 settings
plot(d1_dummy, col = "blue", lwd = 3,
     main = expression(paste("Reference distribution ",Omega, " in 2 simulation settings")), yaxt = "n",
     xlab = expression(paste("shrinkage parameters ", omega)), 
     ylim = c(0,800))
lines(d2_dummy, col = "red", lwd = 3)
abline(v=threshold, col = "blue", lty = 3)
abline(v=threshold2, col = "red", lty = 3)
legend("topleft", legend=c("Simulation 1","Simulaiton 2"),
       col=c( "blue","red"), lty = 1, cex=3)
dev.off()
#create data set for selected covariates.
for(i in 1:num_simu){
  stan_data <- readRDS(paste0("output/ori_dat/dat",i,".rds"))
  select_data1 <- select_cov(stan_data,which(res[i,]==TRUE))
  select_data2 <- select_cov(stan_data,which(res2[i,]==TRUE))
  saveRDS(select_data1, paste0("output/hss_vs_v2_95/dat",i,".rds"))
  saveRDS(select_data2, paste0("output/hss_vs_v2_max/dat",i,".rds"))
}
saveRDS(res2,"hssmax_res.rds")
