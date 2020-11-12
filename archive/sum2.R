library(extraDistr)

### simulation output
simuoutput <- list.files(file.path(paste0(getwd(),"/rdsoutput/simu/df1/")), ".rds") 
num_simu <- length(simuoutput)
simu <- list()
true_beta <- list()
for(i in 1:num_simu){
  simu[[i]] <- rstan::extract(readRDS(paste0("rdsoutput/simu/df1/",simuoutput[i]))) 
}
true_beta<- readRDS(paste0("output/stan_data/df5/exp13763.rds"))$true_beta
true_beta


###################################################
###################################################
cov_for_each <- 16
niter <- 4000

get_output <- function(model){
lambda <- array(NA, dim = c(cov_for_each, num_simu, niter))
beta_hat_s <- array(NA, dim = c(cov_for_each, num_simu, niter))
tau <- matrix(NA,ncol = niter, nrow = num_simu)

for(i in 1:num_simu){
  tau[i,] <- simu[[i]]$tau 
  for(j in 1:cov_for_each){
  lambda[j,i,] <- simu[[i]]$lambda[,j]
  beta_hat_s[j,i,]<- simu[[i]]$beta[,j]
}
}
return(list(tau=tau,lambda=lambda,beta_hat_s=beta_hat_s))
}

tau_0.0041 <- get_output(simu)

tau_0.00035 <- get_output(simu)
tau_0.001 <- get_output(simu)
tau_1 <- get_output(simu)
################################################################################################

select_covar <- function(model,cov,niter = 4000){
  priors <- rhcauchy(niter, sigma = 1)
  s.priors <- sort(priors)
  posts <- model[cov,]
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}

cal_prob <- function(simu_res,num_simu){
prob <- matrix(NA,ncol=cov_for_each,nrow = num_simu)
for(i in 1:num_simu){
  for(j in 1:cov_for_each){
prob[i,j] <- select_covar(simu_res$lambda[,i,],j)    
  }
}
return(prob)
}

prob_0.001 <- cal_prob(tau_0.001,dim(tau_0.001$lambda)[2]) 
prob_0.0041 <- cal_prob(tau_0.0041,dim(tau_0.0041$lambda)[2]) 
prob_0.00035 <- cal_prob(tau_0.00035,dim(tau_0.00035$lambda)[2]) 
prob_1 <- cal_prob(tau_1,dim(tau_1$lambda)[2])

int_cov <- c("log_gni","log_nmr","log_lbw","anc4","edu",
             "gini","urban","gfr","sab","anc1","abr",
             "csec","pab_sm","pfpr","gdp","mmr")
colnames(prob_0.001) <- int_cov
colnames(prob_0.0041) <- int_cov
colnames(prob_0.00035) <- int_cov
colnames(prob_1) <- int_cov
par(mfrow=c(2,2))
boxplot(prob_0.00035, main = "tau0=0.00035,p0=2")
boxplot(prob_0.001,main = "tau0=0.001,p0=5")
boxplot(prob_0.0041, main = "tau0=0.0041,p0=10")
boxplot(prob_1, main = "tau0=1,p0 > 10")
###################################################################################
decision <- function(prob,threshold = 0.05) prob > threshold

true_beta
tru <- c(T,T,T,T,
         T,F,F,F,
         F,F,F,F,
         F,F,F,F)

acc_vs_threshold_plot <- function(mat,cov,truth = tru){
  decis <- sapply(mat[,cov],decision,seq(0,1,0.05))
  apply(decis == truth[cov],1,mean)
} 

get_accuracy_tab <- function(prob_mat){
accuracy <- matrix(NA,nrow = cov_for_each, ncol = length(seq(0,1,0.05)))
for(cov in 1:cov_for_each){
accuracy[cov,] <- acc_vs_threshold_plot(prob_mat,cov)
}
colnames(accuracy) <- seq(0,1,0.05)
return(accuracy)
}
acc_tab_0.001 <- get_accuracy_tab(prob_0.001)
acc_tab_0.0041 <- get_accuracy_tab(prob_0.0041)
acc_tab_0.00035 <- get_accuracy_tab(prob_0.00035)
write.csv(acc_tab_0.001,"output/acc_tab_0.001.csv")
write.csv(acc_tab_0.0041,"output/acc_tab_0.0041.csv")
write.csv(acc_tab_0.00035,"output/acc_tab_0.00035.csv")
par(mfrow=c(4,4))
for(i in 1:cov_for_each){
res <- acc_vs_threshold_plot(prob,i)
upper <- res + qnorm(0.975)*sqrt(res*(1-res)/num_simu)
low <- res - qnorm(0.975)*sqrt(res*(1-res)/num_simu)
plot(res ~ seq(0.01,1,0.01),type = "l", xlab = "Threshold", ylab = "accuracy",
     ylim = c(0,1),
     main = paste(int_cov[i],round(true_beta[i],digits = 3)))
lines(upper~ seq(0.01,1,0.01),lty = 2,col = "red")
lines(low~ seq(0.01,1,0.01), lty = 2,col = "red")
}

#################################################################################
pdf_name <- paste0("fig/threshold_acc.pdf")
pdf(pdf_name, width = 8, height = 8)

threshold_vec <- seq(0.05,0.95,0.05)
for(threshold in threshold_vec){
res <- decision(prob,threshold = threshold)
acc <- round((sum(res[,1:5]) + sum(1-res[,6:16]))/(cov_for_each*num_simu),digits = 4)
par(mfrow=c(1,1))
plot(NA, xlim = c(1, 16), ylim = c(1, 95), xlab = "covar", ylab = "simu id",xaxt = "n", 
     #yaxt="n",
     main = paste0("threshold=",threshold,"\n","accuracy=",acc))
axis(1, 1:16, paste(int_cov,round(true_beta,digits = 3)), las = 2)
for(i in 1:num_simu){
  for(j in 1:cov_for_each){
    if(res[i,j]==T) {color <- "blue"
                     pchtype <- 19} else{color <- "red"
                     pchtype <- 17}
    points(j,i,col = color,pch=pchtype)
  }
}
}
dev.off()

prob <- prob_0.001
num_simu <- dim(prob_0.001)[1]
pdf_name <- paste0("fig/onefigtau0.001.pdf")
pdf(pdf_name, width = 8, height = 8)
int_cov <- c("log_gni","log_nmr","log_lbw","anc4","edu",
             "gini","urban","gfr","sab","anc1","abr",
             "csec","pab_sm","pfpr","gdp","mmr")
threshold_vec <- seq(0.05,0.95,0.05)
for(threshold in threshold_vec){
  res <- decision(prob,threshold = threshold)
  acc <- round((sum(res[,1:5]) + sum(1-res[,6:16]))/(cov_for_each*num_simu),digits = 4)
  par(mfrow=c(1,1))
  plot(NA, xlim = c(0, 1), ylim = c(1, 16), xlab = "prob", ylab="",
       yaxt="n",
       main = paste0("threshold=",threshold,"\n","accuracy=",acc))
  axis(2, 1:16, paste(int_cov,round(true_beta,digits = 3)), las = 2)
  for(i in 1:cov_for_each){
   points(prob[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1),col=ifelse(res[,i]==tru[i],'blue','red'))
  }
  abline(v=threshold)
}
dev.off()

####################################################################################################################
ci_func <- function(x) c(mean(x)-qnorm(0.975)*sd(x)/length(x),mean(x),mean(x)+qnorm(0.975)*sd(x)/length(x))
CI_95 <- apply(prob,2,ci_func)
#apply(decis,2,sum)

int_cov <- c("log_gni","log_nmr","log_lbw","anc4","edu",
             "gini","urban","gfr","sab","anc1","abr",
             "csec","pab_sm","pfpr","gdp","mmr")
mean <- CI_95[2,]
ses1 <- CI_95[1,]
ses2 <- CI_95[3,]

par(mfrow=c(1,1))
plot(NA, xlim = c(0, 1), ylim = c(0, 16), xlab = "prob", ylab = "", yaxt = "n")
# We can add a title:
title("Simulation CI")
# We'll add a y-axis labelling our variables:
axis(2, 1:16, paste(int_cov,round(true_beta,digits = 3)), las = 2)
# We'll add a vertical line for zero:

# Then we'll draw our slopes as points (`pch` tells us what type of point):
points(mean, 1:16, pch = 23, col = "black", bg = "black")
# Then we'll add thick line segments for each 1 SE:
segments(ses1[1],1 , ses2[1],1 , col = "red", lwd = 2)
segments(ses1[2],2 , ses2[2],2 , col = "red", lwd = 2)
segments(ses1[3],3 , ses2[3],3 , col = "red", lwd = 2)
segments(ses1[4],4 , ses2[4],4 , col = "red", lwd = 2)
segments(ses1[5],5 , ses2[5],5 , col = "red", lwd = 2)
segments(ses1[6],6 , ses2[6],6 , col = "red", lwd = 2)
segments(ses1[7],7 , ses2[7],7 , col = "red", lwd = 2)
segments(ses1[8],8 , ses2[8],8 , col = "red", lwd = 2)
segments(ses1[9],9 , ses2[9],9 , col = "red", lwd = 2)
segments(ses1[10],10 , ses2[10],10 , col = "red", lwd = 2)
segments(ses1[11],11 , ses2[11],11 , col = "red", lwd = 2)
segments(ses1[12],12 , ses2[12],12 , col = "red", lwd = 2)
segments(ses1[13],13 , ses2[13],13 , col = "red", lwd = 2)
segments(ses1[14],14 , ses2[14],14 , col = "red", lwd = 2)
segments(ses1[15],15 , ses2[15],15 , col = "red", lwd = 2)
segments(ses1[16],16 , ses2[16],16 , col = "red", lwd = 2)
abline(v = 0.05, col = "gray")
abline(v = 0.2, col = "gray")

####################################################################################################
