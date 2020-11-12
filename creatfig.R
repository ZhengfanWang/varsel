library(extraDistr)
library(transport)
### simulation output
simuoutput <- list.files(file.path(paste0(getwd(),"/rdsoutput/ref/")), ".rds") 
num_simu <- length(simuoutput)
simu <- list()

for(i in 1:num_simu){
  simu[[i]] <- rstan::extract(readRDS(paste0("rdsoutput/ref/",simuoutput[i]))) 
}
true_beta <- c(0,0,0,0,0,
          0.1,0.2,0.3,0.4,0.5,
          1,2,3,4,5,
          16,17,18,19,20)
cov_for_each <- 20
num_cov <- num_simu * cov_for_each
niter <- 4000




ref1ex <- get_output(simu)




prob <- cal_prob(ref1ex,dim(ref1ex$lambda)[2])
prob
prob2 <- cal_prob_lam_tau(ref1ex,dim(ref1ex$lambda)[2])
prob2
prob3 <- cal_wasserstein(ref1ex,dim(ref1ex$lambda)[2])
dim(prob3)

pdf_name <- paste0("fig/wasserstein.pdf")
pdf(pdf_name, width = 12, height = 6)
plot(NA, xlim = c(1, 20), ylim = c(10, 40), xlab = "prob", ylab="",
     main = "wasserstein distance- lambda*tau")
for(i in 1:30){
lines(prob2[i,])
}

plot(NA, xlim = c(1, 20), ylim = c(1, 20), xlab = "prob", ylab="",
     main = "wasserstein distance- lambda")
for(i in 1:30){
  lines(prob3[i,])
}
dev.off()

decision <- function(prob,threshold = 0.05) prob > threshold

tru <- c(F,F,F,F,F,
         T,T,T,T,T,
         T,T,T,T,T,
         T,T,T,T,T)


get_accuracy_tab(prob)

threshold_vec <- seq(0.05,0.95,0.05)
for(threshold in threshold_vec){
  res <- decision(prob,threshold = threshold)
 # acc <- round((sum(res[,1:5]) + sum(1-res[,6:16]))/(cov_for_each*num_simu),digits = 4)
  par(mfrow=c(1,1))
  plot(NA, xlim = c(1, 16), ylim = c(1, 95), xlab = "covar", ylab = "simu id",xaxt = "n", 
       #yaxt="n",
       main = paste0("threshold=",threshold,"\n","accuracy=",acc))
  
  for(i in 1:num_simu){
    for(j in 1:cov_for_each){
      if(res[i,j]==T) {color <- "blue"
      pchtype <- 19} else{color <- "red"
      pchtype <- 17}
      points(j,i,col = color,pch=pchtype)
    }
  }
}


pdf_name <- paste0("fig/onefig_lambda_tau.pdf")
pdf(pdf_name, width = 8, height = 8)

threshold_vec <- seq(0.05,0.95,0.05)
for(threshold in threshold_vec){
  res <- decision(prob2,threshold = threshold)
  #acc <- round((sum(res[,1:5]) + sum(1-res[,6:16]))/(cov_for_each*num_simu),digits = 4)
  par(mfrow=c(1,1))
  plot(NA, xlim = c(0, 1), ylim = c(1, 20), xlab = "prob", ylab="",
       yaxt="n",
       main = paste0("threshold=",threshold,"\n","accuracy=",acc))
  axis(2, 1:20, paste(round(true_beta,digits = 3)), las = 2)
  for(i in 1:cov_for_each){
    points(prob2[,i],rep(i,num_simu)+rnorm(num_simu,0,0.1),col=ifelse(res[,i]==tru[i],'blue','red'))
  }
  abline(v=threshold)
}
dev.off()

