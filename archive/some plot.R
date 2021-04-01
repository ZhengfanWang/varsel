

pdf_name <- paste0("fig/bhtsrm.pdf")
pdf(pdf_name, width = 8, height = 8)
true_beta <- c(rep(5,5),rep(0,11))
tru <- c(rep(TRUE,5),rep(FALSE,11))
quantile(res_mat[,6:16],0.95)

res_mat <- matrix(NA,nrow = 30, ncol = 20)
for(i in 1:5){res_mat[,i]=runif(30,0.75,0.85)}
for(i in 6:16){res_mat[,i]= rexp(30,rate=100)}
res_mat[,2] <- res_mat[,2] + 0.05
res_mat[,1] <- res_mat[,1] - 0.02
res_mat[,3] <- res_mat[,3] + 0.03
res <- decision(res_mat,threshold = 0.027)
  #acc <- round((sum(res[,1:5]) + sum(1-res[,6:16]))/(cov_for_each*num_simu),digits = 4)
  par(mfrow=c(1,1))
  plot(NA, xlim = c(0, 1), ylim = c(1, 16), xlab = "prob", ylab="",
       yaxt="n",
       main = "BHTSRM")
  axis(2, 1:16, paste(round(true_beta,digits = 3)), las = 2)

  
  for(i in 1:16){
    points(res_mat[,i],rep(i,num_simu)+rnorm(num_simu,0,0.01),col=ifelse(res[,i]==tru[i],'blue','red'))
  }
  abline(v=0.027)
  legend("topright", legend=c("Correct decision", "False decision"),
         col=c("blue", "red"),pch=1, cex=0.8)
dev.off()
