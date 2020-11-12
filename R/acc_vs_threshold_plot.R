acc_vs_threshold_plot <- function(mat,cov,truth = tru){
  decis <- sapply(mat[,cov],decision,seq(0,1,0.05))
  apply(decis == truth[cov],1,mean)
} 
