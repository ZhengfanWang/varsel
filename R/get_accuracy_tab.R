get_accuracy_tab <- function(prob_mat){
  accuracy <- matrix(NA,nrow = cov_for_each, ncol = length(seq(0,1,0.05)))
  for(cov in 1:cov_for_each){
    accuracy[cov,] <- acc_vs_threshold_plot(prob_mat,cov)
  }
  colnames(accuracy) <- seq(0,1,0.05)
  return(accuracy)
}