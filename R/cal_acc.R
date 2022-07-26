cal_acc <- function(result_mat,truth){
  num_simu <- dim(result_mat)[1]
  num_cov <- length(truth)
  num_true <- sum(truth == TRUE)
  # false exclusion rate 
  fer <- 1 - sum(result_mat[,1:num_true])/(num_simu*num_true)
  # false inclusion rate
  fir <- sum(result_mat[,(num_true+1):num_cov])/(num_simu*(num_cov - num_true))
  # selection accuracy rate
  acc <- (sum(result_mat[,1:num_true]) + (sum(1-result_mat[,(num_true+1):num_cov])))/(num_simu*num_cov)

  
  return(list(FER = fer, FIR = fir, ACC= acc))
}
