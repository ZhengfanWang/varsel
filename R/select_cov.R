select_cov <- function(dat,cov){
  cov_array <- dat$covar_array[cov,,]
  dat$true_cov <- dat$covar_array
  dat$covar_array <- cov_array
  dat$numcov <- length(cov)
  return(dat)
}