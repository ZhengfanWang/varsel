select_covar_lam_tau <- function(lambda,cov,tau,niter = 4000){
  set.seed(1234)
  priors.lam <- rhcauchy(niter, sigma = 1)
  priors.tau <- rhcauchy(niter,sigma = 1)
  s.priors <- sort(priors.lam*priors.tau)
  posts <- lambda[cov,]*tau
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}
