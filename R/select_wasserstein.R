select_wasserstein <- function(lambda,cov,tau,niter = 4000){
  set.seed(1234)
  priors.lam <- rhcauchy(niter, sigma = 1)
  priors.tau <- rhcauchy(niter,sigma = 1)
  s.priors <- sort(priors.lam*priors.tau)
  posts <- lambda[cov,]*tau
  wasserstein1d(s.priors,posts)
  #wasserstein1d(priors.lam,lambda[cov,])
}
