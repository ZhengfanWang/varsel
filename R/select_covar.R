select_covar <- function(model,cov,niter = 4000){
  set.seed(1234)
  priors <- rhcauchy(niter, sigma = 1)
  s.priors <- sort(priors)
  posts <- model[cov,]
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}
