
dim(ref1ex$tau)
lam <- ref1ex$lambda[2,2,]
tau <- ref1ex$tau[2,]
lam <- ref1ex$lambda[1,2,]
tau <- ref1ex$tau[1,]

set.seed(1234)
priors.lam <- rhcauchy(niter, sigma = 1)
priors.tau <- rhcauchy(niter,sigma = 1)

post <- lam*tau
prior <- priors.lam*priors.tau

s.prior  <-  sort(priors.lam*priors.tau) 
s.post <- sort(lam*tau)

sum(s.post > s.prior)/4000
    

sum(post > prior)/4000    

qqplot(s.prior,s.post,xlim = c(0,1),ylim=c(0,1))
abline(0,1)

qqplot(s.prior,s.post,xlim = c(0,5),ylim=c(0,5))
abline(0,1)

qqplot(s.prior,s.post,xlim = c(0,100),ylim=c(0,100))
abline(0,1)

qqplot(s.prior,s.post,xlim = c(0,1000),ylim=c(0,1000))
abline(0,1)

qqplot(s.prior,s.post,xlim = c(0,2000),ylim=c(0,2000))
abline(0,1)


qqplot(s.prior,s.post)
abline(1,1)
which(s.post < s.prior)
 median(prior)
median(post) 
