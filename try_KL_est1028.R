my.ecdf  <-  function(x)   {
  x   <-   sort(x)
  x.u <-   unique(x)
  n  <-  length(x) 
  x.rle  <-  rle(x)$lengths
  y  <-  (cumsum(x.rle)-0.5) / n
  FUN  <-  approxfun(x.u, y, method="linear", yleft=0, yright=1,
                     rule=2)
  FUN
}


KL_est  <-  function(x, y)   {
  dx  <-  diff(sort(unique(x)))
  dy  <-  diff(sort(unique(y)))
  ex  <-  min(dx) 
  ey  <-  min(dy)
  e   <-  min(ex, ey)/2
  n   <-  length(x)    
  P  <-   my.ecdf(x) 
  Q  <-  my.ecdf(y)
  KL  <-  sum(log( (P(x)-P(x-e))/(Q(x)-Q(x-e)))) / n
  KL              
}

KL  <-  replicate(1000, {x  <-  rnorm(100)
y <- rt(100, df=5)
KL_est(x, y)})
hist(KL, prob=TRUE)

x <- rnorm(1000,0,1)
y <- rnorm(1000,0,1)
KL_est(x,y)

LR  <-  function(x) dunif(x,0,1)-dunif(x,0,10)
integrate(function(x) dnorm(x)*LR(x),lower=-Inf,upper=Inf)$value

my.ecdf(rnorm(100,0,1))

dnorm(1)
dnorm(1,log = T)

x <- sort(x)
x.u <-   unique(x)
n  <-  length(x) 
x.rle  <-  rle(x)$lengths
y  <-  (cumsum(x.rle)-0.5) / n
FUN  <-  approxfun(x.u, y, method="linear", yleft=0, yright=1,
                   rule=2)
FUN
ey
ex

library(transport)
library(extraDistr)
x <- rhcauchy(4000,1)
y <- rnorm(4000,0,10)
wasserstein1d(x,y)

