getSplinesData <- function(nyears,
                           order,    # order of penalization 
                           I = 2.5,  
                           degree){
  if(order!=1&order!=2){
    stop("Order of penalization must either be 1 or 2.")
  }
  
  # get max dimensions
  x.t <- 1:nyears
  sp <- GetSplines(x.t, I = I ,degree = degree)
  K <- length(sp$knots.k)
  B.tk <- sp$B.ik
  # this is the maximum dimensions of the basis splines.
  max.dim.B <- dim(B.tk)
  len.t <- max.dim.B[1]
  len.k <- max.dim.B[2]
  # for order =1 , H = k - 1
  # for order = 2 H = k-2
  # following from this, for order = 1, dim(Z.ih) will be t x (k-1)
  # for order = 2, dim(Z.ih) will be t x (k-2)
  
  H1 <- K-1
  H2 <- K-2
  # stuff for reparameterization
  ## first difference
  Delta.hk <- diff(diag(K), diff = 1)
  Delta1comb.kh <- t(Delta.hk)%*%solve(Delta.hk%*%t(Delta.hk))
  Z.ih <-B.tk%*%Delta1comb.kh
  Z1.tk <- Z.ih
  ## second difference
  Delta.hk <- diff(diag(K), diff = 2) # difference matrix
  Delta2comb.kh <- t(Delta.hk)%*%solve(Delta.hk%*%t(Delta.hk))
  
  Z.ih <-B.tk%*%Delta2comb.kh
  Z2.tk <- Z.ih
  G.kd <- cbind(rep(1, K), seq(1, K)-K/2)
  
  BG.td <- B.tk%*%G.kd
  
  if(order==1){
    return(list(K = K, H = H1, H = max(H1),B.tk = B.tk, Z.tk = Z1.tk))
  }
  if(order==2){
    return(list(K = K, H = H2, D = 2, B.tk = B.tk, Z.tk = Z2.tk, BG.td = BG.td))
  }
}
