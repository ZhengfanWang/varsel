library(extraDistr)

dim(ref1ex$beta_hat_s)

hist(ref1ex$beta_hat_s[1:5,,])
hist(ref1ex$beta_hat_s[6:10,,])
hist(ref1ex$beta_hat_s[11:15,,])
hist(ref1ex$beta_hat_s[16:20,,])

true_beta <- c(0,0,0,0,0,
               0.1,0.2,0.3,0.4,0.5,
               1,2,3,4,5,
               16,17,18,19,20)
set.seed(1234)

lam_prior <- rhcauchy(4000,1)
tau_prior <- rhcauchy(4000,1)
sd_prior <- lam_prior * tau_prior
b_prior <- rnorm(4000,0,sd_prior)
b_prior_s <- sort(b_prior)


cal_prob_beta <- function(beta_hat){
  set.seed(1234)
  lam_prior <- rhcauchy(4000,1)
  tau_prior <- rhcauchy(4000,1)
  sd_prior <- lam_prior * tau_prior
  b_prior <- rnorm(4000,0,sd_prior)
  b_prior_s <- sort(abs(b_prior))
  
  b_post_s <- sort(abs(beta_hat))
  prob <- mean(b_post_s > b_prior_s)
  return(prob)
}

prob <- matrix(NA,nrow = 30,ncol=5)
for(i in 1:30){
  for(k in 1:5){
  prob[i,k] <- cal_prob_beta(ref1ex$beta_hat_s[k,i,])
  
  }
}
prob <- as.vector(prob)

hist(prob)
summary(prob)

qqplot(b_prior,ref1ex$beta_hat_s[1,1,],xlim = c(-10,10),ylim=c(-10,10))
abline(0,1)

qqplot(s.prior,s.post,)
abline(0,1)


prob2 <- matrix(NA,nrow = 30,ncol=15)
for(i in 1:30){
  for(k in 6:20){
    prob2[i,(k-5)] <- cal_prob_beta(ref1ex$beta_hat_s[k,i,])
    
  }
}
prob2
summary(prob2)
summary(prob)
quantile(prob,c(0.75,0.95))
prob2 >quantile(prob,0.95)

par(mfrow = c(1,1))
hist(prob)
mean(prob > quantile(prob,c(0.75,0.95)))
