library(extraDistr)
library(transport)
### simulation output
simuoutput <- list.files(file.path(paste0(getwd(),"/rdsoutput/simu1111/")), ".rds") 
num_simu <- length(simuoutput)
simu <- list()

for(i in 1:num_simu){
  simu[[i]] <- rstan::extract(readRDS(paste0("rdsoutput/simu1111/",simuoutput[i]))) 
}

true_beta <- c(rep(c(0,0.2,0.4,0.6,0.8,5,6,7,8,9),2),rep(0,30))


cov_for_each <- 50
num_cov <- num_simu * cov_for_each
niter <- 4000




ref1ex <- get_output(simu)


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

prob_delta <- matrix(NA,nrow = num_simu,ncol=30)
for(i in 1:30){
  for(k in 21:50){
    prob_delta[i,(k-20)] <- cal_prob_beta(ref1ex$beta_hat_s[k,i,])
  }
}
prob_delta <- as.vector(prob_delta)

hist(prob_delta)
summary(prob_delta)

prob_beta <- matrix(NA,nrow = num_simu,ncol=20)
for(i in 1:30){
  for(k in 1:20){
    prob_beta[i,k] <- cal_prob_beta(ref1ex$beta_hat_s[k,i,])
  }
}

summary(prob_beta)


quantile(prob_delta,0.95)
prob_beta > quantile(prob_delta,0.95)

pdf_name <- paste0("fig/onefig_lambda_tau.pdf")
pdf(pdf_name, width = 8, height = 8)

res <- decision(prob_beta,threshold = quantile(prob_delta,0.95))
tru <- c(F,rep(T,9))
tru <- rep(tru,2)  

plot(NA, xlim = c(0, 1), ylim = c(1, 20), xlab = "prob", ylab="",
     yaxt="n",
     main = paste0("threshold=",0.95,"quantile"))
axis(2, 1:20, paste(round(true_beta[1:20],digits = 3)), las = 2)
for(i in 1:20){
  points(prob_beta[,i],rep(i,num_simu)+rnorm(num_simu,0,0.01),col=ifelse(res[,i]==tru[i],'blue','red'))
}

res_max <- decision(prob_beta,threshold = max(prob_delta))


plot(NA, xlim = c(0, 1), ylim = c(1, 20), xlab = "prob", ylab="",
     yaxt="n",
     main = paste0("threshold=","max prob_delta"))
axis(2, 1:20, paste(round(true_beta[1:20],digits = 3)), las = 2)
for(i in 1:20){
  points(prob_beta[,i],rep(i,num_simu)+rnorm(num_simu,0,0.01),col=ifelse(res_max[,i]==tru[i],'blue','red'))
}

dev.off()
