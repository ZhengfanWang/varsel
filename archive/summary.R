library(extraDistr)

### simulation output
simuoutput <- list.files(file.path(paste0(getwd(),"/rdsoutput/simu/simu2/")), ".rds") 
num_simu <- length(simuoutput)
simu <- list()
true_beta <- list()
for(i in 1:num_simu){
simu[[i]] <- rstan::extract(readRDS(paste0("rdsoutput/simu/simu2/",simuoutput[i]))) 
true_beta[[i]] <- readRDS(paste0("output/stan_data/simu2/",simuoutput[i]))$true_beta
}


###################################################
###################################################
cov_for_each <- 16
num_cov <- num_simu * cov_for_each
niter <- 4000
lambda <- matrix(NA,nrow = num_cov, ncol = niter)
beta_hat_s <- matrix(NA,nrow = num_cov, ncol = niter)
true_b <- c(NA,length = num_cov)
for(i in 1:num_simu){
lambda[((i-1)*cov_for_each+1):((i-1)*cov_for_each+16),] <- t(simu[[i]]$lambda)
beta_hat_s[((i-1)*cov_for_each+1):((i-1)*cov_for_each+16),]<- t(simu[[i]]$beta)
true_b[((i-1)*cov_for_each+1):((i-1)*cov_for_each+16)] <- true_beta[[i]]
}


################################################################################################


select_covar <- function(model,cov,niter = 4000){
  priors <- rhcauchy(niter, sigma = 1)
  s.priors <- sort(priors)
  posts <- model[cov,]
  s.posts <- sort(posts)
  sum(s.posts > s.priors)/niter
}


prob <- c()
for(i in 1:num_cov){
  prob[i] <- select_covar(lambda,i)
}
beta_hat <- apply(beta_hat_s,1,median)
summary_tab <- data.frame(true_b = true_b, beta_hat = beta_hat, prob = prob)
sum_tab <- summary_tab %>% mutate(in0.05 = prob > 0.05,
                       truth = true_b != 0,
                       discor = in0.05 != truth)
sum_tab[sum_tab$discor == TRUE,]

sum(sum_tab$true_b ==0)

sum(sum_tab$inmodel * sum_tab$truth)
sum((1-sum_tab$inmodel) * (1-sum_tab$truth))
sum(sum_tab$inmodel * (1-sum_tab$truth))
sum((1-sum_tab$inmodel) * sum_tab$truth)

decision <- function(prob,threshold = 0.05) prob > threshold
dec <- sapply(prob,decision,seq(0.01,1,0.01))

different <- function(vec1,vec2) vec1 == vec2
res <- apply(dec,1,different,sum_tab$truth)
acc <- apply(res,2,sum)/num_cov

plot(acc ~ seq(0.01,1,0.01),type = "l", xlab = "Threshold", ylab = "accuracy")
abline(h = 0.95,col="red",lty = 3)



