library(rstan)
library(extraDistr)

fit.ref <- readRDS("rdsoutput/ref.rds")
fit.n1000 <- readRDS("rdsoutput/N=1000.rds")
fit.n100 <- readRDS("rdsoutput/N=100.rds")
fit.largeb <- readRDS("rdsoutput/large_b.rds")
fit.medianb1 <- readRDS("rdsoutput/median_b1.rds")
fit.medianb2 <- readRDS("rdsoutput/median_b2.rds")
fit.large10b <- readRDS("rdsoutput/large10b.rds")

fit.chain <- rstan::extract(fit.ref)
fit.n1000 <- rstan::extract(fit.n1000)
fit.n100 <- rstan::extract(fit.n100)
fit.largeb <- rstan::extract(fit.largeb)
fit.medianb <- rstan::extract(fit.medianb1)
fit.large10b <- rstan::extract(fit.large10b)

apply(fit.chain$beta,2,median)
apply(fit.n100$beta,2,median)
apply(fit.n1000$beta,2,median)
apply(fit.largeb$beta,2,median)

apply(fit.chain$lambda,2,median)
apply(fit.n1000$lambda,2,median)
apply(fit.n100$lambda,2,median)
apply(fit.largeb$lambda,2,median)


median(fit.chain$tau_tilde)
median(fit.n1000$tau_tilde)
median(fit.n100$tau_tilde)
median(fit.largeb$tau_tilde)

lambda.largeb <- apply(fit.largeb$lambda,2,median)
lambda.largeb
lambda.ref <- apply(fit.chain$lambda,2,median)
lambda.ref
lambda.medianb <- apply(fit.medianb$lambda,2,median)
lambda.medianb
lambda.large10b <- apply(fit.large10b$lambda,2,median)
lambda.large10b
lambda.n100 <- apply(fit.n100$lambda,2,median)
lambda.n1000 <- apply(fit.n1000$lambda,2,median)

tau.largeb <- median(fit.largeb$tau_tilde)
tau.largeb
tau.ref <- median(fit.chain$tau_tilde)
tau.ref
tau.medianb <- median(fit.medianb$tau_tilde)
tau.medianb
tau.large10b <- median(fit.large10b$tau_tilde)
tau.large10b
tau.n100 <- median(fit.n100$tau_tilde)
tau.n1000 <- median(fit.n1000$tau_tilde)



sd_largeb <- lambda.largeb*tau.largeb
sd_ref <- lambda.ref*tau.ref
sd_medianb <- lambda.medianb*tau.medianb

plot(0,0,xlim = c(1,20),ylim = c(0,50))
lines(lambda.ref,lty = "dashed")
lines(lambda.largeb,lty = "dashed",col = "red")
lines(lambda.medianb,lty = "dashed", col = "blue")

lines(sd_largeb,lty = "solid",col = "red")
lines(sd_ref,lty = "solid")
lines(sd_medianb,lty = "solid", col = "blue")
legend(1,140,legend = c("lambda","lambda*tau"),lty = c("dashed","solid"))


plot(0,0,xlim = c(1,20),ylim = c(0,30))
abline(h=tau.largeb, lty = "dotted",col = "red")
abline(h=tau.ref, lty = "dotted")
abline(h=tau.medianb, lty = "dotted",col = "blue")


plot(0,0,xlim = c(1,20),ylim = c(0,15),main = "lambda and tau")
lines(lambda.ref,lty = "dashed",lwd = 3)
lines(lambda.n1000,lty = "dashed",lwd = 3,col = "red")
lines(lambda.n100,lty = "dashed", lwd = 3,col = "blue")

abline(h=tau.ref,lwd = 3)
abline(h=tau.n1000, col = "red",lwd = 3)
abline(h=tau.n100,col = "blue",lwd = 3)
legend(1,15,legend = c("reference","N=1000","N=100"),lty = c("solid"),col = c("black","red","blue"),lwd = 3)

plot(0,0,xlim = c(1,20),ylim = c(0,50),main = "lambda and tau")
lines(lambda.ref,lty = "dashed",lwd = 3)
lines(lambda.medianb,lty = "dashed",lwd = 3,col = "red")
lines(lambda.largeb,lty = "dashed", lwd = 3,col = "blue")

abline(h=tau.ref,lwd = 3)
abline(h=tau.medianb, col = "red",lwd = 3)
abline(h=tau.largeb,col = "blue",lwd = 3)
legend(1,15,legend = c("reference","median b","large b"),lty = c("solid"),col = c("black","red","blue"),lwd = 3)

median(rhcauchy(4000,1))
lambda.ref
