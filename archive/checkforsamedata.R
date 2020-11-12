fit.medianb1 <- readRDS("rdsoutput/ref.rds")
fit.medianb2 <- readRDS("rdsoutput/large10b.rds")

fit.medianb1 <- rstan::extract(fit.medianb1)
fit.medianb2 <- rstan::extract(fit.medianb2)
b_medianb1 <- apply(fit.medianb1$beta,2,median)
b_medianb2 <- apply(fit.medianb2$beta,2,median)
b_medianb1
b_medianb2

lambda_medianb1 <- apply(fit.medianb1$lambda,2,median)
lambda_medianb2 <- apply(fit.medianb2$lambda,2,median)
tau_median1 <- median(fit.medianb1$tau_tilde)
tau_median2 <- median(fit.medianb2$tau_tilde)

par(mfrow = c(1,1))
plot(0,0,xlim = c(1,20),ylim = c(-5,40))
#lines(b_medianb1-b_medianb2)
lines(lambda_medianb1,lty = "dashed",col = "red",lwd = 3)
lines(lambda_medianb2,lty = "dashed",col= "blue", lwd =3)
abline(h=tau_median1, col = "red",lwd = 3)
abline(h=tau_median2,col = "blue",lwd = 3)
legend(1,40,legend = c("ref","larger 10b"),lty = c("solid"),col = c("red","blue"),lwd = 3)

traceplot(fit.medianb2,par = c("tau_tilde","lambda"))
traceplot(fit.medianb1,par = c("beta"))
print(fit.medianb1,par = c("beta"))

par(mfrow = c(2,1))
hist(fit.medianb1$tau_tilde)
hist(fit.medianb2$tau_tilde)
