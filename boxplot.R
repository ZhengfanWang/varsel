


pdf("fig/boxplot.pdf", height = 9, width = 11)
# d <- density(lam_list[[30]][,1])
# plot(d,xlim = c(0,10),ylim = c(0,0.7),col = "red", main  = "density of lambda")
# for(a in 2:9){
#   d <- density(lam_list[[30]][,a])
#   lines(d,col= "red")
# }
# for(b in 10:16){
#   d <- density(lam_list[[30]][,b])
#   lines(d,col = "blue")
# }
# for(k in 17:36){
#   d <- density(lam_list[[30]][,k])
#   lines(d)
# }
# # d <- density(rhcauchy(4000,1))
# # lines(d,col = "grey")
# legend(8, 0.5, legend=c("relevant", "irrelevant", "dummy" ),
#        col=c("red", "blue", "black"), lty=1, cex=0.8)

# d <- density(phi_list[[30]][,1])
# plot(d,xlim = c(0,0.1),ylim = c(0,210),col = "red", main  = "density of psi")
# for(a in 2:9){
#   d <- density(phi_list[[30]][,a])
#   lines(d,col= "red")
# }
# for(b in 10:16){
#   d <- density(phi_list[[30]][,b])
#   lines(d,col = "blue")
# }
# for(k in 17:36){
#   d <- density(phi_list[[30]][,k])
#   lines(d)
# }

# legend(0.08, 200, legend=c("relevant", "irrelevant", "dummy"),
#        col=c("red", "blue", "black"), lty=1, cex=0.8)

boxplot(mat1[,1:36], ylim = c(0,1), main = "boxplot of shrinkage parameters, simulation 1", xlab = "covariate index", ylab = "shrinkage parameters")
abline(v = 5.5)
abline(v = 16.5)
abline(h = threshold)

boxplot(mat1[,1:36], ylim = c(0.95,1), main = "boxplot of shrinkage parameters, simulation 1", xlab = "covariate index", ylab = "shrinkage parameters")
abline(v = 5.5)
abline(v = 16.5)
abline(h = threshold)

boxplot(mat2[,1:36], ylim = c(0,1), main = "boxplot for shrinkage parameters, simulation 2", xlab = "covariate index", ylab = "shrinkage parameters")
abline(v = 9.5)
abline(v = 16.5)
abline(h = threshold2)
boxplot(mat2[,1:36], ylim = c(0.95,1), main = "boxplot for shrinkage parameters, simulation 2", xlab = "covariate index", ylab = "shrinkage parameters")
abline(v = 9.5)
abline(v = 16.5)
abline(h = threshold2)
dev.off()


#########################################################

