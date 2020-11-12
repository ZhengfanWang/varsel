
hs <- readRDS("rdsoutput/new/regHS_nval_res202000311.rds")
hs.array <- rstan::extract(hs)

res <- apply(hs.array$beta,2,quantile,c(0.05,0.5,0.95))
result <- t(res)

int_cov <- c("log_gni_sm","log_nmr","log_lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
slopes <- result[,2]
ses1 <- result[,1]
ses2 <- result[,3]
plot(NA, xlim = c(-0.5, 0.5), ylim = c(0, 16), xlab = "Slope", ylab = "", yaxt = "n")
# We can add a title:
title("Regression Results")
# We'll add a y-axis labelling our variables:
axis(2, 1:16, int_cov, las = 2)
# We'll add a vertical line for zero:
abline(v = 0, col = "gray")
# Then we'll draw our slopes as points (`pch` tells us what type of point):
points(slopes, 1:16, pch = 23, col = "black", bg = "black")
# Then we'll add thick line segments for each 1 SE:
segments(ses1[1],1 , ses2[1],1 , col = "red", lwd = 2)
segments(ses1[2],2 , ses2[2],2 , col = "red", lwd = 2)
segments(ses1[3],3 , ses2[3],3 , col = "red", lwd = 2)
segments(ses1[4],4 , ses2[4],4 , col = "red", lwd = 2)
segments(ses1[5],5 , ses2[5],5 , col = "red", lwd = 2)
segments(ses1[6],6 , ses2[6],6 , col = "blue", lwd = 2)
segments(ses1[7],7 , ses2[7],7 , col = "black", lwd = 2)
segments(ses1[8],8 , ses2[8],8 , col = "blue", lwd = 2)
segments(ses1[9],9 , ses2[9],9 , col = "blue", lwd = 2)
segments(ses1[10],10 , ses2[10],10 , col = "black", lwd = 2)
segments(ses1[11],11 , ses2[11],11 , col = "black", lwd = 2)
segments(ses1[12],12 , ses2[12],12 , col = "black", lwd = 2)
segments(ses1[13],13 , ses2[13],13 , col = "black", lwd = 2)
segments(ses1[14],14 , ses2[14],14 , col = "black", lwd = 2)
segments(ses1[15],15 , ses2[15],15 , col = "blue", lwd = 2)
segments(ses1[16],16 , ses2[16],16 , col = "blue", lwd = 2)
