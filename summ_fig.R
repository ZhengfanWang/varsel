ci95 <- readRDS("sumres/select/ci95_res.rds")
hss95 <- readRDS("sumres/select/hss95_res.rds")
hssmax <- readRDS("sumres/select/hssmax_res.rds")
proj <- readRDS("sumres/select/proj_res_cov.rds")

#translate proj list to selection matrix

num_simu <- 30
proj_cov <- matrix(NA, ncol = 16, nrow = num_simu)
for(i in 1:num_simu){
  proj_cov[i,proj[[i]][1:9]] <- TRUE
}

proj_cov[is.na(proj_cov)] <- FALSE
proj_cov


#translate waic list to selection matrix
waic_cov <- matrix(NA, ncol = 16, nrow = num_simu)
for(i in 1:num_simu){
  waic_cov[i,proj[[i]][1:8]] <- TRUE
}

waic_cov[is.na(waic_cov)] <- FALSE
waic_cov
#

cal_acc(ci95,truth = c(rep(TRUE,9),rep(FALSE,7)))
cal_acc(proj_cov,truth = c(rep(TRUE,9),rep(FALSE,7)))
cal_acc(waic_cov,truth = c(rep(TRUE,9),rep(FALSE,7)))
cal_acc(hss95,truth = c(rep(TRUE,9),rep(FALSE,7)))

pdf_name <- paste0("fig/summ_select.pdf")
pdf(pdf_name, width = 12, height = 8)
num_method <- 4
plot(-10, -10, xlim=c(0+2, 16*num_method-2), ylim=c(0,1*num_simu-1), yaxt = "n", ylab = "model index",
     xaxt = "n", xlab="covariates index", main = "summary of selection in setting 2")

vline <- seq(0,16*num_method, by = 4)
hline <- seq(0,1*num_simu, by =1)

axis(1, at = vline[1:16]+2 ,labels= 1:16, las=1) 
axis(2, at = hline[1:30]+0.5, labels=1:30, las=1,cex.axis=0.8)  

true <- c(rep(TRUE,9),rep(FALSE,7))
for(i in 1:num_simu){
  for(k in 1:9){
    xleft <- (k-1)*4
    ybot <- i-1
    xright <- 4*k
    ytop <- i
    if(hss95[i,k] == true[k])(col = "blue")else(col = "red")
    rect(xleft,ybot,xright,ytop, col = col)    
    if(waic_cov[i,k] == true[k])(col = "blue")else(col = "red")
    rect(xleft+1,ybot,xright+1,ytop, col = col)  
    if(ci95[i,k] == true[k])(col = "blue")else(col = "red")
    rect(xleft+2,ybot,xright+2,ytop, col = col)  
    if(proj_cov[i,k] == true[k])(col = "blue")else(col = "red")
    rect(xleft+3,ybot,xright+3,ytop, col = col)  
  }
  for(k in 10:16){
    xleft <- (k-1)*4
    ybot <- i-1
    xright <- 4*k
    ytop <- i
    if(hss95[i,k] == true[k])(col = "chartreuse4")else(col = "brown")
    rect(xleft,ybot,xright,ytop, col = col)    
    if(waic_cov[i,k] == true[k])(col = "chartreuse4")else(col = "brown")
    rect(xleft+1,ybot,xright+1,ytop, col = col)  
    if(ci95[i,k] == true[k])(col = "chartreuse4")else(col = "brown")
    rect(xleft+2,ybot,xright+2,ytop, col = col)  
    if(proj_cov[i,k] == true[k])(col = "chartreuse4")else(col = "brown")
    rect(xleft+3,ybot,xright+3,ytop, col = col)  
  }
}
abline(v=vline, h = hline, col = "grey", lwd = "3")
abline(v=36, col = "black", lwd = "5")
abline(v=28, col = "black", lwd = "5")
text(c(1:64)-0.5, rep(-0.5,16*num_method-2),rep(c("H","L","I","P"),16))
dev.off()
