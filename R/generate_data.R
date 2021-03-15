generate_data <- function(){

N <- 2000
num_country <- 150
num_region <- 6
num_country_in_region <- rep(num_country/num_region,num_region)
num_cov <- 20
beta <- c(rep(5,10),rep(0,10))
yearLength <- 15
num_source <- 3
source_sd <- c(0.1,0.2,0.3)
rho <- 0.5 #correlation of covariates if does exist


###Covariates  PART: ignore the truth that the covariates in one country should be correlated.
corr <- T
covar_array <- array(NA,dim = c(num_cov,num_country,yearLength))
if(corr == F){
 covar_mat <- MASS::mvrnorm(num_country*yearLength,mu = rep(0,num_cov), Sigma = diag(1,nrow=num_cov))
 }else{
   rho_vec <- rep(rho,25)
   block_mat <- matrix(rho_vec,nrow = 5, ncol = 5)
   R <- matrix(0,nrow = num_cov, ncol =num_cov)
   for(i in 0:(num_cov/5-1)){ #five covariates in one group
     start <- i*5+1
     end <- i*5+5
     R[start:end,start:end] <- block_mat
   }
   diag(R) <- 1
   covar_mat <- MASS::mvrnorm(num_country*yearLength,mu = rep(0,num_cov), Sigma = R)
 }

for(i in 1:num_cov){covar_array[i,,] <- covar_mat[,i]} 
#dim(covar_array)
#summary(covar_array)
cov_ct <- matrix(NA,ncol = yearLength, nrow = num_country)
for(c in 1:num_country){
   for(t in 1:yearLength){
cov_ct[c,t] <- covar_array[,c,t] %*% beta
}}

###Intercept PART
global_mean <- 2.3
global_sd <- 0.1
regional_sd <- 0.2
regional_mean <- rnorm(num_region,global_mean,global_sd)
country_mean <- list()
for(i in 1:num_region){
country_mean[[i]] <- rnorm(num_country_in_region[i],regional_mean[i],regional_sd)}
country_mean <- unlist(country_mean)
getr_c <- rep(c(1:6),num_country_in_region)

###Smoothing PART
tau_delta <- 0.05
splines.data <- getSplinesData(yearLength,I=1,order=1, degree = 2)
K <- splines.data$K
B_tk <- splines.data$B.tk

alpha_ck <- matrix(NA,ncol = K,nrow = num_country)
alpha_ck[,1] <- rnorm(num_country,0, tau_delta)
for(c in 1:num_country){
for (k in 2:K){
   alpha_ck[c,k] <- rnorm(1,alpha_ck[c,(k-1)], tau_delta)
}}

delta_ct <- matrix(NA,ncol = yearLength,nrow = num_country)
for (t in 1:yearLength){
   delta_ct[,t] <-  alpha_ck%*%B_tk[t,]
}
#dim(delta_ct)
#plot(ylim = c(-0.5,0.5),delta_ct[1,],type = "l")
#for(c in 1:num_country){
#   lines(delta_ct[c,])
#}

###Source type error PART
mu_ct <- matrix(NA,ncol = yearLength,nrow = num_country)
mu_ct <- country_mean + delta_ct +cov_ct
sam <- sort(sample(yearLength*num_country,N))
location <- matrix(NA,ncol = yearLength,nrow = num_country)
location[sam] <- 1:N
mu_i <-mu_ct[sam]
getc_i <- c()
gett_i <- c()
y_i <- c()
getj_i <- sample(num_source,N,replace = T, prob = c(0.7,0.2,0.1))
for(i in 1:N){
getc_i[i] <- ifelse(which(location==i) %% num_country!=0, which(location==i) %% num_country,num_country)
gett_i[i] <- ceiling(which(location==i) / num_country)
y_i[i] <- rnorm(1,mu_i[i],source_sd[getj_i[i]])
}
datatype1_i <- as.numeric(getj_i ==1)
datatype2_i <- as.numeric(getj_i ==2)
datatype3_i <- as.numeric(getj_i ==3)

stan_data <- list(N = N, numcov = num_cov, numcountry = num_country, numregion = num_region, numsource = num_source,
                  yearLength = yearLength, estyears = 1:yearLength, Y = y_i, getc_i = getc_i, getr_c = getr_c, gett_i = gett_i,
                  covar_array = covar_array, H = splines.data$H, Z_th = splines.data$Z.tk, datatype1_i = datatype1_i, 
                  datatype2_i=datatype2_i,datatype3_i=datatype3_i)

stan_data$getitrain_k <- seq(1, N)
stan_data$ntrain <- length(stan_data$getitrain_k)
return(stan_data)
}
