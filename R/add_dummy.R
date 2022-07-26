

add_dummy <- function(covar_array, num_dummy){
num_cov <- dim(covar_array)[1]
num_country <- dim(covar_array)[2]
year_length <- dim(covar_array)[3]
store <- array(NA,dim = c(16+num_dummy,195,21))
store[1:num_cov,,] <- covar_array
store[num_cov+1:num_dummy,,] <- rnorm(num_country*year_length,0,1)
return(store)
}
