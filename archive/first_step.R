#zf simulation to study the updates of lambdas

hs <- readRDS("rdsoutput/new/regHS_nval_res202000311.rds")
hs.array <- rstan::extract(hs)
bias_dt_i <- apply(hs.array$bias_dt_i,2,median)
int_c <- apply(hs.array$gamma_c,2,median)

## data generating process
endyear <- 2020
estyears <- seq(2000,endyear)
numcov <- 16
numcountry <- 195
numregion <- 6
numsource <- 4
source_type_sd <- c(0.02,0.02,0.25,0.12)
#sd for each observations(including source type, sampling error)

covarset.raw <- read.csv("input/covar/sbr_igme_covariates_20191202.csv")
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")


covarset <- covarset.raw %>% select(c("iso3","year",int_cov)) %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso") %>% 
  filter(year>=2000) %>% 
  mutate(gni_sm = log(gni_sm),
         nmr = log(nmr),
         lbw_sm = log(lbw_sm)) %>% 
  arrange(iso,year) 
covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, dataset = covarset)

beta <- apply(hs.array$beta,2,median)
shrinkage <- c("in","in","in","unsure","in",
               "out","out","out","out","unsure",
               "out","unsure","out","unsure","out","out")



#data for model result
sbr2018 <- readRDS("output/data_for_model.rds") 

#-------------------------------#
#   definition adjustment       #
#-------------------------------#
#definition adjustment output
def_adj_output <- readRDS("output/def_adj_res.rds")

#data for model result
sbr2018 <- readRDS("output/data_for_model.rds") 
definition_fac <- c("ge28wks",paste0(unique(def_adj_output$definition_rv[!is.na(def_adj_output$def_bias)])))
sbr2018$definition_rv <- factor(sbr2018$definition_rv, levels = definition_fac)

sbr2018 <- right_join(def_adj_output,sbr2018,by = c("definition_rv","lmic")) 

sbr2018$def_bias <- ifelse(is.na(sbr2018$def_bias),0,sbr2018$def_bias)
sbr2018$def_sd <- ifelse(is.na(sbr2018$def_sd),0,sbr2018$def_sd)

sbr2018$definition_rv <- factor(sbr2018$definition_rv, levels = definition_fac)
getd.i <- as.numeric(sbr2018$definition_rv)

#-------------------------------#
#   source type                 #
#-------------------------------#
sbr2018$source <- droplevels(sbr2018$source)
sbr2018$source2 <- as.numeric(sbr2018$source)
datatype1.i <- ifelse(sbr2018$source2==1,1,0)   # admin
datatype2.i <- ifelse(sbr2018$source2==2,1,0)   # HMIS
datatype3.i <- ifelse(sbr2018$source2==3,1,0)   # subnat.lr
datatype4.i <- ifelse(sbr2018$source2==4,1,0)   # survey
getj.i <- sbr2018$source2

#-------------------------------#
#   Other                       #
#-------------------------------# 
N <- dim(sbr2018)[1]
var_i = sbr2018$SE.logsbr^2 + sbr2018$def_sd^2 
getc.i <- sbr2018$country_idx
getr.c <- countryRegionList$sdg
gett.i<- sbr2018$year-estyears[1]+1
yearLength <- length(estyears)

stan.data <- list(var_i = var_i, beta = beta, source_type_sd = source_type_sd, intercept_c = int_c,
                 covar_array = covar_array, bias_dt_i = bias_dt_i,
                 getj_i = getj.i, gett_i = gett.i, getc_i = getc.i,getr_c = getr.c,
                 datatype1_i = datatype1.i, datatype2_i = datatype2.i, datatype3_i = datatype3.i,datatype4_i=datatype4.i,
                 N = N, numcountry = max(getc.i), numregion = max(getr.c), estyears = estyears, yearLength = yearLength,
                 numcov = numcov, numsource = max(getj.i))
saveRDS(stan.data,"output/forsimu.rds")


