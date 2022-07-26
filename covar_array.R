
endyear <- 2020
estyears <- seq(2000,endyear)
covarset.raw <- read.csv("data/SBR_IGME_covariates_2020-09-04.csv")


int_cov <- c("nmr","gni_sm","lbw_sm","mean_edu_f_sm","csec_sm",
             "anc4_sm","pab_sm","abr_sm","urban","gini_sm",
             "sab","anc1_sm","mmr","pfpr","gdp",
             "gfr")



covarset <- covarset.raw %>% select(c("iso3","year",int_cov)) %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso") %>% 
  filter(year>=2000) %>% 
  mutate(gni_sm = log(gni_sm),
         nmr = log(nmr),
         lbw_sm = log(lbw_sm)) %>% 
  arrange(iso,year) 
covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, dataset = covarset)