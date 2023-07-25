## 99 Archive ---------------

## version 1 based on household expenditure data

house_ult_df <- data.frame("final_demand" = final_demand,
                           "demand" = rep(0),
                           "total_demand" = rep(0),
                           "cal_alpha" = rep(0),
                           "g_i.alpha_i" = rep(0))



## make temp copy of hh_df for filtering year
hh_df_alpha <- hh_df_2 %>% 
  filter(Year == Bench_year) %>% 
  select(- Year)

total_expenditure <- sum(hh_df_alpha[1,])

for(k in 1:nrow(house_ult_df)){
  
  house_ult_df[k,"demand"] <- hh_df_alpha[1,k]
  house_ult_df[k,"total_demand"] <- total_expenditure
  house_ult_df[k,"cal_alpha"] <- hh_df_alpha[1,k]/sum(hh_df_alpha[1,])
  house_ult_df[k,"g_i.alpha_i"] <- house_ult_df[k,"demand"]^house_ult_df[k,"cal_alpha"]
  
}

## rework calibration


## 1.1.1 alpha i
## version 2 based on Input Output Tables



ig_tot_df_cal <- ig_tot_df %>% 
  mutate(int_sum = 0)

rownames(ig_tot_df_cal) <- colnames(ig_tot_df_cal)[2:(ncol(ig_tot_df_cal)-1)]

ig_tot_df_cal <- ig_tot_df_cal %>% 
  select(-Sectors)

for (j in 1:ncol(ig_tot_df_cal)) {
  
  ig_tot_df_cal[,j] <- as.numeric(ig_tot_df_cal[,j])
  
}

for(i in 1:nrow(ig_tot_df_cal)){
  
  ig_tot_df_cal[i,"int_sum"] <- sum(ig_tot_df_cal[i,1:(ncol(ig_tot_df_cal)-1)])
  
}




demand_hh_df<- ig_sum_df %>% 
  select(Sector, Households) %>% 
  mutate(alph_i = Households/sum(Households)) %>% 
  mutate(a_c = sum(Households)/prod(Households^alph_i))



## 1.2 A_c ---------------

cal_a_c  <- total_expenditure/prod(house_ult_df[,"g_i.alpha_i"])