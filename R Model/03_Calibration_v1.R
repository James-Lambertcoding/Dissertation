## 03_Calibration_v1.R ---------------

## Calibration Calculations:

## 1.0 Household Utility ----------
## 1.1 Alpha_i ---------

house_ult_df <- data.frame("final_demand" = final_demand,
                           "demand" = rep(0),
                           "total_demand" = rep(0),
                           "cal_alpha" = rep(0),
                           "g_i^alpha_i" = rep(0))



## make temp copy of hh_df for filtering year
hh_df_alpha <- hh_df_2 %>% 
  filter(Year == Bench_year) %>% 
  select(- Year)

total_expenditure <- sum(hh_df_alpha[1,])

for(k in 1:nrow(house_ult_df)){
  
  house_ult_df[k,"demand"] <- hh_df_alpha[1,k]
  house_ult_df[k,"total_demand"] <- total_expenditure
  house_ult_df[k,"cal_alpha"] <- hh_df_alpha[1,k]/sum(hh_df_alpha[1,])
  house_ult_df[k,"g_i^alpha_i"] <- house_ult_df[k,"demand"]^house_ult_df[k,"cal_alpha"]
  
}

## 1.2 A_c ---------------

cal_a_c  <- total_expenditure



for(k in 1:length(cal_a_c)){
  
  cal_a_c[k] <- total_expenditure
  
}

