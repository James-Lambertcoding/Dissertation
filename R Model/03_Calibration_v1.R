## 03_Calibration_v1.R ---------------

## Calibration Calculations:

## 1.0 Household Utility ----------
## 1.1 Alpha_i  ---------
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

## 1.1.1 alpha i
## version 2 based on Input Output Tables






## 1.2 A_c ---------------

cal_a_c  <- total_expenditure/prod(house_ult_df[,"g_i.alpha_i"])

## 2.0 Producers ----------------

## 2.1 Beta ---------------

ig_tot_df_2 <- ig_tot_df %>% 
  left_join(ig_sum_df_2, by = "Sectors")

## create blank data frame
cal_beta <- data.frame(matrix(ncol = length(sectors), nrow= length(sectors),0))

colnames(cal_beta) <- sectors
rownames(cal_beta) <- sectors

for(i in 1:nrow(cal_beta)){
  for(j in 1:ncol(cal_beta)){
    
    
  cal_beta[i,j] <- ig_tot_df_2[i,j+1]/ig_tot_df_2[i,"total_demand"]
  }
  
}


## 2.2 Gamma -------------------

prim_2015_df <- prim_df %>% 
  filter(Year == 2015)

cal_gamma <- data.frame("Sectors" = sectors,
                        "capital_gamma" = rep(0),
                        "labour_gamma" = rep(0))

cal_gamma <- cal_gamma %>% 
  left_join(sala_df, by = "Sectors")

for(i in 1:nrow(cal_gamma)){
  
  cal_gamma[i,"capital_gamma"] <- prim_2015_df[4,i+2]/ig_tot_df_2[i,"total_demand"]
  cal_gamma[i,"labour_gamma"] <- (prim_2015_df[5,i+2]*cal_gamma[i,"salary"])/(ig_tot_df_2[i,"total_demand"]*10^6)
  
  
}

## 2.3 A_j -------------------


x_ij <- ig_tot_df_2 %>% 
  select(-Sectors, -total_demand)
rownames(x_ij) <- colnames(x_ij)


X_ij_b_ij <- x_ij 

for(i in 1:nrow(X_ij_b_ij)){
  for(j in 1:ncol(X_ij_b_ij)){
    
    X_ij_b_ij[i,j] <- x_ij[i,j] ^ cal_beta[i,j]
    
  }
  
}

cal_gamma_full <- cal_gamma %>% 
  mutate(capital = 0) %>% 
  mutate(labour = 0)

for(i in 1:nrow(cal_gamma_full)){
  
  cal_gamma_full[i,"capital"] <- prim_2015_df[4,i+2]*10^6
  cal_gamma_full[i,"labour"] <- prim_2015_df[5,i+2]
  
  
}

cal_gamma_full_2 <- cal_gamma_full %>% 
  mutate(labour_cost = labour* salary) %>% 
  mutate(capital_raised = capital ^ capital_gamma) %>% 
  mutate(labour_raised = labour_cost ^ labour_gamma)

cal_a_j <-  data.frame("Sectors" = sectors,
                       "a_j" = rep(0))

for(i in 1:nrow(cal_a_j)){
  
  cal_a_j[i,"a_j"] <- (ig_tot_df_2[i,"total_demand"]*10^6)/prod(X_ij_b_ij[,i], cal_gamma_full_2[,"capital_raised"],cal_gamma_full_2[,"labour_raised"])
  
}

## 3.0 Totals -----

m_bar <- sum(cal_gamma_full_2[,"labour_cost"])

