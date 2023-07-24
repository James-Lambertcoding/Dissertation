## 03_Calibration_v1.R ---------------

## Calibration Calculations:

## 1.0 Household Utility ----------
## 1.1 Alpha_i  ---------


## 1.1.1 alpha i
## version 2 based on Input Output Tables

demand_hh_df<- ig_sum_df %>% 
  select(Sector, Households) %>% 
  mutate(alph_i = Households/sum(Households)) %>% 
  mutate(a_c = sum(Households)/prod(Households^alph_i))




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
                        "labour_gamma" = rep(0)) %>% 
  left_join(sala_df, by = "Sectors") %>% 
  mutate(capital_price = Interst_rate) %>%
  mutate( capital_cost = 0)  %>% 
  mutate(capital = 0) %>% 
  mutate(labour = 0) %>% 
  mutate(labour_cost = 0)


cal_gamma_full <- cal_gamma

for(i in 1:nrow(cal_gamma_full)){
  
  cal_gamma_full[i,"capital"] <- prim_2015_df[4,i+2]*10^6
  cal_gamma_full[i,"labour"] <- prim_2015_df[5,i+2]
  cal_gamma_full[i,"capital_cost"] <- cal_gamma_full[i,"capital"]*cal_gamma_full[i,"capital_price"]
  cal_gamma_full[i,"labour_cost"] <- cal_gamma_full[i,"labour"]*cal_gamma_full[i,"salary"]
  
  
  
}

for(i in 1:nrow(cal_gamma)){
  
  cal_gamma_full[i,"capital_gamma"] <- cal_gamma_full[i,"capital_cost"]/(ig_tot_df_2[i,"total_demand"]*10^6)
  cal_gamma_full[i,"labour_gamma"] <- cal_gamma_full[i,"labour_cost"]/(ig_tot_df_2[i,"total_demand"]*10^6)
  
  
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



cal_gamma_full_2 <- cal_gamma_full %>% 
  mutate(capital_raised = capital_cost ^ capital_gamma) %>% 
  mutate(labour_raised = labour_cost ^ labour_gamma)

cal_a_j <-  data.frame("Sectors" = sectors,
                       "a_j" = rep(0))

for(i in 1:nrow(cal_a_j)){
  
  cal_a_j[i,"a_j"] <- (ig_tot_df_2[i,"total_demand"]*10^6)/prod(X_ij_b_ij[,i], cal_gamma_full_2[,"capital_raised"],cal_gamma_full_2[,"labour_raised"])
  
}

## 3.0 Totals -----

m_bar <- sum(cal_gamma_full_2[,"labour_cost"]) + sum(cal_gamma_full_2[,"capital_cost"])

