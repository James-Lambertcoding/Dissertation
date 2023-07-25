## 03_Calibration_v1.R ---------------
## Notes on version of calibration:

## this version does not include:
## saving rate
## government
## exports
## imports

## Therefore required to ensure that the income (wages and rent on capital equal the output)
## this ensures the price is unity in the benchmark case

## Calibration Calculations:

## 1.0 Producers ----------------
## 1.1 Gamma Part 1----------------
## start by understanding the primary inputs to the economy

## part 1 collate the data

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


for(i in 1:nrow(cal_gamma)){
  
  cal_gamma[i,"capital"] <- prim_2015_df[4,i+2]*10^6
  cal_gamma[i,"labour"] <- prim_2015_df[5,i+2]
  cal_gamma[i,"capital_cost"] <- cal_gamma[i,"capital"]*cal_gamma[i,"capital_price"]
  cal_gamma[i,"labour_cost"] <- cal_gamma[i,"labour"]*cal_gamma[i,"salary"]
  
}

## 1.2 M_Bar ----------------

## m_bar is the sum of the costs of the primary inputs

m_bar <- sum(cal_gamma[,"capital_cost"])+sum(cal_gamma[,"labour_cost"])

## m_bar is equal to total final demand

ig_sum_df_temp <- ig_sum_df %>% 
  select(Sector, Households) %>% 
  rename(sectors = Sector) %>% 
  mutate(ratio = Households/sum(Households)) %>% 
  mutate(total_exp = m_bar) %>% 
  mutate(HH_exp = ratio*total_exp)

## include the intermediate goods

ig_tot_df_temp <- ig_tot_df %>% 
  select(-Sectors)

rownames(ig_tot_df_temp) <- colnames(ig_tot_df_temp)

ig_tot_df_temp <- ig_tot_df_temp %>% 
  mutate(in_tot = 0)

for(i in 1:nrow(ig_tot_df_temp)){
  
  ig_tot_df_temp[i, "in_tot"] <- sum(ig_tot_df_temp[i,1:(ncol(ig_tot_df_temp)-1)])
  
}

ig_sum_df_temp_2 <- ig_sum_df_temp %>% 
  mutate(in_tot = ig_tot_df_temp[, "in_tot"]* 10^6) %>% 
  mutate(total_supply = HH_exp+in_tot)


## 1.3 Gamma Part 2 -----------------

cal_gamma <- cal_gamma %>% 
  mutate(total_supply = ig_sum_df_temp_2[,"total_supply"]) 


## this needs to change
for(i in 1:nrow(cal_gamma)){
  
  cal_gamma[i,"capital_gamma"] <- cal_gamma[i,"capital_cost"]/cal_gamma[i,"total_supply"]
  cal_gamma[i,"labour_gamma"] <- cal_gamma[i,"labour_cost"]/cal_gamma[i,"total_supply"]
  
  
}


cal_gamma
## 1.4 Beta ---------------

ig_tot_df_2 <- ig_tot_df %>% 
  left_join(cal_gamma[,c("Sectors","total_supply")], by = "Sectors")

## increase ig_tot_df_2 inputs to millions

for(j in 2:(ncol(ig_tot_df_2)-1)){
  
  ig_tot_df_2[,j] <- ig_tot_df_2[,j]*10^6
  
}

## create blank data frame
cal_beta <- data.frame(matrix(ncol = length(sectors), nrow= length(sectors),0))

colnames(cal_beta) <- sectors
rownames(cal_beta) <- sectors

for(i in 1:nrow(cal_beta)){
  for(j in 1:ncol(cal_beta)){
    
    
  cal_beta[i,j] <- ig_tot_df_2[i,j+1]/ig_tot_df_2[i,"total_supply"]
  }
  
}


## 1.5 A_j -------------------


x_ij <- ig_tot_df_2 %>% 
  select(-Sectors, -total_supply)

rownames(x_ij) <- colnames(x_ij)


X_ij_b_ij <- x_ij 

for(i in 1:nrow(X_ij_b_ij)){
  for(j in 1:ncol(X_ij_b_ij)){
    
    X_ij_b_ij[i,j] <- x_ij[i,j] ^ cal_beta[i,j]
    
  }
  
}

## work v_j ^ gama_j


cal_gamma_2 <- cal_gamma %>% 
  mutate(capital_raised = capital_cost ^ capital_gamma) %>% 
  mutate(labour_raised = labour_cost ^ labour_gamma)

cal_a_j <-  data.frame("Sectors" = sectors,
                       "a_j" = rep(0))

for(i in 1:nrow(cal_a_j)){
  
  cal_a_j[i,"a_j"] <- (cal_gamma_2[i,"total_supply"])/prod(X_ij_b_ij[,i], cal_gamma_2[i,"capital_raised"],cal_gamma_2[i,"labour_raised"])
  
}

## 1.6 Vj_bar ---------------------------

cal_vj_bar <- cal_gamma_2 %>% 
  select(Sectors,capital,labour) %>% 
  mutate(vj_bar = capital + labour)


## 2.0 Household Utility ----------
## 2.1 Alpha_i  ---------

cal_HH_utl <- ig_sum_df_temp_2 %>% 
  select(sectors, HH_exp) %>% 
  rename(Sectors = sectors) %>% 
  mutate(alpha_i = HH_exp/sum(HH_exp)) %>% 
  mutate(demand_raised = HH_exp ^ alpha_i) %>% 
  mutate(A_c = sum(HH_exp)/prod(demand_raised))




