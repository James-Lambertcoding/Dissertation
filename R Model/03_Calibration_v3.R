## 03_Calibration_v2.R ---------------
## Notes on version of calibration:

## this version does not include:
## saving rate
## government
## exports
## imports

## Therefore required to ensure that the income (wages and rent on capital equal the output)
## this will be a function that takes the inputs of Z and creates the calibrated factors

## Calibration Calculations:

##  0.0 Function  ----------------

function_calibration_3 <- function(price,             # vector
                                   labour,            # vector 
                                   capital,           # vector 
                                   inter_goods_coef,  # matrix
                                   inter_totals,      # vector
                                   salary,            # vector
                                   interest_rate,     # number 
                                   demand_ratio,      # vector
                                   sectors            # vector - name
){
  ## 1.1 Gamma Part 1 -------------
  cal_gamma <- data.frame("Sectors" = sectors,
                          "capital" = capital,
                          "capital_price" = rep(interest_rate),
                          "capital_cost" = rep(0),
                          "capital_gamma" = rep(0),
                          "labour" = labour,
                          "labour_price" = salary,
                          "labour_cost" = rep(0),
                          "labour_gamma" = rep(0))
  
  
  for(i in 1:nrow(cal_gamma)){
    
    cal_gamma[i,"capital_cost"] <- cal_gamma[i,"capital"]*cal_gamma[i,"capital_price"]
    cal_gamma[i,"labour_cost"] <- cal_gamma[i,"labour"]*cal_gamma[i,"labour_price"]
    
  }
  
  ## 1.2 M_Bar ----------------
  
  ## m_bar is the sum of the costs of the primary inputs
  
  m_bar <- sum(cal_gamma[,"capital_cost"])+sum(cal_gamma[,"labour_cost"])
  
  inter_good_sum <- sum(inter_totals) 
  
  hh_exp_total <- m_bar - inter_good_sum
  
  cal_gamma <- cal_gamma %>% 
    mutate(int_tots =inter_totals) 
  
  ## 1.3 Beta ---------------
  ## include the intermediate goods
  
  
  ## create blank data frame
  cal_beta <- data.frame(matrix(ncol = length(sectors), nrow= length(sectors),0))
  
  colnames(cal_beta) <- sectors
  rownames(cal_beta) <- sectors
  
  for(i in 1:nrow(cal_beta)){
    for(j in 1:ncol(cal_beta)){
      
      
      cal_beta[i,j] <- (inter_goods_coef[i,j]*price[i])/(price[j])
      
    }
    
  }
  
  ## 1.4 Gamma Part 2 -----------------
  
  cal_gamma <- cal_gamma %>% 
    mutate(total_supply = cal_sum_aut[,"total_supply"]) %>% 
    mutate(labour_gamma_2 = 0) %>% 
    mutate(capital_gamma_2 = 0)
  
  ## 1.5 alpha----------
  
  
  
  cal_HH_utl <- data.frame("Sectors" = sectors,
                           "demand_ratios" = rep(0),
                           "hh_demand_totals"= rep(hh_exp_total) ) 
  
  for(i in 1:nrow(cal_HH_utl)){
    
    cal_HH_utl[i,"demand_ratios"] <- demand_ratio[i]
    
  }
  
  cal_HH_utl <- cal_HH_utl %>% 
    mutate(hh_exp = demand_ratio * hh_exp_total) %>% 
    mutate(price = price) %>% 
    mutate(hh_cost = price*hh_exp) %>% 
    mutate(alpha_i = hh_cost/sum(hh_cost)) %>% 
    mutate(demand_raised = hh_cost ^ alpha_i) %>% 
    mutate(A_c = sum(hh_cost)/prod(demand_raised)) %>% 
    mutate(int_tots =inter_totals) %>% 
    mutate(total_supply = int_tots + hh_exp) %>% 
    mutate(total_supply_cost = total_supply*price)
  
  ## 1.6 Gamma Part 3 -----------------
  
  for(i in 1:nrow(cal_gamma)){
    
    cal_gamma[i,"capital_gamma"] <- cal_gamma[i,"capital_cost"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"labour_gamma"] <- cal_gamma[i,"labour_cost"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"capital_gamma_2"] <- cal_gamma[i,"capital"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"labour_gamma_2"] <- cal_gamma[i,"labour"]/cal_gamma[i,"total_supply"]
  
  }

  
  
  ## work v_j ^ gamma_j
  cal_gamma_2 <- cal_gamma %>% 
    mutate(capital_raised = capital_cost ^ capital_gamma) %>% 
    mutate(labour_raised = labour_cost ^ labour_gamma) %>% 
    mutate(capital_raised_2 = capital ^ capital_gamma) %>% 
    mutate(labour_raised_2 = labour ^ labour_gamma) 
  

  
  ## 1.6 Vj_bar ---------------------------
  
  cal_vj_bar <- cal_gamma_2 %>% 
    select(Sectors,capital,labour) %>% 
    mutate(vj_bar = capital + labour)
  
  
  ## 1.7 Misc ------------
  
  primary_price_test <- data.frame("capital_price" = rep(interest_rate,length(sectors)),
                                   "labour_price" = salary)
  
  
  ## 1.8 RE-CALIBRATION ------------
  
  re_cal <- data.frame("sectors" = sectors,
                       "total_supply" = cal_gamma_2[,"total_supply"],
                       "capital_cost"= cal_gamma_2[,"capital_cost"],
                       "capital" = cal_gamma_2[,"capital"],
                       "labour_cost"= cal_gamma_2[,"labour_cost"],
                       "labour" = cal_gamma_2[,"labour"],
                       "labour_price" = cal_gamma_2[,"labour_price"],
                       "int_good_one" = rep(0)) %>% 
    mutate(capital_one = capital_cost/total_supply) %>% 
    mutate(labour_one = labour_cost/total_supply)
  
  for(i in 1:nrow(re_cal)){
    
    re_cal[i,"int_good_one"] <- sum(inter_goods_coef[,i])
    
  }
  
  ## 1.81 New Labour Amounts -----------
  re_cal <- re_cal %>% 
    mutate(total_cost = capital_one + labour_one+int_good_one) %>% 
    mutate(labour_redo = 1- capital_one - int_good_one) %>% 
    mutate(labour_ratio = labour_redo/labour_one) %>% 
    mutate(new_labour = labour*labour_ratio) %>% 
    mutate(new_labour_cost = new_labour*labour_price) %>% 
    mutate(old_ratio = 0) %>% 
    mutate(new_hh_ratio = (1+int_good_one))
  
  new_m_bar = sum(re_cal[,"new_labour_cost"] + re_cal[,"capital_cost"])
  
  for(i in 1:nrow(re_cal)){
    
    re_cal[i,"old_ratio"] <- cal_HH_utl[i,"demand_ratios"]
    re_cal[i,"new_hh_ratio"] <- re_cal[i,"new_hh_ratio"]* cal_HH_utl[i,"demand_ratios"]

  }
  
  new_hh_total_ratio <- sum(re_cal[,"new_hh_ratio"])
  
  re_cal <- re_cal %>% 
    mutate(new_hh_ratio_2 = old_ratio/new_hh_total_ratio) %>% 
    mutate(new_demand = new_hh_ratio_2*new_m_bar) %>% 
    
  ## 1.82 new Alpha ---------------  
    mutate(new_alphi = new_demand/sum(new_demand)) %>% 
    mutate(demand_raise_alpha = new_demand^new_alphi) %>% 
  ## 1.83 new A_c -----------
    mutate(new_a_c = sum(new_demand)/prod(demand_raise_alpha)) %>% 
    mutate(total_supply = 0)
  

  
  ## new production totals
  
  new_int_goods <- inter_goods_coef
  
  for(i in 1: nrow(new_int_goods)){
    
    for(j in 1:ncol(new_int_goods)){
      
      new_int_goods[i,j] <- new_int_goods[i,j] * re_cal[j,"new_demand"]
      
    }
    
  }
    
  total_supply_df <- data.frame("Sectors" = sectors,
                                "total_supply" = rep(0)) 
  
  for(i in 1:nrow(total_supply_df)){
    
    total_supply_df[i,"total_supply"] <- sum(new_int_goods[i,]) + re_cal[i,"new_demand"]
    
  }
  
  re_cal[,"total_supply"] <- total_supply_df[,"total_supply"]
  
  ## 1.84 re-gamma -----------
  
  re_cal <- re_cal %>% 
    mutate(new_labour_cost = new_labour*salary) %>% 
    mutate(new_gamma_labour = new_labour_cost/total_supply) %>% 
    mutate(new_gamma_capital = capital_cost/total_supply) %>% 
    mutate(new_labour_raised = new_labour^new_gamma_labour) %>% 
    mutate(new_capital_raised = capital^new_gamma_capital)  
    
  
  ## 1.85 New A_j ------------
  
  
  ## x_ij raised to gamma
  
  x_ij <- new_int_goods
  
  rownames(x_ij) <- colnames(x_ij)
  
  X_ij_b_ij <- x_ij 
  
  for(i in 1:nrow(X_ij_b_ij)){
    for(j in 1:ncol(X_ij_b_ij)){
      
      X_ij_b_ij[i,j] <- x_ij[i,j] ^ cal_beta[i,j]
      
    }
    
  }
  


  new_cal_a_j <-  data.frame("Sectors" = sectors,
                         "a_j" = rep(0))
  
  for(i in 1:nrow(cal_a_j)){
    
    new_cal_a_j[i,"a_j"] <- (re_cal[i,"total_supply"])/(prod(X_ij_b_ij[,i])* re_cal[i,"new_capital_raised"]*re_cal[i,"new_labour_raised"])
    
  }
  
  
  ## 1.9 outputs -------------
  cal_list <- list()

  ## put items in the list
  ## Alpha
  cal_list[[1]] <- re_cal[,"new_alphi"]
  ## A_C
  cal_list[[2]] <- re_cal[,"new_a_c"]
  ## Beta
  cal_list[[3]] <- cal_beta
  ## Gammas
  cal_list[[4]] <- re_cal[,c("new_gamma_capital","new_gamma_labour")]
  ## A_j
  cal_list[[5]] <- new_cal_a_j[,"a_j"]
  ## Primary Factors
  cal_list[[6]] <- c(sum(re_cal[,"new_labour"]),sum(re_cal[,"capital"]))
  ## Price
  cal_list[[7]] <- price
  ## Total Supply
  cal_list[[8]] <- total_supply_df[,"total_supply"]
  cal_list[[9]] <- re_cal[,"new_labour"]
  cal_list[[10]] <- salary
  cal_list[[11]] <- capital
  cal_list[[12]] <- primary_price_test
  cal_list[[13]] <- new_m_bar
  cal_list[[14]] <- x_ij
  cal_list[[15]] <- X_ij_b_ij
  cal_list[[16]] <-


  return(cal_list)
  
}

test_cal_first[[3]][,23]

test_cal_first <- function_calibration_3(price = price_cal_test,
                                         labour = labour_cal_test, 
                                         capital = capital_cal_test,
                                         inter_goods_coef = ig_coef_sum,
                                         inter_totals = inter_total_cal_test,
                                         salary = salary_cal_test,
                                         interest_rate = interest_rate_cal_test,
                                         demand_ratio = demand_ratios_cal_test,
                                         sectors = sectors)
