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

function_calibration <- function(price,             # vector
                                 labour,            # vector 
                                 capital,           # vector 
                                 inter_goods,       # matrix
                                 inter_totals,      # vector
                                 salary,            # vector
                                 interest_rate,     # number 
                                 demand_ratio,     # vector
                                 sectors            # vector - name
                                 ){
  
  cal_list <- list()
  
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
  
  
  ## 1.4 Beta ---------------
  ## include the intermediate goods

  
  ## create blank data frame
  cal_beta <- data.frame(matrix(ncol = length(sectors), nrow= length(sectors),0))
  
  colnames(cal_beta) <- sectors
  rownames(cal_beta) <- sectors
  
  for(i in 1:nrow(cal_beta)){
    for(j in 1:ncol(cal_beta)){
      
      
      cal_beta[i,j] <- (inter_goods[i,j]*price[i])/(inter_totals[j]*price[j])
      
    }
    
  }
  
  
  ## 1.3 Gamma Part 2 -----------------
  
  cal_gamma <- cal_gamma %>% 
    mutate(total_supply = cal_sum_aut[,"total_supply"]) %>% 
    mutate(labour_gamma_2 = 0) %>% 
    mutate(capital_gamma_2 = 0)
  
  ## 1.7 alpha----------
  
  
  
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
  
  
  ## this needs to change
  for(i in 1:nrow(cal_gamma)){
    
    cal_gamma[i,"capital_gamma"] <- cal_gamma[i,"capital_cost"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"labour_gamma"] <- cal_gamma[i,"labour_cost"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"capital_gamma_2"] <- cal_gamma[i,"capital"]/cal_gamma[i,"total_supply"]
    cal_gamma[i,"labour_gamma_2"] <- cal_gamma[i,"labour"]/cal_gamma[i,"total_supply"]
    
    
    
  }
  
  ## 1.5 A_j -------------------
  
  
  x_ij <- inter_goods
  
  rownames(x_ij) <- colnames(x_ij)
  
  X_ij_b_ij <- x_ij 
  
  for(i in 1:nrow(X_ij_b_ij)){
    for(j in 1:ncol(X_ij_b_ij)){
      
      X_ij_b_ij[i,j] <- x_ij[i,j] ^ cal_beta[i,j]
      
    }
    
  }
  
  ## work v_j ^ gamma_j
  cal_gamma_2 <- cal_gamma %>% 
    mutate(capital_raised = capital_cost ^ capital_gamma) %>% 
    mutate(labour_raised = labour_cost ^ labour_gamma) %>% 
    mutate(capital_raised_2 = capital ^ capital_gamma) %>% 
    mutate(labour_raised_2 = labour ^ labour_gamma) 
    
  
  cal_a_j <-  data.frame("Sectors" = sectors,
                         "a_j" = rep(0))
  
  for(i in 1:nrow(cal_a_j)){
    
    cal_a_j[i,"a_j"] <- (cal_gamma_2[i,"total_supply"])/(prod(X_ij_b_ij[,i])* cal_gamma_2[i,"capital_raised_2"]*cal_gamma_2[i,"labour_raised_2"])
    
  }
  
  ## 1.6 Vj_bar ---------------------------
  
  cal_vj_bar <- cal_gamma_2 %>% 
    select(Sectors,capital,labour) %>% 
    mutate(vj_bar = capital + labour)
  
  
  ## 1.7 Misc ------------
  
  primary_price_test <- data.frame("capital_price" = rep(interest_rate,length(sectors)),
                                   "labour_price" = salary)
  ## 1.7 outputs -------------
  ## put items in the list
  cal_list[[1]] <- cal_HH_utl[,"alpha_i"]
  cal_list[[2]] <- cal_HH_utl[,"A_c"]
  cal_list[[3]] <- cal_beta
  cal_list[[4]] <- cal_gamma_2[,c("capital_gamma","labour_gamma")]
  cal_list[[5]] <- cal_a_j[,"a_j"]
  cal_list[[6]] <- c(sum(cal_gamma_2[,"labour"]),sum(cal_gamma_2[,"capital"]))
  cal_list[[7]] <- price
  cal_list[[8]] <- cal_HH_utl[,"total_supply"]
  cal_list[[9]] <- labour
  cal_list[[10]] <- salary
  cal_list[[11]] <- capital
  cal_list[[12]] <- primary_price_test
  cal_list[[13]] <- m_bar

  
  return(cal_list)
  
}


## 2.0 test function ---------








