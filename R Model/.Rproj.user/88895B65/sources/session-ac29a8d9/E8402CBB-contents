## 04_CGE_Functions.R

## 1.0 General Equilibrium Point Functions -----------------


## 2.0 Test Data--------------

function_test_delta_2 <- function(test_function_cal,runs){
  
  ## 2.1 test_C ----------------------
  test_c <- function_delta_c_i(alpha = test_function_cal[[1]],
                               beta = test_function_cal[[3]],
                               price = test_function_cal[[7]],
                               output = test_function_cal[[8]],
                               labour = test_function_cal[[9]],
                               salary = test_function_cal[[10]],
                               capital = test_function_cal[[11]], 
                               interest_rate = test_function_cal[[15]])
  
  ## 2.2 test_F ----------------------
  test_F <- function_delta_F_f(gamma_capital =  test_function_cal[[4]][,1],
                               gamma_labour = test_function_cal[[4]][,2],
                               price = test_function_cal[[7]],
                               output = test_function_cal[[8]],
                               capital_price = test_function_cal[[12]][,1],
                               labour_price = test_function_cal[[10]],
                               capital =  test_function_cal[[11]],
                               labour = test_function_cal[[9]])
  
  ## 2.2 test_pi ----------------------
  
  test_pi <- function_delta_pi_j(price = test_function_cal[[7]],
                                 A_j = test_function_cal[[5]],
                                 beta = test_function_cal[[3]],
                                 primary_costs = test_function_cal[[12]],
                                 gamma_capital =  test_function_cal[[4]][,1],
                                 gamma_labour = test_function_cal[[4]][,2],
                                 runs = runs)
  
  ## 2.4 test_m ----------------------
  test_m <- function_delta_m(salary = test_function_cal[[10]],
                             labour = test_function_cal[[9]],
                             capital = test_function_cal[[11]],
                             interest_rate = test_function_cal[[15]],
                             m_tot = test_function_cal[[13]]
  )
  
  
  
  ## Debugging purposes
  
  # results_list <- list()
  # 
  # results_list[[1]] <- test_c
  # results_list[[2]] <- test_F
  # results_list[[3]] <- test_pi
  # results_list[[4]] <- test_m
  
  ## 2.5 Changes -----------------------
  
  ## once the deltas are calculated the Z vector has to be changed
  ## the constant in this is W the wage amount
  
  ## 2.5.1 New prices -----------------
  
  max_pi <- max(test_c)
  min_pi <- min(test_c)  
  
  
  max_price <-max(test_function_cal[[7]])
  min_price <- min(test_function_cal[[7]])
  
  new_max_price <- max_price*alter_factor
  new_min_price <- min_price/alter_factor
  
  
  new_prices <- rep(1, length(sectors))
  
  # new_price_df <- data.frame("price" = new_prices,
  #                            "direction" =  rep(0),
  #                            "change" = rep(0))
  
  
  for(i in 1:length(sectors)){
    
    if(test_c[i] > 0){
      
      new_prices[i] <- test_function_cal[[7]][i] + ((new_max_price-test_function_cal[[7]][i])*(test_c[i]/max_pi))
      
      
    } else if(test_c[i] < 0) {
      
      new_prices[i] <- test_function_cal[[7]][i] - ((test_function_cal[[7]][i]-new_min_price)*(test_c[i]/min_pi))
      
      
    }
    
  } 
  
  
  
  ## 2.5.2 New Wages -----------------------------------
  
  new_labour_prices <- test_function_cal[[10]]
  
  if(test_F[[2]][2]>test_F[[2]][1]){
    
    percentage_change <- min( ((test_F[[2]][2]-test_F[[2]][1])/test_F[[2]][1]),0.1)
    
    new_labour_prices <- new_labour_prices*(1+percentage_change)
    
  } else{
    
    percentage_change <- min( ((test_F[[2]][1]-test_F[[2]][2])/test_F[[2]][2]),0.1)
    
    new_labour_prices <- new_labour_prices*(1-percentage_change)
    
  }

    
  ## check how deltas are evolving
  
  delta_amount <- rep(0,4)
  
  for(i in length(test_c)){
    
    delta_amount[1] <- delta_amount[1]+ abs(test_c[i])
    
  }
  
  for(i in length(test_F[[1]])){
    
    delta_amount[2] <- delta_amount[2]+ abs(test_F[[1]][i])
    
  }
  
  for(i in length(test_pi)){
    
    delta_amount[3] <- delta_amount[3]+ abs(test_pi[i])
    
  }
  
  for(i in length(test_m)){
    
    delta_amount[4] <- delta_amount[4]+ abs(test_m[i])
    
  }
  
  return_list <- list()
  
  return_list[[1]] <- new_prices
  return_list[[2]] <- delta_amount
  return_list[[3]] <- test_F
  
  ## keep old wages
  return_list[[4]] <- test_function_cal[[10]]
  #return_list[[4]] <- new_labour_prices
  
  
  return(return_list)
  
}




