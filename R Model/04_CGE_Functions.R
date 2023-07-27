## 04_CGE_Functions.R

## 1.0 General Equilibrium Point Functions -----------------

## 1.1 Delta C_i ----------------

function_delta_c_i <- function(alpha, beta, price, output, salary, labour, capital, saving, interest_rate){
  
  delta_c <- rep(0,length(sectors))
  
  ## step 1
  for(i in 1:length(sectors)){
    
    for (j in 1:nrow(beta)) {
      
      delta_c[i]  <- delta_c[i] +   ( beta[j,i]*price[j]*output[j])
      
    }
    
  }
  ## step 2
  
  temp_primary_factor_df <- data.frame("labour" = labour,
                                       "Salary" = salary,
                                       "capital" = capital,
                                       "capital_price" = rep(interest_rate),
                                       "alpha" = alpha) %>% 
    mutate(labour_cost = labour * salary) %>% 
    mutate(capital_cost = capital * interest_rate) %>% 
    mutate(step_2 = alpha * (labour_cost+capital_cost))
  

  

  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] + temp_primary_factor_df[i,"step_2"]
    
  }
  
  ## step 3
  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] - (price[i]*output[i])
    
  }
  
  
  
  return(delta_c)
  
}

function_delta_F_f <- function(gamma_capital,gamma_labour,price,output,capital_price,labour_price, capital, labour ){
  
  delta_f <- rep(0,2)
  
  delta_f_sectors <- data.frame("sectors" = sectors,
                                "gamma_capital" = gamma_capital,
                                "gamma_labour" = gamma_labour,
                                "price" = price,
                                "output" = output,
                                "capital_price"= capital_price,
                                "labour_price" = labour_price
                                ) %>% 
    mutate(step_1_labour = gamma_labour*price*output/labour_price) %>% 
    mutate(step_1_capital  = gamma_capital*price*output/capital_price)
  
  delta_f[1] <- sum(delta_f_sectors[,"step_1_labour"])-sum(labour)
  delta_f[2] <- sum(delta_f_sectors[,"step_1_capital"])-sum(capital)
 
  return(delta_f)
  
}

function_delta_pi_j <- function(price, A_j, beta, gamma_capital, gamma_labour, primary_costs,runs){
  

  
  delta_pi <- rep(0, length(sectors))
  
  ## first part
  delta_pi <- price
  
  ## second part
  step_1_list <- list()
  
  ## third part
  
  step_2_df <- data.frame("gamma_capital" = gamma_capital,
                          "gamma_labour" = gamma_labour) %>% 
    mutate(capial_price = primary_costs[,"capital_price"]) %>% 
    mutate(labour_price = primary_costs[,"labour_price"]) %>% 
    mutate(w_g_g1 = (capial_price/gamma_capital)^gamma_capital) %>% 
    mutate(w_g_g2 = (labour_price/gamma_labour)^gamma_labour) %>% 
    mutate(step_2_j = w_g_g1 * w_g_g2)
    
  
  step_1_df <- beta 
  step_1_vec <- rep(0,nrow(beta))
  
  
  
  for(j in 1:ncol(step_1_df)){
    for(i in 1:nrow(step_1_df)){
      
      step_1_df[i,j] <- (price[i]/step_1_df[i,j])^step_1_df[i,j]
      
    }
    step_1_vec[j] <- prod(step_1_df[,j])
      
  }
  

  for(j in 1:nrow(beta)){
    
    delta_pi[j] <- price[j] - (A_j[j]* step_1_vec[j] *step_2_df[j,"step_2_j"])
    
  }
  
  print(delta_pi)
  
  # for(j in 1:length(sectors)){
  #   
  #   step_1_list[[j]] <- beta %>% 
  #     mutate(prices = price) 
  #   
  #   step_1_list[[j]] <- step_1_list[[j]][,c(j,ncol(step_1_list[[j]]))]
  #   
  #   colnames(step_1_list[[j]])[1] <- "beta_j"
  #   
  #  
  #   
  #   step_1_list[[j]] <- step_1_list[[j]] %>% 
  #     ##p_b_b = price/ beta raised to beta
  #     mutate(p_b_b = (price/beta_j)^beta_j)
  #   
  #  delta_pi[j] <- delta_pi[j] - A_j[j]*prod(step_1_list[[j]][,"p_b_b"])*step_2_df[j,"step_2_j"]
  #   
  # }
  
  
 #return(step_2_df) 
  return(delta_pi)
  
}

function_delta_m <- function(salary, labour, capital, interest_rate, m_tot){
  
  delta_m <- 0
  
  delta_m_sectors <- data.frame("labour" = labour,
                                "labour_price" = salary,
                                "captial" = capital,
                                "capital_price" = interest_rate) %>% 
    mutate(capital_cost = capital*capital_price) %>% 
    mutate(labour_cost = labour*labour_price)
  
  tot_labour_cost <- sum(delta_m_sectors[,"labour_cost"])
  tot_capital_cost <- sum(delta_m_sectors[,"capital_cost"])
  
  delta_m <- sum(tot_capital_cost,tot_labour_cost) - m_tot
  
  return(delta_m)
  
}
  
## 2.0 Test Data--------------

function_test_delta <- function(test_function_cal,runs){
  
## 2.1 test_C ----------------------
  test_c <- function_delta_c_i(alpha = test_function_cal[[1]],
                             beta = test_function_cal[[3]],
                             price = test_function_cal[[7]],
                             output = test_function_cal[[8]],
                             labour = test_function_cal[[9]],
                             salary = test_function_cal[[10]],
                             capital = test_function_cal[[11]], 
                             interest_rate = Interest_rate)

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
                           interest_rate = Interest_rate,
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
  
  max_pi <- max(test_pi)
  min_pi <- min(test_pi)  
  
  
  max_price <-max(test_function_cal[[7]])
  min_price <- min(test_function_cal[[7]])
  
  new_max_price <- max_price*alter_factor
  new_min_price <- min_price/alter_factor
  
  
  new_prices <- rep(1, length(sectors))
  
  # new_price_df <- data.frame("price" = new_prices,
  #                            "direction" =  rep(0),
  #                            "change" = rep(0))

  
  for(i in 1:length(sectors)){
    
    
      
      
    if(test_pi[i] > 0){
      
      new_prices[i] <- test_function_cal[[7]][i] - ((test_function_cal[[7]][i]-new_min_price)*(test_pi[i]/max_pi))
      
      # new_price_df[i,1] <- test_function_cal[[7]][i]-((1/alter_factor)*(test_pi[i]/max_pi))
      # new_price_df[i,2] <- "down"
      # new_price_df[i,3] <- test_pi[i]/max_pi
      
    } else if(test_pi[i] < 0) {
      
      new_prices[i] <- test_function_cal[[7]][i] + ((new_max_price-test_function_cal[[7]][i])*(test_pi[i]/min_pi))
      
      # new_price_df[i,1] <- test_function_cal[[7]][i] + (test_pi[i]/min_pi)
      # new_price_df[i,2] <- "up"
      # new_price_df[i,3] <- test_pi[i]/min_pi
      }
    
  } 
  
  ## check how deltas are evoling
  
  delta_amount <- rep(0,4)
  
  for(i in length(test_c)){
    
    delta_amount[1] <- delta_amount[1]+ abs(test_c[i])
    
  }
  
  for(i in length(test_F)){
    
    delta_amount[2] <- delta_amount[2]+ abs(test_F[i])
    
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
  
  
  return(return_list)
  
  }




