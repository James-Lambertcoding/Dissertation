## 04_CGE_Functions.R

## Delta C_i

function_delta_c_i <- function(alpha, beta, price, output, salary, labour, capital, saving){
  
  delta_c <- rep(0,length(sectors))
  
  ## step 1
  for(i in 1:length(sectors)){
    
    for (j in 1:nrow(beta)) {
      
      delta_c[i]  <- delta_c[i] +   ( beta[i,j]*price[j]*output[j])
      
    }
    
  }
  ## step 2
  
  temp_primary_factor_df <- data.frame("labour" = labour,
                                       "Salary" = salary) %>% 
    mutate(labour_cost = labour * salary)
  
  temp_primary_tot <- sum(temp_primary_factor_df[,"labour_cost"])
  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] + alpha[i]*temp_primary_tot
    
  }
  
  ## step 3
  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] - (price[i]*output[i])
    
  }
  
  
  
  return(delta_c)
  
}

prices_test <- rep(1,length(sectors))
output_test <- ig_tot_df_2[,"total_demand"]*10^6
labour_test <- cal_gamma_full_2[,"labour"]
salary_test <- cal_gamma_full_2[,"salary"]
alpha_test <- demand_hh_df[,"alph_i"]

test <- function_delta_c_i(alpha = alpha_test,
                           beta = cal_beta,
                           price = prices_test,
                           output = output_test,
                           labour = labour_test,
                           salary = salary_test)
