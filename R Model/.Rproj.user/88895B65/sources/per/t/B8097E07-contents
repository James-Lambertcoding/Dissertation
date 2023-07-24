## 04_CGE_Functions.R

## 1.0 General Equilibrium Point Functions -----------------

## 1.1 Delta C_i ----------------

function_delta_c_i <- function(alpha, beta, price, output, salary, labour, capital, saving, interest_rate){
  
  delta_c <- rep(0,length(sectors))
  
  ## step 1
  for(i in 1:length(sectors)){
    
    for (j in 1:nrow(beta)) {
      
      delta_c[i]  <- delta_c[i] +   ( beta[i,j]*price[j]*output[j])
      
    }
    
  }
  ## step 2
  
  temp_primary_factor_df <- data.frame("labour" = labour,
                                       "Salary" = salary,
                                       "capital" = capital,
                                       "capital_price" = rep(interest_rate)) %>% 
    mutate(labour_cost = labour * salary) %>% 
    mutate(capital_cost = capital * interest_rate)
  
  temp_primary_tot_1 <- sum(temp_primary_factor_df[,"labour_cost"])
  temp_primary_tot_2 <- sum(temp_primary_factor_df[,"capital_cost"])
  
  temp_primary_tot <- temp_primary_tot_1 + temp_primary_tot_2
  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] + alpha[i]*temp_primary_tot
    
  }
  
  ## step 3
  
  for(i in 1:length(sectors)){
    
    delta_c[i]  <- delta_c[i] - (price[i]*output[i])
    
  }
  
  
  
  return(delta_c)
  
}

function_delta_F_f <- function(gamma,price,output,primary_cost, primary_factors){
  
  delta_f <- rep(0,length(sectors))
  
  
  
  
  return(delta_f)
  
}


function_delta_m <- function(salary, labour, capital, interst_rate, m_tot){
  
  
  
}
  
## 2.0 Test Data--------------

prices_test <- rep(1,length(sectors))
output_test <- ig_tot_df_2[,"total_demand"]*10^6
labour_test <- cal_gamma_full_2[,"labour"]
salary_test <- cal_gamma_full_2[,"salary"]
alpha_test <- demand_hh_df[,"alph_i"]
capital_test <- cal_gamma_full_2[,"capital"]
gamma_test <- cal_gamma_full_2[,c("capital_gamma","labour_gamma")]

test_F <- function_delta_F_f()


test_c <- function_delta_c_i(alpha = alpha_test,
                           beta = cal_beta,
                           price = prices_test,
                           output = output_test,
                           labour = labour_test,
                           salary = salary_test,
                           capital = capital_test, 
                           interest_rate = Interst_rate)


