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

function_delta_pi_j <- funciton(price,A_j, )

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

prices_test <- rep(1,length(sectors))
capital_price_test <- rep(Interest_rate,length(sectors))
output_test <- cal_gamma_2[,"total_supply"]
labour_test <- cal_gamma_2[,"labour"]
salary_test <- cal_gamma_2[,"salary"]
alpha_test <- cal_HH_utl[,"alpha_i"]
capital_test <- cal_gamma_2[,"capital"]
gamma_capital_test <- cal_gamma_2[,"capital_gamma"]
gamma_labour_test <- cal_gamma_2[,"labour_gamma"]

gamma_test <- cal_gamma_2[,c("capital_gamma","labour_gamma")]
m_test <- m_bar
vj_test <- cal_vj_bar[,"vj_bar"]

test_F <- function_delta_F_f(gamma_capital =  gamma_capital_test,
                             gamma_labour = gamma_labour_test,
                             price = prices_test,
                             output = output_test,
                             capital_price = capital_price_test,
                             labour_price = salary_test,
                             capital =  capital_test,
                             labour = labour_test
                             )

test_m <- function_delta_m(salary = salary_test,
                           labour = labour_test,
                           capital = capital_test,
                           interest_rate = Interst_rate,
                           m_tot = m_test
                            )


test_c <- function_delta_c_i(alpha = alpha_test,
                           beta = cal_beta,
                           price = prices_test,
                           output = output_test,
                           labour = labour_test,
                           salary = salary_test,
                           capital = capital_test, 
                           interest_rate = Interst_rate)


