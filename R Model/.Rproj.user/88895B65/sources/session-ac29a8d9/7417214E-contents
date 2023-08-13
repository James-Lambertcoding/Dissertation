# 10_simple_version.R

## simple economy to test the system

## three goods
## 1.0 set  up economy ------------
iot_simple <- data.frame("x.1" = c(0,0,0),
                         "x.2" = c(10,0,0),
                         "x.3" = c(10,0,0), 
                         "labour" = c(20,20,20),
                         
                         "final_demand" = c(0,20,20),
                         "price" = c(1,1,1),
                         "salary" = c(1,1,1)) %>% 
  mutate(int_demand = x.1 + x.2 +x.3) %>% 
  mutate(total_demand = (final_demand + int_demand))


## 2.0 Calibration Function ------------

fun_s_calibration <- function(iot_simple){
  
  


cal_iot_simple <- iot_simple %>% 
  mutate(alpha_i = final_demand*price/sum(final_demand*price)) %>% 
  mutate(gi_ai = (final_demand*price)^alpha_i) %>% 
  mutate(A_c = sum(final_demand*price)/prod(gi_ai)) %>% 
  mutate(b_x1 = x.1/total_demand) %>% 
  mutate(b_x2 = x.2/total_demand) %>% 
  mutate(b_x3 = x.3/total_demand) %>% 
  mutate(labour_cost = salary * labour) %>% 
  mutate(gamma_j = labour/total_demand) %>%
  mutate(gamma_j_2 = labour_cost/total_demand) %>%
  mutate(xj_bj1 = x.1^b_x1 ) %>% 
  mutate(xj_bj2 = x.2^b_x2 ) %>% 
  mutate(xj_bj3 = x.3^b_x3 ) %>% 
  mutate(labour_gamma = labour^gamma_j) %>% 
  mutate(labour_gamma_2 = labour_cost^gamma_j_2) %>% 
  mutate(a_j = total_demand) %>% 
  ## test equation 15
  mutate(lhs = price * total_demand) %>% 
  mutate(p_x1 = price *x.1) %>% 
  mutate(p_x2 = price *x.2) %>% 
  mutate(p_x3 = price *x.3) %>% 
  mutate(rhs = 0)

cal_iot_simple[1,"rhs"] <- sum(cal_iot_simple[,"p_x1"])+cal_iot_simple[1,"labour_cost"]
cal_iot_simple[2,"rhs"] <- sum(cal_iot_simple[,"p_x2"])+cal_iot_simple[2,"labour_cost"]
cal_iot_simple[3,"rhs"] <- sum(cal_iot_simple[,"p_x3"])+cal_iot_simple[3,"labour_cost"]



cal_iot_simple[1,"a_j"] <- cal_iot_simple[1,"total_demand"] /(prod(cal_iot_simple[,"xj_bj1"]) * cal_iot_simple[1,"labour_gamma_2"])
cal_iot_simple[2,"a_j"] <- cal_iot_simple[2,"total_demand"] /(prod(cal_iot_simple[,"xj_bj2"]) * cal_iot_simple[2,"labour_gamma_2"])
cal_iot_simple[3,"a_j"] <- cal_iot_simple[3,"total_demand"] /(prod(cal_iot_simple[,"xj_bj3"]) * cal_iot_simple[3,"labour_gamma_2"])

return(cal_iot_simple)

}


cal_iot_simple <- fun_s_calibration(iot_simple = iot_simple)

## 3.0 Deltas Function ---------------

## 3.1 delta m -------------

fun_s_delta_m <- function(cal_iot_simple){
  
  delta_m <- sum(cal_iot_simple[,"labour_cost"]) - sum(cal_iot_simple[,"total_demand"])
  
  return(delta_m)
  
}

## 3.2 delta F ----------------
fun_s_delta_F <- function(cal_iot_simple){
## step 1

cal_delta_f <- cal_iot_simple %>% 
  mutate(step_1_f = gamma_j*price*total_demand/salary)

delta_f <- sum(cal_delta_f[,"step_1_f"]) - sum(cal_delta_f[,"total_demand"])

return(delta_f)

}
## 3.3 delta pi -----------------------
fun_s_delta_pi <- function(cal_iot_simple){
cal_delta_pi <- cal_iot_simple %>% 
  mutate(step_1_pi_1 = 0 ) %>% 
  mutate(step_1_pi_2 = 0 ) %>% 
  mutate(step_1_pi_3 = 0 ) %>% 
  mutate(step_2_pi = (salary/gamma_j)^gamma_j)  

for(j in 1:3){
  
  cal_delta_pi[j,"step_1_pi_1"] <- (cal_delta_pi[1,"price"]/cal_delta_pi[j,"b_x1"])^cal_delta_pi[j,"b_x1"]
  cal_delta_pi[j,"step_1_pi_2"] <- (cal_delta_pi[2,"price"]/cal_delta_pi[j,"b_x2"])^cal_delta_pi[j,"b_x2"]
  cal_delta_pi[j,"step_1_pi_3"] <- (cal_delta_pi[3,"price"]/cal_delta_pi[j,"b_x3"])^cal_delta_pi[j,"b_x3"]
  
  
}

cal_delta_pi_2 <- cal_delta_pi %>% 
  mutate(delta = price - (a_j*step_1_pi_1*step_1_pi_2*step_1_pi_3*step_2_pi))

delta_pi <- cal_delta_pi_2[,"delta"]

return(delta_pi)

}
## 3.4 delta C --------------
fun_s_delta_c <- function(cal_iot_simple){

cal_delta_c <- cal_iot_simple %>% 
  mutate(step_1_1 = b_x1*price* total_demand ) %>% 
  mutate(step_1_2 = b_x2*price* total_demand ) %>% 
  mutate(step_1_3 = b_x3*price* total_demand ) %>% 
  mutate(labour_cost = labour*salary) %>% 
  mutate(step_2 = alpha_i*sum(labour_cost)) %>% 
  mutate(step_3 = price * total_demand) %>% 
  mutate(delta_c = step_1_1 + step_1_2 + step_1_3 + step_2 - step_3)

delta_c <- cal_delta_c[,"delta_c"]

return(delta_c)


}
## 3.5 All Deltas --------------

fun_s_delta_all <- function(cal_iot_simple){
  
  delta_m <- fun_s_delta_m(cal_iot_simple = cal_iot_simple)
  delta_F <- fun_s_delta_F(cal_iot_simple = cal_iot_simple)
  delta_pi <- fun_s_delta_pi(cal_iot_simple = cal_iot_simple)
  delta_c <- fun_s_delta_c(cal_iot_simple = cal_iot_simple)
  
  delta_list <- list()
  
  delta_list[[1]] <- delta_m
  delta_list[[2]] <- delta_F
  delta_list[[3]] <- delta_pi
  delta_list[[4]] <- delta_c
  
  
return(delta_list)
  
  
}


## 4.0 Equilibrium tests ------------

## 4.1 Test Output ----------

fun_test_y <- function(cal_iot_simple){
  
  lhs_y <- cal_iot_simple[,"total_demand"]
  
  rhs_y <- cal_iot_simple[,"int_demand"] + cal_iot_simple[,"final_demand"]
  
  test_y <- data.frame("lhs" = lhs_y,
                       "rhs"= rhs_y)
  
  return(test_y)
  
}

## 4.2 Test Price -----------

fun_test_p <- function(cal_iot_simple){
  
  
  test_p <- data.frame("lhs" = cal_iot_simple[,"lhs"],
                       "rhs"= cal_iot_simple[,"rhs"])
  
  return(test_p)
  
}

fun_equi <- function(cal_iot_simple){
  
  fun_equi_list <- list()
  
  fun_equi_list[[1]] <- fun_test_y(cal_iot_simple = cal_iot_simple)
  fun_equi_list[[2]] <- fun_test_p(cal_iot_simple = cal_iot_simple)
  
  return(fun_equi_list)
  
}


test_equi <- fun_equi(cal_iot_simple = cal_iot_simple)

test_all <- fun_s_delta_all(cal_iot_simple = cal_iot_simple)

test_all
