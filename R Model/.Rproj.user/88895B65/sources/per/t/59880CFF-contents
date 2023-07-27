# 10_simple_version.R

## simple economy to test the system

## three goods
## 1.0 set  up economy ------------
iot_simple <- data.frame("x.1" = c(0,0,0),
                         "x.2" = c(0,0,0),
                         "x.3" = c(0,0,0), 
                         "final_demand" = c(40,20,20),
                         "labour" = c(20,20,20),
                         "price" = c(1,1,1),
                         "salary" = c(1,1,1)) %>% 
  mutate(int_demand = x.1 + x.2 +x.3) %>% 
  mutate(total_demand = (final_demand + int_demand))


## 2.0 calculate calibration ------------
cal_iot_simple <- iot_simple %>% 
  mutate(alpha_i = final_demand/sum(final_demand)) %>% 
  mutate(gi_ai = final_demand^alpha_i) %>% 
  mutate(A_c = sum(final_demand)/prod(gi_ai)) %>% 
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

cal_iot_simple[1,"rhs"] <- sum(cal_iot_simple[,"p_x1"])-cal_iot_simple[1,"labour_cost"]
cal_iot_simple[2,"rhs"] <- sum(cal_iot_simple[,"p_x2"])-cal_iot_simple[2,"labour_cost"]
cal_iot_simple[3,"rhs"] <- sum(cal_iot_simple[,"p_x3"])-cal_iot_simple[3,"labour_cost"]

  

cal_iot_simple[1,"a_j"] <- cal_iot_simple[1,"total_demand"] /(prod(cal_iot_simple[,"xj_bj1"]) * cal_iot_simple[1,"labour_gamma_2"])
cal_iot_simple[2,"a_j"] <- cal_iot_simple[2,"total_demand"] /(prod(cal_iot_simple[,"xj_bj2"]) * cal_iot_simple[2,"labour_gamma_2"])
cal_iot_simple[3,"a_j"] <- cal_iot_simple[3,"total_demand"] /(prod(cal_iot_simple[,"xj_bj3"]) * cal_iot_simple[3,"labour_gamma_2"])


## 3.0 calculate Deltas ---------------

## 3.1 delta m -------------

delta_m <- sum(cal_iot_simple[,"labour_cost"]) - sum(cal_iot_simple[,"total_demand"])

## 3.2 delta F ----------------

## step 1

cal_delta_f <- cal_iot_simple %>% 
  mutate(step_1_f = gamma_j*price*total_demand/salary)

delta_f <- sum(cal_delta_f[,"step_1_f"]) - sum(cal_delta_f[,"total_demand"])

## 3.3 delta pi -----------------------

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

## 3.4 delta C --------------

cal_delta_c <- cal_iot_simple %>% 
  mutate(step_1_1 = b_x1*price* total_demand ) %>% 
  mutate(step_1_2 = b_x2*price* total_demand ) %>% 
  mutate(step_1_3 = b_x3*price* total_demand ) %>% 
  mutate(labour_cost = labour*salary) %>% 
  mutate(step_2 = alpha_i*sum(labour_cost)) %>% 
  mutate(step_3 = price * total_demand) %>% 
  mutate(delta_c = step_1_1 + step_1_2 + step_1_3 + step_2 - step_3)

delta_c <- cal_delta_c[,"delta_c"]

## Deltas --------------
delta_m
delta_f
delta_pi  
delta_c  

