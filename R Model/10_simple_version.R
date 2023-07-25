# 10_simple_version.R

## simple economy to test the system

## three goods

iot_simple <- data.frame("x.1" = c(0,0,0),
                         "x.2" = c(2,0,0),
                         "x.3" = c(2,0,0), 
                         "final_demand" = c(40,20,20),
                         "labour" = c(40,20,20),
                         "salary" = c(1,1,1)) %>% 
  mutate(int_demand = x.1 + x.2 +x.3) %>% 
  mutate(total_demand = final_demand + int_demand)

cal_iot_simple <- iot_simple %>% 
  mutate(alpha_i = final_demand/sum(final_demand)) %>% 
  mutate(gi_ai = final_demand^alpha_i) %>% 
  mutate(A_c = sum(final_demand)/prod(gi_ai)) %>% 
  mutate(b_x1 = x.1/total_demand) %>% 
  mutate(b_x2 = x.2/total_demand) %>% 
  mutate(b_x3 = x.3/total_demand) %>% 
  mutate(gamma_j = labour/total_demand) %>% 
  mutate(xj_bj1 = x.1^b_x1 ) %>% 
  mutate(xj_bj2 = x.2^b_x2 ) %>% 
  mutate(xj_bj3 = x.3^b_x3 ) %>% 
  mutate(labour_gamma = labour^gamma_j) %>% 
  mutate(a_j = total_demand)


  
cal_iot_simple[1,"a_j"] <- cal_iot_simple[1,"total_demand"] /(prod(cal_iot_simple[,"xj_bj1"]) * cal_iot_simple[1,"labour_gamma"])
cal_iot_simple[2,"a_j"] <- cal_iot_simple[2,"total_demand"] /(prod(cal_iot_simple[,"xj_bj2"]) * cal_iot_simple[2,"labour_gamma"])
cal_iot_simple[3,"a_j"] <- cal_iot_simple[3,"total_demand"] /(prod(cal_iot_simple[,"xj_bj3"]) * cal_iot_simple[3,"labour_gamma"])

  
  

  
  

