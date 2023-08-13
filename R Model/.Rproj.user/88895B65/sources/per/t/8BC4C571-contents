## Model_2_Calibration.R 
## 0.0 Assumptions ----------

## structure is 5x5 and 5x2
## 5 sectors
## 2 primary input

labour_tot <- 28764400
Capital_tot <- 11137120
interest_rate <- 0.1
capital_returns <- Capital_tot * interest_rate


## 1.0 Data Names ---------------


ig_coef_name <- "Simple_data/prod_coef.csv"
hh_coef_name <- "Simple_data/hh_coef.csv"
emp_coef_name <- "Simple_data/emp_coef.csv"
cap_coef_name <- "Simple_data/cap_coef.csv"

## 2.0 Read Data in csv -------------

ig_coef_df <- read.csv(ig_coef_name, stringsAsFactors = F)
hh_coef_df <- read.csv(hh_coef_name, stringsAsFactors = F)
emp_coef_df <- read.csv(emp_coef_name, stringsAsFactors = F)
cap_coef_df <-read.csv(cap_coef_name, stringsAsFactors = F)

## 3.0 Assumptions ----------

Sectors_52 <- paste0("sec_",ig_coef_df[,"Sectors"])
M_bar <- labour_tot +capital_returns

price_52 <- rep(1,length(Sectors_52))
wages_52 <- rep(1,length(Sectors_52))

primary_factor_52 <- data.frame("sectors" = Sectors_52,
                                "cap_coef" = cap_coef_df[,"Coef"],
                                "emp_coef" = emp_coef_df[,"Coef"]) %>% 
  mutate(capital = cap_coef*Capital_tot,
         labour = emp_coef*labour_tot,
         capital_costs = cap_coef *capital_returns,
         g_id = labour + capital_costs) 

## 4.0 Calibration --------- 
g_id_df <- primary_factor_52 %>% 
  select(g_id)

colnames(ig_coef_df)[2:6] <- Sectors_52

## 4.1 x_ij -------------
x_ij <- ig_coef_df %>% 
  select(-Sectors) %>% 
  mutate(g_id = g_id_df[,"g_id"])%>% 
  mutate(A_tot = 0) %>% 
  mutate(B_tot = 0) %>% 
  mutate(C_tot = 0) %>% 
  mutate(D_tot = 0) %>% 
  mutate(E_tot = 0) %>% 
  mutate(emp_tot = as.numeric(hh_coef_df[,"Coef"]*labour_tot)) %>% 
  mutate(y_i = 0)

for (i in 1:length(Sectors_52)) {
  
  x_ij[i,"A_tot"] <-x_ij[i,"sec_A"]*x_ij[i,"g_id"]
  x_ij[i,"B_tot"] <-x_ij[i,"sec_B"]*x_ij[i,"g_id"]
  x_ij[i,"C_tot"] <-x_ij[i,"sec_C"]*x_ij[i,"g_id"]
  x_ij[i,"D_tot"] <-x_ij[i,"sec_D"]*x_ij[i,"g_id"]
  x_ij[i,"E_tot"] <-x_ij[i,"sec_E"]*x_ij[i,"g_id"]
  
  
}



x_ij[1,"y_i"] <- sum(x_ij[,"A_tot"])+x_ij[1,"g_id"]
x_ij[2,"y_i"] <- sum(x_ij[,"B_tot"])+x_ij[2,"g_id"]
x_ij[3,"y_i"] <- sum(x_ij[,"C_tot"])+x_ij[3,"g_id"]
x_ij[4,"y_i"] <- sum(x_ij[,"D_tot"])+x_ij[4,"g_id"]
x_ij[5,"y_i"] <- sum(x_ij[,"E_tot"])+x_ij[5,"g_id"]

## needed for function at the end
x_ij_factors <- x_ij[1:length(Sectors_52),1:length(Sectors_52)]


## 4.2 Beta-------------------
b_ij <- matrix(ncol=length(Sectors_52),nrow=length(Sectors_52),0)

for(i in 1:length(Sectors_52)){
  
  
  b_ij[i,1] <-  x_ij[i,"A_tot"]/x_ij[1,"y_i"]
  b_ij[i,2] <-  x_ij[i,"B_tot"]/x_ij[2,"y_i"]
  b_ij[i,3] <-  x_ij[i,"C_tot"]/x_ij[3,"y_i"]
  b_ij[i,4] <-  x_ij[i,"D_tot"]/x_ij[4,"y_i"]
  b_ij[i,5] <-  x_ij[i,"E_tot"]/x_ij[5,"y_i"]
  
}  


## 4.3 Gamma -------------
gamma_i <- primary_factor_52 %>% 
  mutate(y_i = x_ij[,"y_i"]) %>% 
  mutate(gamma_lab = labour/y_i) %>% 
  mutate(gamma_cap = capital_costs/y_i)


check_factors <-as.data.frame(  t(b_ij) )%>% 
  bind_cols(gamma_i) %>% 
  mutate(check_sum = V1+V2+V3+V4+V5+gamma_lab+gamma_cap)

x_ij_only <- x_ij %>% 
  select(A_tot, B_tot, C_tot, D_tot, E_tot)

x_ij_tran <- t(x_ij_only)
b_ij_tran <-t(b_ij)



A_J <- data.frame("y_i" = check_factors[,"y_i"],
                  "x_1j_b_1j" = 0,
                  "x_2j_b_2j" = 0,
                  "x_3j_b_3j" = 0,
                  "x_4j_b_4j" = 0,
                  "x_5j_b_5j" = 0
)

for(i in 1:length(Sectors_52)){
  
  for(j in 1:length(Sectors_52)){
    
    A_J[i,j+1] <- x_ij_tran[i,j]^b_ij_tran[i,j]
    
  }
}


## 4.4 A_j ---------------

A_J <- A_J %>% 
  mutate(labour = check_factors[,"labour"]) %>%
  mutate(capital_costs = check_factors[,"capital_costs"]) %>%
  mutate(gamma_lab = check_factors[,"gamma_lab"]) %>% 
  mutate(gamma_cap = check_factors[,"gamma_cap"]) %>% 
  mutate(lab_gamma_i = labour^gamma_lab) %>%
  mutate(cap_gamma_i = capital_costs^gamma_cap) %>%
  mutate(A_J = 0)

for(i in 1:length(Sectors_52)){
  
  A_J[i,"A_J"] <- A_J[i,"y_i"]/(A_J[i,"x_1j_b_1j"]*A_J[i,"x_2j_b_2j"]*A_J[i,"x_3j_b_3j"]*A_J[i,"x_4j_b_4j"]*A_J[i,"x_5j_b_5j"]*A_J[i,"lab_gamma_i"]*A_J[i,"cap_gamma_i"])
  
}   


## 4.5 Combine Calibration ----------

cal_52_list <- list()

## Prices
cal_52_list[[1]] <- price_52
## Wages
cal_52_list[[2]] <- wages_52
## Total Outputs
cal_52_list[[3]] <-gamma_i[,"y_i"]
## Total Consumption
cal_52_list[[4]] <- gamma_i[,"g_id"]
## Alpha 
cal_52_list[[5]] <- emp_coef_df[,"Coef"]
## Gamma_lab
cal_52_list[[6]] <- gamma_i[,"gamma_lab"]
## labour
cal_52_list[[7]] <- gamma_i[,"labour"]
## A_j
cal_52_list[[8]] <- A_J[,"A_J"]
##beta
cal_52_list[[9]] <- b_ij
## ig <- x_ij
cal_52_list[[10]] <- x_ij_only
## Sectors
cal_52_list[[11]] <- Sectors_52
## Capital Costs
cal_52_list[[12]] <- gamma_i[,"capital_costs"]
## Gamma_lab
cal_52_list[[13]] <- gamma_i[,"gamma_cap"]
## Capital Price 
cal_52_list[[14]] <- rep(interest_rate,length(Sectors_52))




## 5.0 Delta Functions -------------


## 5.1 Delta C -------------
delta_52_c <- function(cal_52_list){
  
  step_1 <- as.data.frame( cal_52_list[[9]]) %>% 
    mutate(prices = cal_52_list[[1]] ) %>% 
    mutate(total_supply = cal_52_list[[3]])
  
  step_1_sec <- rep(0,length(Sectors_52))
  
  for(i in 1:length(Sectors_52)){
    for(j in 1:length(Sectors_52) ){
      
      step_1_sec[i] <- step_1_sec[i] + step_1[i,j]*step_1[j,"prices"]*step_1[j,"total_supply"]
      
    }
    
  }
  
  
  step_2 <-rep(0,length(Sectors_52))
  
  step_2b <- data.frame("wages" = cal_52_list[[2]],
                        "labour" = cal_52_list[[7]],
                        "capital_costs" = cal_52_list[[12]]) %>% 
    mutate(labour_cost = wages *labour)
  
  
  
  step_2b_tot <- sum(step_2b[,"labour_cost"]) + sum(step_2b[,"capital_costs"])
  
 
  for(i in 1:length(Sectors_52)){
    
    step_2[i] <- cal_52_list[[5]][i]*step_2b_tot
    
  }
  
  
  delta_52_c <- rep(0,length(Sectors_52))
  
  for(i in 1:length(Sectors_52)){
    
    delta_52_c[i] <- step_1_sec[i]+step_2[i]-(cal_52_list[[1]][i]*cal_52_list[[3]][[i]])
    
  }
  
  
  
  return(delta_52_c)
  
}

## 5.2 Delta F -------------

delta_52_f <- function(cal_52_list){
  
  step_1 <- data.frame("Sectors" = cal_52_list[[11]],
                       "prices"= cal_52_list[[1]],
                       "total_supply" = cal_52_list[[3]],
                       "wages" = cal_52_list[[2]],
                       "gamma_lab" = cal_52_list[[6]],
                       "gamma_cap" = cal_52_list[[13]]  ) %>% 
    mutate(step_1a = gamma_lab*prices*total_supply/wages,
           step_1b = gamma_cap*prices*total_supply/wages)
  
  step_1a_tot <- sum(step_1[,"step_1a"])
  step_1b_tot <- sum(step_1[,"step_1b"])
  
  delta_52_f <- c(0,0)
  
  delta_52_f[1] <- step_1a_tot - sum(cal_52_list[[7]])
  delta_52_f[2] <- step_1b_tot - sum(cal_52_list[[12]])
  
  
  return(delta_52_f)
  
  
}

## 5.3 Delta pi --------------

delta_52_pi <- function(cal_52_list){
  
  
  step_1 <- cal_52_list[[9]]
  
  for(i in 1:length(Sectors_52)){
    for(j in 1:length(Sectors_52)){
      
      step_1[i,j] <- (cal_52_list[[1]][i]/cal_52_list[[9]][i,j])^cal_52_list[[9]][i,j]
      
    }
    
  }
  
  step_1_sec <- rep(0,length(Sectors_52))
  
  for(j in 1:length(Sectors_52)){
    
    step_1_sec[j] <- prod(step_1[,j])
    
  }
  
  ## w raised to gamma (labour)
  step_2a <-  cal_52_list[[2]]
  for(j in 1:length(Sectors_52)){
    
    step_2a[j] <- (cal_52_list[[2]][j]/cal_52_list[[6]][j])^cal_52_list[[6]][j]
    
  }
  
  ## w raised to gamma (capital)
  step_2b <-  cal_52_list[[2]]
  for(j in 1:length(Sectors_52)){
    
    step_2b[j] <- (cal_52_list[[14]][j]/cal_52_list[[13]][j])^cal_52_list[[13]][j]
    
  }
  
  step_1_2 <-  rep(0,length(Sectors_52))
  
  for(j in 1:length(Sectors_52)){
    
    step_1_2[j] <-  cal_52_list[[8]][j]*step_1_sec[j]*step_2a[j]*step_2b[j]
    
  }
  
  delta_52_pi <- cal_52_list[[1]]
  
  for(j in 1:length(Sectors_52)){
    
    delta_52_pi[j] <-  delta_52_pi[[j]] - step_1_2[j]
    
  }
  
  return(delta_52_pi)
  
}

## 5.4 Delta M ----------------

delta_52_m <- function(cal_52_list){
  
  labour_cost <- sum(cal_52_list[[7]] *cal_52_list[[2]])
  
  delta_52_m <- sum(cal_52_list[[12]]) +labour_cost - M_bar
  
  return(delta_52_m)
  
}

## 5.4 run tests --------
delta_52_all_fun <- function(cal_52_list){
  
  delta_all_list <- list()
  
  test_c <- delta_52_c(cal_52_list = cal_52_list)
  
  test_f <- delta_52_f(cal_52_list = cal_52_list)    

  test_pi <- delta_52_pi(cal_52_list = cal_52_list)

  test_m <- delta_52_m(cal_52_list = cal_52_list )
  
  delta_all_list[[1]] <- test_c
  delta_all_list[[2]] <- test_f
  delta_all_list[[3]] <- test_pi
  delta_all_list[[4]] <- test_m
  
  return(delta_all_list)
  
}

test_all <- delta_52_all_fun(cal_52_list = cal_52_list)

## 6.0 Search Function ---------------------------


update_inputs_fun <- function(delta_start, cal_52_list,runs){
  
  search_factor <- 2  
  steps <- length(Sectors_52)
  ## storage lists
  cal_52_all_list <- list()
  delta_list_all <- list()
  cal_52_all_list[[1]] <- cal_52_list
  
  ## storage Data Frames
  ## Scalars
  scalar_df <- data.frame("consu_1" = rep(1,2+runs),
                                      "consu_2" = rep(1),
                                      "consu_3" = rep(1),
                                      "consu_4" = rep(1),
                                      "consu_5" = rep(1),
                                      "wages_1" = rep(1),
                                      "wages_2" = rep(1),
                                      "wages_3" = rep(1),
                                      "wages_4" = rep(1),
                                      "wages_5" = rep(1),
                                      "prices_1" = rep(1),
                                      "prices_3" = rep(1),
                                      "prices_3" = rep(1),
                                      "prices_4" = rep(1),
                                      "prices_5" = rep(1))
  
  delta_df <- data.frame("consu_1" = rep(1,2+runs),
                          "consu_2" = rep(1),
                          "consu_3" = rep(1),
                          "consu_4" = rep(1),
                          "consu_5" = rep(1),
                          "wages" = rep(1),
                          "prices_1" = rep(1),
                          "prices_3" = rep(1),
                          "prices_3" = rep(1),
                          "prices_4" = rep(1),
                          "prices_5" = rep(1))
  
  delta_df[1,] <- c(delta_start[[1]],delta_start[[2]][1],delta_start[[3]])
  delta_df[2,] <- c(delta_start[[1]],delta_start[[2]][1],delta_start[[3]])
  
  

 k=1
 i=1 
  for(k in 1:runs){
   
    ## calculate scalars
    ## 6.1 Consumption ------------------
    for(i in 1:steps){
      
      ## if the delta is zero keep the same
      if(delta_df[1+k,i]==0){
        
        scalar_df[k+2,i] <- scalar_df[k+1,i]
        
      ## the delta do not have the same sign  
      } else if(sign(delta_df[1+k,i]) != sign(delta_df[k,i]) ){
        
        scalar_df[k+2,i] <- scalar_df[k,i] + ((scalar_df[k+1,i]-scalar_df[k,i])/2)
        
      } else if(sign(delta_df[k,i]) > 0 ){
        
        scalar_df[k+2,i] <- scalar_df[k+1,i] *search_factor
        
      } else {
        
        scalar_df[k+2,i] <- scalar_df[k+1,i] /search_factor
        
      }
      
    }

    ## calculate scalars
    ## 6.2 price -------------------------------
    for(i in 1:steps){
      
      ## if the delta is zero keep the same
      if(delta_df[1+k,i+6]==0){
        
        scalar_df[k+2,i+10] <- scalar_df[k+1,i+10]
        
        ## the delta do not have the same sign  
      } else if(sign(delta_df[1+k,i+6]) != sign(delta_df[k,i+6]) ){
        
        scalar_df[k+2,i+10] <- scalar_df[k,i+10] + ((scalar_df[k+1,i+10]-scalar_df[k,i+10])/2)
        
      } else if(sign(delta_df[k,i+6]) > 0 ){
        
        scalar_df[k+2,i+10] <- scalar_df[k+1,i+10] *search_factor
        
      } else {
        
        scalar_df[k+2,i+10] <- scalar_df[k+1,i+10] /search_factor
        
      }
      
    }
  ## 6.3 Wages ----------
    
  ## 6.4 Update Cal_List -------------
  
    cal_52_all_list[[k+1]] <- cal_52_all_list[[k]]
    
    ## update prices
    cal_52_all_list[[k+1]][[1]] <- cal_52_all_list[[k]][[1]] * as.numeric(scalar_df[k+2,c(11:15)])
    
    ## update consumption
    cal_52_all_list[[k+1]][[4]] <- cal_52_all_list[[k]][[4]] *as.numeric(scalar_df[k+2,c(1:5)])
    
  ## 6.4.1 Update Ouptut ---------  
   ## calculate new total ouptut   
    
    x_ij_temp <- ig_coef_df %>% 
      select(-Sectors) %>% 
      mutate(g_id = cal_52_all_list[[k+1]][[4]])%>% 
      mutate(A_tot = 0) %>% 
      mutate(B_tot = 0) %>% 
      mutate(C_tot = 0) %>% 
      mutate(D_tot = 0) %>% 
      mutate(E_tot = 0) %>% 
      mutate(emp_tot = as.numeric(hh_coef_df[,"Coef"]*labour_tot)) %>% 
      mutate(y_i = 0)
    
    for (i in 1:length(Sectors_52)) {
      
      x_ij_temp[i,"A_tot"] <-x_ij_temp[i,"sec_A"]*x_ij_temp[i,"g_id"]
      x_ij_temp[i,"B_tot"] <-x_ij_temp[i,"sec_B"]*x_ij_temp[i,"g_id"]
      x_ij_temp[i,"C_tot"] <-x_ij_temp[i,"sec_C"]*x_ij_temp[i,"g_id"]
      x_ij_temp[i,"D_tot"] <-x_ij_temp[i,"sec_D"]*x_ij_temp[i,"g_id"]
      x_ij_temp[i,"E_tot"] <-x_ij_temp[i,"sec_E"]*x_ij_temp[i,"g_id"]
      
    }
    
    x_ij_temp[1,"y_i"] <- sum(x_ij_temp[,"A_tot"])+x_ij_temp[1,"g_id"]
    x_ij_temp[2,"y_i"] <- sum(x_ij_temp[,"B_tot"])+x_ij_temp[2,"g_id"]
    x_ij_temp[3,"y_i"] <- sum(x_ij_temp[,"C_tot"])+x_ij_temp[3,"g_id"]
    x_ij_temp[4,"y_i"] <- sum(x_ij_temp[,"D_tot"])+x_ij_temp[4,"g_id"]
    x_ij_temp[5,"y_i"] <- sum(x_ij_temp[,"E_tot"])+x_ij_temp[5,"g_id"]
    
    
    cal_52_all_list[[k+1]][[3]] <- x_ij_temp[,"y_i"]
    
  
    
  ## 6.5 new deltas ----------
    
    delta_list_all[[k]] <- delta_52_all_fun(cal_52_list = cal_52_all_list[[k+1]])
    
    ## 6.51 Input deltas to delta data frame  
    
    delta_df[k+2,] <- c(delta_list_all[[k]][[1]],delta_list_all[[k]][[2]][1],delta_list_all[[k]][[3]])
    
 }
  
  
  
  return(delta_df)
  
}

test_up <- update_inputs_fun(delta_start = test_all,cal_52_list=cal_52_list,runs = 5)


