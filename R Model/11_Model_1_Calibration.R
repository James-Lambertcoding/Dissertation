## Model_1 Calibration.R

## 0.0 Assumptions ----------

## structure is 5x5 and 5x1
## 5 sectors
## 1 primary input

labour_tot <- 28764400


## 1.0 Data Names ---------------


ig_coef_name <- "Simple_data/prod_coef.csv"
hh_coef_name <- "Simple_data/hh_coef.csv"
emp_coef_name <- "Simple_data/emp_coef.csv"

## 2.0 Read Data in csv -------------

ig_coef_df <- read.csv(ig_coef_name, stringsAsFactors = F)
hh_coef_df <- read.csv(hh_coef_name, stringsAsFactors = F)
emp_coef_df <- read.csv(emp_coef_name, stringsAsFactors = F)


## 3.0 Assumptions ----------

Sectors_51 <- paste0("sec_",ig_coef_df[,"Sectors"])
M_bar <- labour_tot

price_51 <- rep(1,length(Sectors_51))
wages_51 <- rep(1,length(Sectors_51))


## 4.0 Calibration --------- 
g_id_df <- hh_coef_df %>% 
  mutate(g_id = as.numeric(Coef)*M_bar)

colnames(ig_coef_df)[2:6] <- Sectors_51

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

for (i in 1:length(Sectors_51)) {
  
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

## 4.2 Beta-------------------
  b_ij <- matrix(ncol=length(Sectors_51),nrow=length(Sectors_51),0)
  
for(i in 1:length(Sectors_51)){
  
    
  b_ij[i,1] <-  x_ij[i,"A_tot"]/x_ij[1,"y_i"]
  b_ij[i,2] <-  x_ij[i,"B_tot"]/x_ij[2,"y_i"]
  b_ij[i,3] <-  x_ij[i,"C_tot"]/x_ij[3,"y_i"]
  b_ij[i,4] <-  x_ij[i,"D_tot"]/x_ij[4,"y_i"]
  b_ij[i,5] <-  x_ij[i,"E_tot"]/x_ij[5,"y_i"]
  
}  


## 4.3 Gamma -------------
  gamma_i <- g_id_df %>% 
    mutate(y_i = x_ij[,"y_i"]) %>% 
    mutate(gamma_i = g_id/y_i)

  
  check_factors <-as.data.frame(  t(b_ij) )%>% 
    bind_cols(gamma_i) %>% 
    mutate(check_sum = V1+V2+V3+V4+V5+gamma_i)
    
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

    for(i in 1:length(Sectors_51)){
      
      for(j in 1:length(Sectors_51)){
        
        A_J[i,j+1] <- x_ij_tran[i,j]^b_ij_tran[i,j]
        
      }
    }
       
    
    ## 4.4 A_j ---------------
    
    A_J <- A_J %>% 
      mutate(emp = check_factors[,"g_id"]) %>% 
      mutate(gamma_i = check_factors[,"gamma_i"]) %>% 
      mutate(emp_gamma_i = emp^gamma_i) %>% 
      mutate(A_J = 0)
    
 for(i in 1:length(Sectors_51)){
   
   A_J[i,"A_J"] <- A_J[i,"y_i"]/(A_J[i,"x_1j_b_1j"]*A_J[i,"x_2j_b_2j"]*A_J[i,"x_3j_b_3j"]*A_J[i,"x_4j_b_4j"]*A_J[i,"x_5j_b_5j"]*A_J[i,"emp_gamma_i"])
   
 }   
    

    ## 4.5 Combine Calibration ----------
    
    cal_51_list <- list()
    
    ## Prices
    cal_51_list[[1]] <- price_51
    ## Wages
    cal_51_list[[2]] <- wages_51
    ## Total Outputs
    cal_51_list[[3]] <-gamma_i[,"y_i"]
    ## Total Consumption
    cal_51_list[[4]] <- gamma_i[,"g_id"]
    ## Alpha 
    cal_51_list[[5]] <- emp_coef_df[,"Coef"]
    ## Gamma
    cal_51_list[[6]] <- gamma_i[,"gamma_i"]
    ## labour
    cal_51_list[[7]] <- gamma_i[,"g_id"]
    ## A_j
    cal_51_list[[8]] <- A_J[,"A_J"]
    ##beta
    cal_51_list[[9]] <- b_ij
    ## ig <- x_ij
    cal_51_list[[10]] <- x_ij_only
    ## Sectors
    cal_51_list[[11]] <- Sectors_51
    
    ## 5.0 Delta Functions -------------
    
    
    ## 5.1 Delta C -------------
    delta_51_c <- function(cal_51_list){
      
      step_1 <- cal_51_list[[9]] %>% 
        mutate(prices = cal_51_list[[1]] ) %>% 
        mutate(total_supply = cal_51_list[[3]])
      
      step_1_sec <- rep(0,length(Sectors_51))
      
      for(i in 1:length(Sectors_51)){
        for(j in 1:length(Sectors_51) ){
          
        step_1_sec[i] <- step_1_sec[i] + step_1[i,j]*step_1[j,"prices"]*step_1[j,"total_supply"]
        
        }
        
      }
      
      
      step_2 <-rep(0,length(Sectors_51))
      
      step_2b <- data.frame("wages" = cal_51_list[[2]],
                            "labour" = cal_51_list[[7]]) %>% 
        mutate(labour_cost = wages *labour)
      
      step_2b_tot <- sum(step_2b[,"labour_cost"])
      
      for(i in 1:length(Sectors_51)){
        
        step_2[i] <- cal_51_list[[5]][i]*step_2b_tot
        
      }
      
      delta_51_c <- rep(0,length(Sectors_51))
      
      for(i in 1:length(Sectors_51)){
        
        delta_51_c[i] <- step_1_sec[i]+step_2b[i]-(cal_51_list[[1]][i]*cal_51_list[[3]][[i]])
        
      }
      

      
      return(delta_51_c)
      
    }
    
    ## 5.2 Delta F -------------
    
    delta_51_f <- function(cal_51_list){
      
      step_1 <- data.frame("Sectors" = cal_51_list[[11]],
                           "prices"= cal_51_list[[1]],
                           "total_supply" = cal_51_list[[3]],
                           "wages" = cal_51_list[[2]],
                           "gamma" = cal_51_list[[6]]) %>% 
        mutate(step_1 = gamma*prices*total_supply/wages)
      
      step_1_tot <- sum(step_1[,"step_1"])
      
      delta_51_f <- step_1_tot - sum(cal_51_list[[7]])
      
      return(delta_51_f)
      
      
    }

    ## 5.3 Delta pi --------------
    
    delta_51_pi <- function(cal_51_list){
      
      
      step_1 <- cal_51_list[[9]]
      
      for(i in 1:length(Sectors_51)){
        for(j in 1:length(Sectors_51)){
          
          step_1[i,j] <- (cal_51_list[[1]][i]/cal_51_list[[9]][i,j])^cal_51_list[[9]][i,j]
          
        }
        
      }
      
      step_1_sec <- rep(0,length(Sectors_51))
      
      for(j in 1:length(Sectors_51)){
    
          step_1_sec[j] <- prod(step_1[,j])
          
      }
      
      step_2 <-  cal_51_list[[2]]
      
      
      for(j in 1:length(Sectors_51)){
        
        step_2[j] <- (cal_51_list[[2]][j]/cal_51_list[[6]][j])^cal_51_list[[6]][j]
        
      }
      
      step_1_2 <-  rep(0,length(Sectors_51))
      
      for(j in 1:length(Sectors_51)){
      
        step_1_2[j] <-  cal_51_list[[8]][j]*step_1_sec[j]*step_2[j]
          
      }
      
      delta_51_pi <- cal_51_list[[1]]
      
      for(j in 1:length(Sectors_51)){
        
        delta_51_pi[j] <-  delta_51_pi[[j]] - step_1_2[j]
        
      }
      
      return(delta_51_pi)
      
    }
    
    test_f <- delta_51_f(cal_51_list = cal_51_list)    
    test_c <- delta_51_f(cal_51_list = cal_51_list)
    test_pi <- delta_51_pi(cal_51_list = cal_51_list)
    