## Price_Elascity Code v2

## 0.0 Package -----------

library(dplyr)
library(Matrix)
library(glmnet)


## 0.1 Assumptions ------------

point_year <- 2015

## 1.0 Data ---------------

GDP_Name <- "Price_data/GDP_deflator.csv"
CPI_Name <- "Price_data/CPI_index.csv"
HHS_Name <- "Price_data/household_T_19_08.csv"

GDP_df <- read.csv(file = GDP_Name, stringsAsFactors = F)
CPI_df <- read.csv(file = CPI_Name, stringsAsFactors = F)
HHS_df <- read.csv(file = HHS_Name, stringsAsFactors = F)


## 1.1 Household data -----------------------

percentiles <- unique(HHS_df[,"Percentile"])

cpi_re_df <- CPI_df[,-8]

hhs_list <- list()
hhs_per_list <- list()
hhs_change <- list()

for (k in 1:length(percentiles)) {
  
  
  hhs_list[[k]] <- HHS_df %>% 
    filter(Percentile == percentiles[k]) %>% 
    filter(Year > 2009) 
  
  for(j in 3:17){
    
    hhs_list[[k]][,j] <- as.numeric(hhs_list[[k]][,j])
    hhs_list[[k]][is.na(hhs_list[[k]][,j]),j]<-0
     
  }
  
  
  hhs_list[[k]] <- hhs_list[[k]]%>% 
   mutate(total = X1 + X2 +X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)
  
  hhs_per_list[[k]] <- hhs_list[[k]][,-10]
  
  for(j in 3:16){
    
    hhs_per_list[[k]][,j] <- hhs_list[[k]][,j]/hhs_list[[k]][,"total"]
    
  }
  
  ## calculate the percentage change in percentage in each price bracket
  
  hhs_change[[k]] <- hhs_per_list[[k]]
  
  ## which rows the constant row
  temp_row <- match(point_year,hhs_change[[k]][,"Year"])
  
  for(i in 1:nrow(hhs_change[[k]])){
    for(j in 4:17){
      
      hhs_change[[k]][i,j] <- (hhs_per_list[[k]][i,j]-hhs_per_list[[k]][temp_row,j])/ hhs_per_list[[k]][temp_row,j]
      
    }
    
    
  }
  
  
}

## 1.2 CPI data ------------

cpi_re_df <- cpi_re_df %>% 
  filter(Year >=2010) %>% 
  filter(Year <= 2019)

cpi_per_df <- cpi_re_df

for(i in 1:nrow(cpi_per_df)){
    
  temp_row <- match(point_year,cpi_re_df[,"Year"])
    
  for(j in 2:ncol(cpi_per_df)){
    
    cpi_per_df[i,j] <- (cpi_re_df[i,j]-cpi_re_df[temp_row,j])/ cpi_re_df[temp_row,j]
    
  }
  
}

cpi_per_df_2 <- cpi_per_df %>% 
  arrange(desc(Year))

## 1.3 Households by year ------------------

house_hold_df <- hhs_change[[1]]

for(k in 2:length(hhs_change)){
  
  house_hold_df <- house_hold_df %>% 
    bind_rows(hhs_change[[k]]) %>% 
    select(-Lower.Bound) %>% 
    select(-total)
  
}

## Sectors
sectors <- colnames(house_hold_df)[3:ncol(house_hold_df)]

## combine all data (income levels) into one list 

## separate by income level
price_data_list <- list()
## combined in on list
price_data_all <- list()


## re-organise data
for(k in 1:length(hhs_change)){
  
  price_data_list[[k]] <- list()
  
    for(m in 1:length(sectors)){
      
      ## create a temp_df to join allowing the colname to be changed
      temp_join <- hhs_change[[k]][,c(2,3+m)]
      colnames(temp_join)[2] <- paste0(colnames(temp_join)[2],"_change")
      
      price_data_list[[k]][[m]] <- cpi_per_df_2 %>% 
        left_join(temp_join, by = "Year") %>% 
        filter(Year != 2015)
      
      if(k == 1){
        
        price_data_all[[m]] <- price_data_list[[k]][[m]]
        
      } else {
        
        price_data_all[[m]] <- price_data_all[[m]] %>% 
          bind_rows(price_data_list[[k]][[m]])
        
      }
      
    }
  
  
}

## 2.0 LASSO Regression ---------------------

## Model 2.1 ---------------------
## Grouped data include own price change

## Data sets for the LASSO Regression
resp_list_1 <- list()
input_lists_1 <- list()

for (k in 1:length(price_data_all)) {
  
  resp_list_1[[k]] <- price_data_all[[k]][,ncol(price_data_all[[k]])]
  input_lists_1[[k]] <- price_data_all[[k]][,-ncol(price_data_all[[k]])] %>% 
    select(-Year)
  
  input_lists_1[[k]] <- data.matrix(input_lists_1[[k]])
  
}

## lists for LASSO solutions
cv_model_list <- list()
best_lambda <- rep(0, length(input_lists_1))

for(k in 1:length(resp_list_1)){
  
  #perform k-fold cross-validation to find optimal lambda value
  cv_model_list[[k]] <- cv.glmnet(x = input_lists_1[[k]], y = resp_list_1[[k]], alpha = 1, intercept = FALSE)
  #find optimal lambda value that minimizes test MSE
  best_lambda[k] <- cv_model_list[[k]]$lambda.min
  print(k)
}

#find coefficients of best model
best_model_list_1 <- list()

## construct best models
for(k in 1:length(resp_list_1)){
  
  best_model_list_1[[k]] <- glmnet(x = input_lists_1[[k]], resp_list_1[[k]], alpha = 1, lambda = best_lambda[k],intercept = FALSE)
  
}

## Model 2.2 ---------------------
## Grouped data exclude own price change

## Data sets for the LASSO Regression
resp_list_2 <- list()
input_lists_2 <- list()

for (k in 1:length(price_data_all)) {
  
  resp_list_2[[k]] <- price_data_all[[k]][,ncol(price_data_all[[k]])]
  input_lists_2[[k]] <- price_data_all[[k]][,-ncol(price_data_all[[k]])] %>% 
    select(-Year)
  
  input_lists_2[[k]] <- data.matrix(input_lists_2[[k]])
  ## remove own price change
  input_lists_2[[k]] <- input_lists_2[[k]][,-k]
  
  
}


## lists for LASSO solutions
cv_model_list_2 <- list()
best_lambda_2 <- rep(0, length(input_lists_2))

## Run LASSO
for(k in 1:length(resp_list_2)){
  
  #perform k-fold cross-validation to find optimal lambda value
  cv_model_list_2[[k]] <- cv.glmnet(x = input_lists_2[[k]], y = resp_list_2[[k]], alpha = 1, intercept = FALSE)
  #find optimal lambda value that minimizes test MSE
  best_lambda_2[k] <- cv_model_list_2[[k]]$lambda.min
  print(k)
  
}

#find coefficients of best model
best_model_list_2 <- list()

## construct best models
for(k in 1:length(resp_list_2)){
  
  best_model_list_2[[k]] <- glmnet(x = input_lists_2[[k]], resp_list_2[[k]], alpha = 1, lambda = best_lambda_2[k],intercept = FALSE)
  
}

## coef dataframe
coef_df <- data.frame(matrix(ncol=length(best_model_list_2), nrow=length(best_model_list_2),0))
colnames(coef_df) <- sectors


## bring coef together
for (k in 1:length(best_model_list_2)) {
  
  for(j in 1:(length(best_model_list_2)-1)){
    
    if(j>=k){
      
      coef_df[k,j+1] <-coef(best_model_list_2[[k]])[j+1] 
    }else{
      
      coef_df[k,j] <-coef(best_model_list_2[[k]])[j+1] 
    }
    
    
  }
  
  
}

write.csv(x = coef_df, file = "Price_data/sub_elascity.csv")


## Individual matrix by income levels

## Data sets for the LASSO Regression
resp_lists <- list()
input_lists <- list()

## Manipulate Data for LASSO Regression
for (m in 1:length(price_data_list)) {
  resp_lists[[m]] <- list()
  input_lists[[m]] <- list()
  
  for(k in 1:length(price_data_list[[m]])){
    
    resp_lists[[m]][[k]] <- price_data_list[[m]][[k]][,ncol(price_data_list[[m]][[k]])]
    input_lists[[m]][[k]] <- price_data_list[[m]][[k]][,-ncol(price_data_list[[m]][[k]])] %>% 
      select(-Year)
    
    input_lists[[m]][[k]] <- data.matrix(input_lists[[m]][[k]])
  }
  
}

cv_model_lists <- list()
best_lambda_list <- list()

for(m in 1:length(resp_lists)){
  
  cv_model_lists[[m]] <- list()
  best_lambda_list[[m]] <- rep(0, length(resp_lists[[m]]))
  
  for(k in 1:length(resp_lists[[m]])){
  
    #perform k-fold cross-validation to find optimal lambda value
  
    cv_model_lists[[m]][[k]] <- cv.glmnet(x = input_lists[[m]][[k]], y = resp_lists[[m]][[k]], alpha = 1, intercept = FALSE)
  
    #find optimal lambda value that minimizes test MSE
  
    best_lambda_list[[m]][k] <- cv_model_lists[[m]][[k]]$lambda.min
 
  }
  }

#find coefficients of best model
best_model_lists <- list()

for(m in 1:length(resp_lists)){
  
  best_model_lists[[m]] <- list()
  
  
  for(k in 1:length(resp_lists[[m]])){
    
  best_model_lists[[m]][[k]] <- glmnet(x = input_lists[[m]][[k]], y = resp_lists[[m]][[k]], alpha = 1, intercept = FALSE, lambda = best_lambda_list[[m]][k])
  
  }
  print (m)
}

coef(best_model_lists[[1]][[1]])
