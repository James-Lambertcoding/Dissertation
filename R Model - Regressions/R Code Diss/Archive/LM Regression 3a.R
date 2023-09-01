## Linear Regression including Intermittent goods

## 0.0 packages -------------

library(dplyr)

## 1.0 Data ---------------

data_name <- "UK_all_data.csv"
data_df <- read.csv(file = data_name, stringsAsFactors = F)

## variables
sectors <- colnames(data_df)[3:ncol(data_df)]
years <- unique(data_df[,"Year"])
types <- unique(data_df[,"Type"])
types_simple <- c("G", "L", "Q", "K")

## Data Manipulation Preparation
blank_df <- data.frame("year" = years,
                       "a1" = rep(0),
                       "a2" = rep(0),
                       "a3" = rep(0),
                       "a4" = rep(0))

colnames(blank_df)[2:5] <- types_simple

data_list <- list()
data_org_list <- list()

data_length <- 0

## 1.1 Data Manipulation ------------------
## Turn the single DF into a list by sectors

for (k in 1:length(sectors)) {
  
  data_list[[k]] <- blank_df
  
  for(i in 1:nrow(data_list[[k]])){
    
    for(j in 1:(ncol(data_list[[k]])-1)){
      
      data_list[[k]][i,j+1] <- data_df[j+((i-1)*length(types)),k+2]
      
    }
    
  }
  
  data_org_list[[k]] <- data_list[[k]] %>% 
    mutate(Q_hat = log(Q/K)) %>% 
    mutate(G_hat = log(G/K)) %>% 
    mutate(L_hat = log(L/K)) %>% 
    filter(G != 0) %>% 
    filter(L != 0) %>% 
    filter(Q != 0) %>% 
    filter(K != 0)  
    
  if(nrow(data_org_list[[k]])> 0){
    
    data_length <- data_length +1
    
  }    
  
}


## 2.0 Linear Regression ------------------
## data_lenght != length(sectors)


lin_list <- list()

coef_df <- data.frame("sector" = sectors[1:39],
                      "A" = rep(0),
                      "Alpha" = rep(0),
                      "Beta" = rep(0))


for(k in 1:data_length){
  
  lin_list[[k]] <- lm(Q_hat ~ G_hat + L_hat, data = data_org_list[[k]])
  
## convert to coefficent table
  coef_df[k,1] <- sectors[k]
  coef_df[k,2] <- lin_list[[k]][[1]][1]
  coef_df[k,3] <- lin_list[[k]][[1]][2]
  coef_df[k,4] <- lin_list[[k]][[1]][3]
  
  
}

coef_org_df <- coef_df %>% 
  mutate(Gamma = 1 - Alpha - Beta) %>% 
  filter(A > 0) %>% 
  filter(Alpha > 0) %>% 
  filter(Beta > 0) %>% 
  filter(Gamma > 0) %>% 
  filter(Alpha < 1) %>% 
  filter(Beta < 1) %>% 
  filter(Gamma < 1)  
  
  
write.csv(x = coef_org_df,file = "Production_coef.csv")  







