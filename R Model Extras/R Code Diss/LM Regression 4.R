## Linear Search pattern  

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

data_lenght_each <- rep(0,40)

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
  
  data_lenght_each[k] <- nrow(data_org_list[[k]])
  
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


## work out the ones that need searching
sectors_done <- coef_org_df[,"sector"]

sectors_need <- (1:40)[!(sectors %in% sectors_done)]

## set up search table
digits <- 1:97
digits_100 <- digits/100

Min_A <- round(min(coef_org_df[,"A"]),2)*100
Max_A <- round(max(coef_org_df[,"A"]),2)*100

A_search <- (Min_A:Max_A)/100

years

search_table <- data.frame("Alpha" = rep(digits_100, length(digits_100)*length(A_search)),
                           "Beta"=rep(digits_100, each = length(digits_100), length(A_search)),
                           "A" = rep(A_search, each= length(digits_100)^2),
                           "a1" = rep(0),
                           "a2" = rep(0),
                           "a3" = rep(0),
                           "a4" = rep(0),
                           "a5" = rep(0),
                           "a6" = rep(0),
                           "a7" = rep(0),
                           "a8" = rep(0),
                           "Total" = rep(0)
)

colnames(search_table)[4:(ncol(search_table)-1)] <- paste0("year_",years)

min_table <- data.frame("sectors" = sectors[sectors_need],
                        "A" = rep(0),
                        "Alpha" = rep(0),
                        "Beta" = rep(0))


search_list <- list()


for(k in 1:(length(sectors_need)-1)){
  
  
  search_list[[k]] <- search_table
  
  for(i in 1:nrow(search_list[[k]])){
    #  for(i in 1:10){
    for (j in 1:data_lenght_each[sectors_need[k]]) {
      
      search_list[[k]][i,j+3] <- search_list[[k]][i,"A"] +
        (search_list[[k]][i,"Alpha"]*data_org_list[[sectors_need[k]]][j,"G_hat"]) +
        (search_list[[k]][i,"Beta"]*data_org_list[[sectors_need[k]]][j,"L_hat"]) 
      
      search_list[[k]][i,j+3] <-  (data_org_list[[sectors_need[k]]][j,"Q_hat"] - search_list[[k]][i,j+3])^2
      
      
    }
    
    search_list[[k]][i,"Total"] <- sum(
      search_list[[k]][i,"year_2019"],
      search_list[[k]][i,"year_2018"],
      search_list[[k]][i,"year_2017"],
      search_list[[k]][i,"year_2016"],
      search_list[[k]][i,"year_2015"],
      search_list[[k]][i,"year_2014"],
      search_list[[k]][i,"year_2013"],
      search_list[[k]][i,"year_2010"],
      na.rm = T)
    
    
  }
  
  temp_match <- match(x=min(search_list[[k]][,"Total"]), table = search_list[[k]][,"Total"])
  
  min_table[k,2] <- search_list[[k]][temp_match,"A"]
  min_table[k,3] <- search_list[[k]][temp_match,"Alpha"]
  min_table[k,4] <- search_list[[k]][temp_match,"Beta"]
  
  print(k)
  
}

write.csv(x = search_list[[2]],file = "full_search_2.csv")
