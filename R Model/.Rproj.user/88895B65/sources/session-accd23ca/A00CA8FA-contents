## 02_read_data_v1.R ------------------

## 1.0 Data Names ---------------

hh_name <- "Data/household_T_19_08.csv"


## 2.0 Read -------------------

hh_df <- read.csv(file = hh_name, stringsAsFactors = F)


## 3.0 Manipulation ------------

hh_df_1 <- hh_df %>% 
  ## Make it only average household
  filter(Percentile == "All") %>% 
  ## combine 1 and 2
  mutate(X1_2 = X1+X2) %>% 
  ## remove x1 and x2 and x7
  select(-X1) %>% 
  select(-X2) %>% 
  select(-X7) %>% 
  ## remove Percentile
  select(-Percentile) %>% 
  ## remove Lower Bound
  select(-Lower.Bound)

## Reorder Columns
hh_df_2 <- hh_df_1[,c(1,ncol(hh_df_1),2:(ncol(hh_df_1)-1))] 

for(j in 1:ncol(hh_df_2)){
  
  hh_df_2[,j] <- as.numeric(hh_df_2[,j])
}

final_demand <- colnames(hh_df_2)[2:ncol(hh_df_2)]
