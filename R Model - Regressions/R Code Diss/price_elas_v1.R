## Price_Elascity Code

## 0.0 Package -----------

library(dplyr)


## 1.0 Data ---------------

GDP_Name <- "Price_data/GDP_deflator.csv"
CPI_Name <- "Price_data/CPI_index.csv"
HHS_Name <- "Price_data/household_2019_13.csv"

GDP_df <- read.csv(file = GDP_Name, stringsAsFactors = F)
CPI_df <- read.csv(file = CPI_Name, stringsAsFactors = F)
HHS_df <- read.csv(file = HHS_Name, stringsAsFactors = F)


## 1.1 Sector -----------------------

sectors <- unique(HHS_df[,"Section"])

hhs_list <- list()
hhs_2019_list <- list()
hhs_2019_unit_list <- list()



for(k in 1:length(sectors)){
  
  hhs_list[[k]] <- HHS_df %>% 
    filter(Section == sectors[k]) %>% 
    left_join(y = GDP_df[,1:2], by = "Year") %>% 
    mutate(index_2019 = (Index/income_df[1,"Index"]))
  
  hhs_2019_list[[k]] <- hhs_list[[k]]
  
  for(j in 4:12){
    
    hhs_list[[k]][,j] <- as.numeric(hhs_list[[k]][,j])
    hhs_2019_list[[k]][,j] <- hhs_list[[k]][,j]/hhs_list[[k]][,"index_2019"]
    
  }
  
  if(k != 1){
    
    hhs_2019_list[[k]] <- hhs_2019_list[[k]] %>% 
      left_join(CPI_df[,c(1,k)], by ="Year")
  
    
    hhs_2019_unit_list[[k]] <- hhs_2019_list[[k]]
    
    hhs_2019_unit_list[[k]][,16] <- hhs_2019_list[[k]][,16]/100
    
    for(j in 4:12){
      
      hhs_2019_unit_list[[k]][,j] <- hhs_2019_list[[k]][,j]/hhs_2019_list[[k]][,16]
      
    }
    
    
    
    }
  
}

ncol(hhs_2019_list[[2]])
hhs_2019_list[[2]]

## 1.2 Income ---------------

income_df <- HHS_df %>% 
  filter(Section == "Lower Bound") %>% 
  left_join(y = GDP_df[,1:2], by = "Year") %>% 
  mutate(index_2019 = (Index/income_df[1,"Index"]))

income_df_2019 <- income_df

for(j in 4:12){
  
  income_df_2019[,j] <- as.numeric(income_df_2019[,j])
  income_df[,j] <- as.numeric(income_df[,j])
  income_df_2019[,j] <- income_df[,j]/income_df[,"index_2019"]
  
}


