## Fixed Effects Regression:

## 0.0 Package -----------

library(dplyr)
library(Matrix)
library(glmnet)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Matrix)
library(glmnet)
library(lfe)

## 1.0 Data ------------------

data_name_1 <- "Price_data/regression_data_hh.csv"
data_name_2 <- "Price_data/price_quantity.csv"
#data_name_3 <- "Price_data/multiple_regression_data.csv"

hh_df <- read.csv(file = data_name_1, stringsAsFactors = F)
sh_df <- read.csv(file = data_name_2, stringsAsFactors = F)
#mh_df <- read.csv(file = data_name_3, stringsAsFactors = F)
mh_df <- sh_df

hh_df_1 <- hh_df %>% 
  filter(Incl ==1)

colnames(hh_df_1)

## Good types:

good_types_name <- c("Food & non-alcoholic drinks",
                     "Alcoholic drinks, tobacco & narcotics",
                     "Clothing & footwear",
                     "Housing(net), fuel & power",
                     "Household goods & services",
                     "Health",
                     "Transport",
                     "Transpoprt - Automotive Vehicles",
                     "Transpoprt - Other",
                     "Communication",
                     "Recreation & culture",
                     "Education",
                     "Restaurants & hotels",
                     "Miscellaneous goods & services")

good_one_trans <- good_types_name[-7]


## 2.0 Data Manipulation -------------

good_types <- unique(sh_df[,"Good.Name"])

split_list <- list()
multi_list <- list()


for(i in 1:length(good_types)){
  
  split_list[[i]] <- sh_df %>% 
    filter(Good == i) %>% 
    filter(Quantity !=0) %>% 
    mutate(log_income = log(Income.Amount)) %>% 
    mutate(log_price = log(Price)) %>% 
    mutate(Income.Bracket = paste0("perc_",Income.Group*10)) %>% 
    mutate(Income.Desc = case_when(
      Income.Group <= 3 ~ "Low",
      Income.Group <= 7 & Income.Group >3 ~ "Medium",
      Income.Group >7 ~ "High"
    ))
  
  multi_list[[i]] <- mh_df %>% 
    filter(Good == i) %>% 
    filter(Quantity !=0) %>% 
    select(-Price_7)
  
  
}

## 3.0 Fixed Effects -----------


fe_list <- list()
fe_df <- data.frame( "good" = good_types,
                     "coef_1" = rep(0),
                     "coef_1_pr" = rep(0),
                     "model_r2" = rep(0),
                     "model_ad_r2" = rep(0))


for(i in 1:length(good_types)){
  
  ## Run Regression
  fe_list[[i]] <- felm(Quantity ~ Price|Income.Group,data = split_list[[i]])  
  
  ## Input Summary into data frame
  fe_df[i,2] <- summary(fe_list[[i]])[[7]][1]
  fe_df[i,3] <- summary(fe_list[[i]])[[7]][4]
  fe_df[i,4] <- summary(fe_list[[i]])[22]
  fe_df[i,5] <- summary(fe_list[[i]])[[23]]
  
}

## 4.0 Save Results

fe_df <- fe_df %>% 
  filter(good != 7)

write.csv(x = fe_df,file = "Output_3/Linear Coefs/Price_Fixed_Effects.csv")
