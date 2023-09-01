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
data_name_2 <- "Price_data/Simple_All_data.csv"
data_name_3 <- "Price_data/multiple_regression_data.csv"

hh_df <- read.csv(file = data_name_1, stringsAsFactors = F)
sh_df <- read.csv(file = data_name_2, stringsAsFactors = F)
mh_df <- read.csv(file = data_name_3, stringsAsFactors = F)


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


## 2.0 Income Elasticity -------------

good_types <- unique(sh_df[,"Good.Name"])

split_list <- list()
multi_list <- list()


for(i in 1:length(good_types)){
  
  split_list[[i]] <- sh_df %>% 
    filter(Good == i) %>% 
    filter(Spend !=0) %>% 
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
    filter(Spend !=0) %>% 
    select(-Price_7)
  
  
}


for
