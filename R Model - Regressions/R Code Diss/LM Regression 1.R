# Production Function Regression

## Search for Alpha and Beta

## 0.0 packages -------------

library(dplyr)

## 1.0 Data ---------------

data_name <- "UK_prod_data_full.csv"
data_df <- read.csv(file = data_name, stringsAsFactors = F)

auto_df <- data_df %>% 
  select(Year, Q_auto, K_Auto, L_auto) %>% 
  mutate(ln_out = log(Q_auto/K_Auto)) %>% 
  mutate(ln_lab = log(L_auto/K_Auto)) %>% 
  filter(Year < 2020)

rem_df <- data_df %>% 
  select(Year, Q_rem, K_rem,L_rem) %>% 
  mutate(ln_out = log(Q_rem/K_rem)) %>% 
  mutate(ln_lab = log(L_rem/K_rem)) %>% 
  filter(Year < 2020)



## 2.0 Linear regression ---------------

lm_auto = lm(ln_out ~ ln_lab, data = auto_df)
lm_rem =lm(ln_out ~ ln_lab, data = rem_df)


