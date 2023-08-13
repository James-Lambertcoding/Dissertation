
## 0.0 packages -------------

library(dplyr)

## 1.0 Data ---------------

data_name <- "UK_IOT_full.csv"
data_df <- read.csv(file = data_name, stringsAsFactors = F)

auto_df <- data_df %>% 
  select(Year, Q_auto, G_auto, K_autp, L_auto) %>% 
  filter(Year != 2016) %>% 
  mutate(Q_auto_ln = log(Q_auto)) %>% 
  mutate(G_auto_ln = log(G_auto)) %>% 
  mutate(K_auto_ln = log(K_autp)) %>% 
  mutate(L_auto_ln = log(L_auto)) 

ln_simple <- lm(Q_auto_ln ~ G_auto_ln + K_auto_ln + L_auto_ln, data = auto_df)  


