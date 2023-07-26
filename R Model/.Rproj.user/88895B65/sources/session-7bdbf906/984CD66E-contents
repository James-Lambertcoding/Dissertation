## 02_read_data_v2.R

## 1.0 Data Names ---------------


ig_coef_name <- "Data/IG_coef.csv"
ig_tot_name <- "Data/IG_total_2015.csv"
ig_sum_name <- "Data/IG_summary_2015.csv"
prim_name <- "Data/primary_factors.csv"
sala_name <- "Data/salary.csv"


## 2.0 Read -------------------

ig_coef_df <- read.csv(file = ig_coef_name, stringsAsFactors = F)
ig_amount_df <- read.csv(file= ig_tot_name, stringsAsFactors = F)
ig_sum_df <- read.csv(file= ig_sum_name, stringsAsFactors = F)
prim_df <- read.csv(file= prim_name, stringsAsFactors = F)
sala_df <- read.csv(file= sala_name, stringsAsFactors = F)

## 3.0 Manipulation ------------

colnames(ig_sum_df) <- c("Sectors",
                           "Int_total",
                           "Households",
                           "Government",
                           "Capital_increase",
                           "Total_demand",
                           "Exports")

sectors <- unique(ig_sum_df[,"Sectors"])

## 3.1 Autarky --------------------

## new Aut data frames
ig_sum_aut <- ig_sum_df %>% 
  mutate(export_ratio = Exports/Total_demand) %>% 
  mutate(new_int_good = 0)

## Remove exports from ig_tot
ig_amt_aut <- ig_amount_df

##Autarky data frame, remove export percentage of int goods
for(j in 2:ncol(ig_amt_aut)){
  
  ig_amt_aut[,j] <- ig_amt_aut[,j]*(1-ig_sum_aut[j-1,"export_ratio"])
  ig_amt_aut[,j] <- ig_amt_aut[,j] *10^6
}


for(i in 1:nrow(ig_amt_aut)){
  
  ig_sum_aut[i,"new_int_good"] <- sum(ig_amt_aut[i,2:ncol(ig_amt_aut)])
  
}

aut_ig_total <- sum(ig_sum_aut[,"new_int_good"])
