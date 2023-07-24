## 02_read_data_v1.R ------------------

## 1.0 Data Names ---------------

hh_name <- "Data/household_T_19_08.csv"
ig_coef_name <- "Data/IG_coef.csv"
ig_tot_name <- "Data/IG_total_2015.csv"
ig_sum_name <- "Data/IG_summary_2015.csv"
prim_name <- "Data/primary_factors.csv"
sala_name <- "Data/salary.csv"


## 2.0 Read -------------------

hh_df <- read.csv(file = hh_name, stringsAsFactors = F)
ig_coef_df <- read.csv(file = ig_coef_name, stringsAsFactors = F)
ig_tot_df <- read.csv(file= ig_tot_name, stringsAsFactors = F)
ig_sum_df <- read.csv(file= ig_sum_name, stringsAsFactors = F)
prim_df <- read.csv(file= prim_name, stringsAsFactors = F)
sala_df <- read.csv(file= sala_name, stringsAsFactors = F)


## 3.0 Manipulation ------------

## 3.1 Household Expenidture -----------------
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

## 3.2 Ouptut_Summary -----------------

ig_sum_df_2 <- ig_sum_df %>% 
  rename(total_demand = 'Total.Demand.for.Products.at.Basic.Prices') %>% 
  rename(Sectors = Sector) %>% 
  select(Sectors, total_demand)

## make numeric
ig_sum_df_2[,2] <- as.numeric(ig_sum_df_2[,2])

sectors <- unique(ig_sum_df_2[,"Sectors"])

## 3.3 Output_totals ------------------

for(j in 2:ncol(ig_tot_df)){
  
  ig_tot_df[,j] <- as.numeric(ig_tot_df[,j])
  
}

## 3.4 Primary Factors -------------

for(j in 2:ncol(prim_df)){
  
  prim_df[,j] <- as.numeric(prim_df[,j])
  
}
