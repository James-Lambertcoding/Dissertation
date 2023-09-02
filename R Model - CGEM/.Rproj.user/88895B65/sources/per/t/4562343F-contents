## 02a_Start_point_data.R

## take data from read_data_function
price_cal_test  <- rep(1, length(sectors))
labour_cal_test  <- primary_factor_2015[,"labour"]
capital_cal_test <- primary_factor_2015[,"capital"]
inter_goods_cal_test <- ig_amt_aut %>% 
  select(-Sectors)
inter_total_cal_test <- ig_sum_aut[,"new_int_good"]
salary_cal_test <- sala_df[,"salary"]
interest_rate_cal_test <- rep(Interest_rate,length(sectors))
demand_ratios_cal_test <- demand_ratio_2015
sectors_cal_test <- sectors



## Prepare the model

cal_input <- list()
delta_list <- list()
prices_list <-list()
delta_F_list <- list()

for (m in 1:runs) {
  
  cal_input[[m]] <- list()
  
}
