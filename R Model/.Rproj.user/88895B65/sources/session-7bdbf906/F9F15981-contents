## Main Model -------------

## 0.0 Assumptions ----------

## Benchmark Year

Bench_year <- 2015
Interest_rate <- 0.1
effective_tax <- 0.3
min_price <- 0.001
alter_factor <- 2
runs <- 2

## 1.0 Code Blocks ---------

## Packages
source("01_Packages.R")
## Data
source("02_read_data_v2.R")

## Import functions

## Calibration
source("03_Calibration_v2.R")
## General Equilibrium Functions
source("04_CGE_Functions.R")


## 2.0 Start Point -----------

## take data from read_data_function
price_cal_test  <- rep(1, length(sectors))
labour_cal_test  <- primary_factor_2015[,"labour"]
capital_cal_test <- primary_factor_2015[,"capital"]
inter_goods_cal_test <- ig_amt_aut %>% 
  select(-Sectors)
inter_total_cal_test <- ig_sum_aut[,"new_int_good"]
salary_cal_test <- sala_df[,"salary"]
interest_rate_cal_test <- Interest_rate
demand_ratios_cal_test <- demand_ratio_2015
sectors_cal_test <- sectors

## run calibration on start

test_function_cal_first <- function_calibration(price = price_cal_test,
                                          labour = labour_cal_test, 
                                          capital = capital_cal_test,
                                          inter_goods = inter_goods_cal_test,
                                          inter_totals = inter_total_cal_test,
                                          salary = salary_cal_test,
                                          interest_rate = interest_rate_cal_test,
                                          demand_ratio = demand_ratios_cal_test,
                                          sectors = sectors)

test_function_cal[[7]]

test_delta_function <- function_test_delta(test_function_cal = test_function_cal_first)

length(test_delta_function[[3]])

for(k in 1:runs){
  
  test_function_cal_step <- function_calibration(price = test_delta_function,
                                                 labour = labour_cal_test, 
                                                 capital = capital_cal_test,
                                                 inter_goods = inter_goods_cal_test,
                                                 inter_totals = inter_total_cal_test,
                                                 salary = salary_cal_test,
                                                 interest_rate = interest_rate_cal_test,
                                                 demand_ratio = demand_ratios_cal_test,
                                                 sectors = sectors)
  
  print(k)
  test_delta_function <- function_test_delta(test_function_cal =  test_function_cal_step)
  
  
}


