## Main Model -------------

## 0.0 Assumptions ----------

## Benchmark Year

Bench_year <- 2015
Interest_rate <- 0.1
effective_tax <- 0.3
min_price <- 0.001
alter_factor <- 2
runs <- 17

## 1.0 Code Blocks ---------

## Packages
source("01_Packages.R")
## Data
source("02_read_data_v2.R")

## Start Point Data
source("02a_Start_point_data.R")

## Import functions

## Calibration
source("03_Calibration_v3.R")

## General Equilibrium Functions
source("04_CGE_Functions_v_2.R")



## 2.0 Start Point -----------


## run calibration on start


test_function_cal_first <- function_calibration_3(price = price_cal_test,
                                          labour = labour_cal_test, 
                                          capital = capital_cal_test,
                                          inter_goods_coef = ig_coef_sum,
                                          inter_totals = inter_total_cal_test,
                                          salary = salary_cal_test,
                                          interest_rate = interest_rate_cal_test,
                                          demand_ratio = demand_ratios_cal_test,
                                          sectors = sectors)








## run the model

for(m in 1:runs){
  
  if(m==1){
    
    start_time <- Sys.time()
    
  }
  
  cal_input[[1]] <- test_function_cal_first

  test_delta_function <- function_test_delta_2(test_function_cal =  cal_input[[m]], runs = m)

  delta_list[[m]] <- test_delta_function[[2]]
  prices_list[[m]] <- test_delta_function[[1]]
  delta_F_list[[m]] <- test_delta_function[[3]]
  
  test_function_cal_step <- function_calibration_3(price = test_delta_function[[1]],
                                                 labour = cal_input[[m]][[9]], 
                                                 capital = cal_input[[m]][[11]],
                                                 inter_totals = cal_input[[m]][[14]],
                                                 inter_goods_coef = ig_coef_sum,
                                                 salary = test_delta_function[[4]],
                                                 interest_rate = cal_input[[m]][[15]],
                                                 demand_ratio = demand_ratios_cal_test,
                                                 sectors = sectors)
  

  cal_input[[m+1]] <- test_function_cal_step
  print(m)
  if(m==runs){
    
    end_time <- Sys.time()
    
    print(end_time-start_time)
  }
  

}


