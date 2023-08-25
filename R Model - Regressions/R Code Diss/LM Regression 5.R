## spherical Search pattern  

## 0.0 packages -------------

library(dplyr)

## 1.0 Data ---------------

data_name <- "UK_all_data.csv"
data_df <- read.csv(file = data_name, stringsAsFactors = F)

## variables
sectors <- colnames(data_df)[3:ncol(data_df)]
years <- unique(data_df[,"Year"])
types <- unique(data_df[,"Type"])
types_simple <- c("G", "L", "Q", "K")

## Data Manipulation Preparation
blank_df <- data.frame("year" = years,
                       "a1" = rep(0),
                       "a2" = rep(0),
                       "a3" = rep(0),
                       "a4" = rep(0))

colnames(blank_df)[2:5] <- types_simple

data_list <- list()
data_org_list <- list()

data_length <- 0

data_lenght_each <- rep(0,40)

## 1.1 Data Manipulation ------------------
## Turn the single DF into a list by sectors

for (k in 1:length(sectors)) {
  
  data_list[[k]] <- blank_df
  
  for(i in 1:nrow(data_list[[k]])){
    
    for(j in 1:(ncol(data_list[[k]])-1)){
      
      data_list[[k]][i,j+1] <- data_df[j+((i-1)*length(types)),k+2]
      
    }
    
  }
  
  data_org_list[[k]] <- data_list[[k]] %>% 
    mutate(Q_hat = log(Q/K)) %>% 
    mutate(G_hat = log(G/K)) %>% 
    mutate(L_hat = log(L/K)) %>% 
    filter(G != 0) %>% 
    filter(L != 0) %>% 
    filter(Q != 0) %>% 
    filter(K != 0)  
  
  if(nrow(data_org_list[[k]])> 0){
    
    data_length <- data_length +1
    
  }  
  
  data_lenght_each[k] <- nrow(data_org_list[[k]])
  
}



## 2.0 Linear Regression ------------------
## data_lenght != length(sectors)


lin_list <- list()

coef_df <- data.frame("sector" = sectors[1:39],
                      "A" = rep(0),
                      "Alpha" = rep(0),
                      "Beta" = rep(0))


for(k in 1:data_length){
  
  lin_list[[k]] <- lm(Q_hat ~ G_hat + L_hat, data = data_org_list[[k]])
  
  ## convert to coefficent table
  coef_df[k,1] <- sectors[k]
  coef_df[k,2] <- lin_list[[k]][[1]][1]
  coef_df[k,3] <- lin_list[[k]][[1]][2]
  coef_df[k,4] <- lin_list[[k]][[1]][3]
  
  
}

coef_org_df <- coef_df %>% 
  mutate(Gamma = 1 - Alpha - Beta) %>% 
  filter(A > 0) %>% 
  filter(Alpha > 0) %>% 
  filter(Beta > 0) %>% 
  filter(Gamma > 0) %>% 
  filter(Alpha < 1) %>% 
  filter(Beta < 1) %>% 
  filter(Gamma < 1)  


## work out the ones that need searching
sectors_done <- coef_org_df[,"sector"]

sectors_need <- (1:40)[!(sectors %in% sectors_done)]


Min_A <- min(coef_org_df[,"A"])
Max_A <- max(coef_org_df[,"A"])
Mid_A <- round((Max_A + Min_A)/2,2)

## 3.0 Search Alogithm ------------

## new test table
test_blank_df <- data.frame("A" = rep(0,27),
                            "Alpha" = rep(0),
                            "Beta" = rep(0),
                            "Gamma" = rep(0),
                            "Check_Sum" = rep(0))

## generate new points to test
## start_point should be vector length 3

## need a minimum size of gamma to confine the search space

min_gamma <- 0.001

vector_fun <- function(start_point, size){
  
  # 6 single direction changes
  # 12 double direction changes
  # 8 triple direction changes
  # 1 start point
  ## 27 rows total
  
  test_points <- test_blank_df
  
  for(i in 1:nrow(test_points)){
    
    test_points[i,1:3] <- start_point
    
  }
  
  ## change the points - Singles
  test_points[1,1] <- test_points[1,1] + size
  test_points[2,1] <- test_points[2,1] - size
  test_points[3,2] <- test_points[3,2] + size
  test_points[4,2] <- test_points[4,2] - size
  test_points[5,3] <- test_points[5,3] + size
  test_points[6,3] <- (test_points[6,3] - size) %% 1
  
  ## change the points - double
  ## A and Alpha Directions
  test_points[7,1] <- test_points[7,1] + (size/(2^0.5)) 
  test_points[7,2] <- test_points[7,2] + (size/(2^0.5))
  
  test_points[8,1] <- test_points[8,1] - (size/(2^0.5))
  test_points[8,2] <- test_points[8,2] + (size/(2^0.5))
  
  test_points[9,1] <- test_points[9,1] + (size/(2^0.5))
  test_points[9,2] <- test_points[9,2] - (size/(2^0.5))
  
  test_points[10,1] <- test_points[10,1] - (size/(2^0.5))
  test_points[10,2] <- test_points[10,2] - (size/(2^0.5))
  
  ## A and Beta
  test_points[11,1] <- test_points[11,1] + (size/(2^0.5))
  test_points[11,3] <- test_points[11,3] + (size/(2^0.5))
  
  test_points[12,1] <- test_points[12,1] - (size/(2^0.5))
  test_points[12,3] <- test_points[12,3] + (size/(2^0.5))
  
  test_points[13,1] <- test_points[13,1] + (size/(2^0.5))
  test_points[13,3] <- test_points[13,3] - (size/(2^0.5))
  
  test_points[14,1] <- test_points[14,1] - (size/(2^0.5))
  test_points[14,3] <- test_points[14,3] - (size/(2^0.5))
  
  ## Alpha and Beta
  test_points[15,2] <- test_points[15,2] + (size/(2^0.5))
  test_points[15,3] <- test_points[15,3] + (size/(2^0.5))
  
  test_points[16,2] <- test_points[16,2] - (size/(2^0.5))
  test_points[16,3] <- test_points[16,3] + (size/(2^0.5))
  
  test_points[17,2] <- test_points[17,2] + (size/(2^0.5))
  test_points[17,3] <- test_points[17,3] - (size/(2^0.5))
  
  test_points[18,2] <- test_points[18,2] - (size/(2^0.5))
  test_points[18,3] <- test_points[18,3] - (size/(2^0.5))
  
  ## change the points - triple
  test_points[19,1] <- test_points[19,1] + (size/(3^0.5))
  test_points[19,2] <- test_points[19,2] + (size/(3^0.5))
  test_points[19,3] <- test_points[19,3] + (size/(3^0.5))
  
  test_points[20,1] <- test_points[20,1] - (size/(3^0.5))
  test_points[20,2] <- test_points[20,2] + (size/(3^0.5))
  test_points[20,3] <- test_points[20,3] + (size/(3^0.5))
  
  test_points[21,1] <- test_points[21,1] + (size/(3^0.5))
  test_points[21,2] <- test_points[21,2] - (size/(3^0.5))
  test_points[21,3] <- test_points[21,3] + (size/(3^0.5))
  
  test_points[22,1] <- test_points[22,1] + (size/(3^0.5))
  test_points[22,2] <- test_points[22,2] + (size/(3^0.5))
  test_points[22,3] <- test_points[22,3] - (size/(3^0.5))
  
  test_points[23,1] <- test_points[23,1] - (size/(3^0.5))
  test_points[23,2] <- test_points[23,2] - (size/(3^0.5))
  test_points[23,3] <- test_points[23,3] + (size/(3^0.5))
  
  test_points[24,1] <- test_points[24,1] - (size/(3^0.5))
  test_points[24,2] <- test_points[24,2] + (size/(3^0.5))
  test_points[24,3] <- test_points[24,3] - (size/(3^0.5))
  
  test_points[25,1] <- test_points[25,1] + (size/(3^0.5))
  test_points[25,2] <- test_points[25,2] - (size/(3^0.5))
  test_points[25,3] <- test_points[25,3] - (size/(3^0.5))
  
  test_points[26,1] <- test_points[26,1] - (size/(3^0.5))
  test_points[26,2] <- test_points[26,2] - (size/(3^0.5))
  test_points[26,3] <- test_points[26,3] - (size/(3^0.5))
  
  ## return absolute
  test_points[,1] <- abs(test_points[,1])
  test_points[,2] <- abs(test_points[,2])
  test_points[,3] <- abs(test_points[,3])
  
  ## produce Gamma
  test_points[,4] <- 1- test_points[,2] - test_points[,3]
  
  ## ensure points are valid
  for(i in 1:nrow(test_points)){
    
    ## get rid of negative gammas
    test_points[i,2] <- max(test_points[i,2] - (abs(min(2*test_points[i,4],0))*(test_points[i,2]/(test_points[i,2]+test_points[i,3]))),min_gamma)
    test_points[i,3] <- max(test_points[i,3] - (abs(min(2*test_points[i,4],0))*(test_points[i,3]/(test_points[i,2]+test_points[i,3]))),min_gamma)
    
    test_points[i,4] <- max(test_points[i,4], abs(test_points[i,4]))
    
  }
  
  ## check sums
  test_points[,5] <- test_points[,2]+ test_points[,3]+test_points[,4]
  ## get rid of combinations that are too small
  test_points[,4] <- test_points[,4]+ 1 - test_points[,5]
  
  ## check sum new
  test_points[,5] <- test_points[,2]+ test_points[,3]+test_points[,4]
  
  ## return answer
  return(test_points)
  
}

## Function to calculate MSE

test_df <- vector_fun(start_point = start_test,size = 0.4)

MSE_fun <- function(test_points, sector_no){
  
  ## what data are we testing
  temp_data <- data_org_list[[sectors_need[sector_no]]]
  ## how many data points
  temp_years <- nrow(temp_data)
  ## data frame to calculate MSE
  temp_test <-data.frame(matrix(nrow = nrow(test_points),ncol = temp_years +1,data = 0))
  for(j in 1:temp_years){
    
    temp_test[,j] <- (temp_data[j,"Q_hat"]-(test_points[,"A"] + test_points[,"Alpha"]*temp_data[j,"G_hat"]+test_points[,"Beta"]*temp_data[j,"L_hat"]))^2
    
  }
  for(i in 1:nrow(test_points)){
    
    temp_test[i,temp_years+1] <- sum(temp_test[i,1:temp_years])
    
  }
  
  temp_min <-  min(temp_test[,temp_years+1])
  temp_min_row <- match(x = temp_min, table = temp_test[,temp_years+1])
  
  temp_start <- test_points[temp_min_row,1:3] %>% 
    mutate(MSE = temp_min) %>% 
    mutate(direction = temp_min_row)
  return(temp_start)
  
}

MSE_fun(test_points = test_df, sector_no = 1)



## search setup data
## steps, first, last size


## remove 40 sector
sectors_need_2 <- sectors_need[-length(sectors_need)]

steps_fun <- function(steps_no){
  
  search_steps <- data.frame("step" =1:steps_no[1],
                             "size" = rep(0))
  
  search_steps[,2] <- (((steps_no[3]-steps_no[2])/steps_no[1])*(search_steps[,1]-1))+steps_no[2]
  
  return(search_steps)
  
}
## steps <- vector[3]
## start <- vector[3]
## the_big_list <- is a [2] nested list 
## temp_sectors <- sectors_need


mega_function <- function(steps,start_loc,the_big_list, temp_sectors){
  
  start_time <- Sys.time()
  
  for(q in 1:length(temp_sectors)){
    
    temp_data_df <- data.frame("A" = rep(0,steps[1]),
                               "Alpha" = rep(0),
                               "Beta" = rep(0),
                               "MSE" = rep(0),
                               "direction" = rep(0))
    
    temp_step_size <- steps_fun(steps_no = steps)
    
    temp_start <- start_loc
    
    for(k in 1:steps[1]){
      
      
      temp_search_df <- vector_fun(start_point = temp_start,size = temp_step_size[k,2])
      
      temp_results <- MSE_fun(test_points = temp_search_df,sector_no = q)
      
      
      temp_data_df[k,1] <- temp_results[[1,1]]
      temp_data_df[k,2] <- temp_results[[1,2]]
      temp_data_df[k,3] <- temp_results[[1,3]]
      temp_data_df[k,4] <- temp_results[[1,4]]
      temp_data_df[k,5] <- temp_results[[1,5]]
      
      temp_start <- temp_results[1:3]
      
      
    }
    
    the_big_list[[q]] <- temp_data_df
    
  }
  
  end_time <- Sys.time()
  
  run_time <- end_time - start_time
  
  print(run_time)
  
  return(the_big_list)
}


## 0.4 Run Function Run ---------------

## Setup
## Search parameters
search_steps_no <- c(4000,0.5,0.01)

## Start location
start_loc <- c(0.9,0.333,0.333)

## set up lists for lists
big_list <- list()


test_small <- mega_function(steps =search_steps_no, 
                            start_loc = start_loc,
                            the_big_list =big_list,
                            temp_sectors = sectors_need_2
)

## bring final results into single data frame

results_final_df<- data.frame(matrix(nrow=length(sectors_need_2), ncol= ncol(test_small[[1]])+1))
colnames(results_final_df) <-c("Sector",colnames(test_small[[1]]))

for(i in 1: nrow(results_final_df)){
  
  results_final_df[i,1] <- sectors[sectors_need_2[i]]
  
  for(j in 1:(ncol(results_final_df)-1)){
    
    results_final_df[i,j+1] <- test_small[[i]][nrow(test_small[[i]]),j]
    
  }
  
}

write.csv(x = results_final_df, file = "cobbs_doug_found.csv")
