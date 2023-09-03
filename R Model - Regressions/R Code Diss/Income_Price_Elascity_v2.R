## Price_Elascity Code v1

## 0.0 Package -----------

library(dplyr)
library(Matrix)
library(glmnet)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Matrix)
library(glmnet)

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
                     "Transport - Automotive Vehicles",
                     "Transport - Other",
                     "Communication",
                     "Recreation & culture",
                     "Education",
                     "Restaurants & hotels",
                     "Miscellaneous goods & services")

good_one_trans <- good_types_name[-7]

colnames(sh_df)

## 2.0 Income Elasticity -------------

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

## List for the regression data

## regression Lists
income_reg_0 <- list()
income_reg_all <- list()
price_reg <- list()
income_price_reg <- list()
income_log_reg <- list()
income_log_price_reg <- list()
income_log_price_log_reg <- list()


## Test Lists
anova_list_1 <- list()
anova_list_2 <- list()
anova_list_3 <- list()
anova_list_4 <- list()
anova_list_5 <- list()

## Regression Results Data Frame
income_reg_0_df <- data.frame( "good" = good_types,
                               "coef_1" =rep(0,length(good_types)),
                               "coef_1_pr" = rep(0),
                               "model_r2" = rep(0),
                               "model_ad_r2" = rep(0))

income_reg_all_df <- data.frame( "good" = good_types,
                                 "int" = rep(0,length(good_types)),
                                 "int_pr" = rep(0),
                                 "coef_1" = rep(0),
                                 "coef_1_pr" = rep(0),
                                 "model_r2" = rep(0),
                                 "model_ad_r2" = rep(0))

price_reg_df <- data.frame( "good" = good_types,
                            "int" = rep(0,length(good_types)),
                            "int_pr" = rep(0),
                            "coef_1" = rep(0),
                            "coef_1_pr" = rep(0),
                            "model_r2" = rep(0),
                            "model_ad_r2" = rep(0))

income_price_reg_df <- data.frame( "good" = good_types,
                                   "int" = rep(0,length(good_types)),
                                   "int_pr" = rep(0),
                                   "coef_1" = rep(0),
                                   "coef_1_pr" = rep(0),
                                   "coef_2" = rep(0),
                                   "coef_2_pr" = rep(0),
                                   "model_r2" = rep(0),
                                   "model_ad_r2" = rep(0))

income_log_reg_df <- data.frame( "good" = good_types,
                                 "int" = rep(0,length(good_types)),
                                 "int_pr" = rep(0),
                                 "coef_1" = rep(0),
                                 "coef_1_pr" = rep(0),
                                 "model_r2" = rep(0),
                                 "model_ad_r2" = rep(0))

income_log_price_reg_df <- data.frame( "good" = good_types,
                                       "int" = rep(0,length(good_types)),
                                       "int_pr" = rep(0),
                                       "coef_1" = rep(0),
                                       "coef_1_pr" = rep(0),
                                       "coef_2" = rep(0),
                                       "coef_2_pr" = rep(0),
                                       "model_r2" = rep(0),
                                       "model_ad_r2" = rep(0))

income_log_price_log_reg_df <- data.frame( "good" = good_types,
                                           "int" = rep(0,length(good_types)),
                                           "int_pr" = rep(0),
                                           "coef_1" = rep(0),
                                           "coef_1_pr" = rep(0),
                                           "coef_2" = rep(0),
                                           "coef_2_pr" = rep(0),
                                           "model_r2" = rep(0),
                                           "model_ad_r2" = rep(0))


## Data frame for regression results and comparison results

anova_pr_df <-data.frame( "good" = good_types,
                          "anova_pr_1" =rep(0,length(good_types)),
                          "anova_pr_2" =rep(0),
                          "anova_pr_3" =rep(0),
                          "anova_pr_4" = rep(0),
                          "anova_pr_5" = rep(0)
)

for(i in 1:length(good_types)){
  
  ## 2.1 run regression ----------------
  income_reg_0[[i]] <- lm(Quantity ~ 0 +Income.Amount, data = split_list[[i]])
  income_reg_all[[i]] <- lm(Quantity ~ Income.Amount, data = split_list[[i]])
  price_reg[[i]] <- lm(Quantity ~  Price, data = split_list[[i]])
  income_price_reg[[i]] <- lm(Quantity ~ Income.Amount + Price, data = split_list[[i]])
  income_log_reg[[i]] <- lm(Quantity ~ log_income, data = split_list[[i]])
  income_log_price_reg[[i]] <- lm(Quantity ~ log_income + Price, data = split_list[[i]])
  income_log_price_log_reg[[i]] <- lm(Quantity ~ log_income + log_price, data = split_list[[i]])
  
  
  
  ## run Anova on intercept Model
  anova_list_1[[i]] <- anova(income_reg_all[[i]],income_reg_0[[i]])
  
  ## run Anova on two variable Model
  anova_list_2[[i]] <- anova(income_reg_0[[i]],income_price_reg[[i]])
  
  ## run Anova on log model 
  anova_list_3[[i]] <- anova(income_reg_0[[i]],income_log_reg[[i]])
  
  ## run Anova on the two two variable Model
  anova_list_4[[i]] <- anova(income_reg_0[[i]],income_log_price_log_reg[[i]])
  
  ## run Anova on the two two variable Model
  anova_list_5[[i]] <- anova(income_price_reg[[i]],income_log_price_log_reg[[i]])
  
  
  
  ## 2.2 Input results in data frame -------------
  ## Anova
  anova_pr_df[i,2] <- anova_list_1[[i]][2,6]
  anova_pr_df[i,3] <- anova_list_2[[i]][2,6]
  anova_pr_df[i,4] <- anova_list_3[[i]][2,6]
  anova_pr_df[i,5] <- anova_list_4[[i]][2,6]
  anova_pr_df[i,6] <- anova_list_5[[i]][2,6]
  
  
  ## income_reg_0
  income_reg_0_df[i,2] <-  summary(income_reg_0[[i]])[[4]][1]
  income_reg_0_df[i,3] <-  summary(income_reg_0[[i]])[[4]][4]
  income_reg_0_df[i,4] <-  summary(income_reg_0[[i]])[[8]]
  income_reg_0_df[i,5] <-  summary(income_reg_0[[i]])[[9]]
  
  ## income_reg_all_df
  income_reg_all_df[i,2] <- summary(income_reg_all[[i]])[[4]][1,1]
  income_reg_all_df[i,3] <- summary(income_reg_all[[i]])[[4]][1,4]
  income_reg_all_df[i,4] <- summary(income_reg_all[[i]])[[4]][2,1]
  income_reg_all_df[i,5] <- summary(income_reg_all[[i]])[[4]][2,4]
  income_reg_all_df[i,6] <- summary(income_reg_all[[i]])[[8]]
  income_reg_all_df[i,7] <- summary(income_reg_all[[i]])[[9]]
  
  ## price_reg_df
  price_reg_df[i,2] <- summary(price_reg[[i]])[[4]][1,1]
  price_reg_df[i,3] <- summary(price_reg[[i]])[[4]][1,4]
  price_reg_df[i,4] <- summary(price_reg[[i]])[[4]][2,1]
  price_reg_df[i,5] <- summary(price_reg[[i]])[[4]][2,4]
  price_reg_df[i,6] <- summary(price_reg[[i]])[[8]]
  price_reg_df[i,7] <- summary(price_reg[[i]])[[9]]
  
  ## income_price_reg_df
  income_price_reg_df[i,2] <- summary(income_price_reg[[i]])[[4]][1,1]
  income_price_reg_df[i,3] <- summary(income_price_reg[[i]])[[4]][1,4]
  income_price_reg_df[i,4] <- summary(income_price_reg[[i]])[[4]][2,1]
  income_price_reg_df[i,5] <- summary(income_price_reg[[i]])[[4]][2,4]
  income_price_reg_df[i,6] <- summary(income_price_reg[[i]])[[4]][3,1]
  income_price_reg_df[i,7] <- summary(income_price_reg[[i]])[[4]][3,4]
  income_price_reg_df[i,8] <- summary(income_price_reg[[i]])[[8]]
  income_price_reg_df[i,9] <- summary(income_price_reg[[i]])[[9]]
  
  ## income_log_reg_df
  income_log_reg_df[i,2] <- summary(income_log_reg[[i]])[[4]][1,1]
  income_log_reg_df[i,3] <- summary(income_log_reg[[i]])[[4]][1,4]
  income_log_reg_df[i,4] <- summary(income_log_reg[[i]])[[4]][2,1]
  income_log_reg_df[i,5] <- summary(income_log_reg[[i]])[[4]][2,4]
  income_log_reg_df[i,6] <- summary(income_log_reg[[i]])[[8]]
  income_log_reg_df[i,7] <- summary(income_log_reg[[i]])[[9]]
  
  ## income_price_reg_df
  income_log_price_log_reg_df[i,2] <- summary(income_log_price_log_reg[[i]])[[4]][1,1]
  income_log_price_log_reg_df[i,3] <- summary(income_log_price_log_reg[[i]])[[4]][1,4]
  income_log_price_log_reg_df[i,4] <- summary(income_log_price_log_reg[[i]])[[4]][2,1]
  income_log_price_log_reg_df[i,5] <- summary(income_log_price_log_reg[[i]])[[4]][2,4]
  income_log_price_log_reg_df[i,6] <- summary(income_log_price_log_reg[[i]])[[4]][3,1]
  income_log_price_log_reg_df[i,7] <- summary(income_log_price_log_reg[[i]])[[4]][3,4]
  income_log_price_log_reg_df[i,8] <- summary(income_log_price_log_reg[[i]])[[8]]
  income_log_price_log_reg_df[i,9] <- summary(income_log_price_log_reg[[i]])[[9]]  
  
  
}


## 3.0 Create Plots -----------

scatter_plots_line <- list()
scatter_plots_log <- list()
scatter_plots_price <- list()
scatter_plots_income <- list()


## 3.1 Linear Plots Income vs Spend ------------

for(i in 1:length(good_types)){
  
  scatter_plots_log[[i]] <- ggplot(split_list[[i]], aes(x=Income.Amount, y = Quantity))+
    geom_point(color = "darkblue") +
    theme_economist_white() + 
    scale_colour_economist()+
    xlab("Total Monthly Spend (2008 Prices)") +
    ylab("Quantity")+
    ggtitle(good_types_name[i])+
    geom_smooth(method=lm,se=FALSE, formula = "y~log(x)",linetype="dashed",color="#581845")+
    theme(axis.title.y =  element_text(margin = margin(r = 10)))+
    theme(axis.title.x = element_text(margin = margin(t = 10)))
  
  #geom_smooth(method=lm,se=FALSE, formula = "y~x",linetype="dashed",color="#c70039")
  
}

## 3.2 Log Plots Income vs Spend ------------

for(i in 1:length(good_types)){
  
  scatter_plots_line[[i]] <- ggplot(split_list[[i]], aes(x=Income.Amount, y = Quantity))+
    geom_point(color = "darkblue") +
    theme_economist_white() + 
    scale_colour_economist()+
    xlab("Total Monthly Spend (2008 Prices)") +
    ylab("Quantity")+
    ggtitle(good_types_name[i])+
    geom_smooth(method=lm,se=FALSE, formula = "y~x",linetype="dashed",color="#c70039")+
    theme(axis.title.y =  element_text(margin = margin(r = 10)))+
    theme(axis.title.x = element_text(margin = margin(t = 10)))
  
  #geom_smooth(method=lm,se=FALSE, formula = "y~log(x)",linetype="dashed",color="#581845")+
}

## 3.3 Plots Price vs Spend ------------

for(i in 1:length(good_types)){
  
  scatter_plots_price[[i]] <- ggplot(split_list[[i]], aes(x=Price, y = Quantity, color = Income.Desc))+
    geom_point()+
    # geom_point(color = "darkblue") +
    theme_economist_white() + 
    #scale_colour_economist()+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    xlab("Index Price for 2008-2019 (2015 = 100)") +
    ylab("Quantity")+
    ggtitle(good_types_name[i])+
    #geom_smooth(method=lm,se=FALSE, formula = "y~x",linetype="dashed",color="#c70039")+
    theme(axis.title.y =  element_text(margin = margin(r = 10)))+
    theme(axis.title.x = element_text(margin = margin(t = 10))) +
    guides(color=guide_legend(title="Income Level Description"))
  
  #geom_smooth(method=lm,se=FALSE, formula = "y~log(x)",linetype="dashed",color="#581845")+
}

## 3.4 Plots Income vs Spend ------------

for(i in 1:length(good_types)){
  
  scatter_plots_income[[i]] <- ggplot(split_list[[i]], aes(x=Income.Amount, y = Quantity))+
    geom_point(color = "darkblue") +
    theme_economist_white() + 
    scale_colour_economist()+
    xlab("Total Monthly Spend (2008 Prices)") +
    ylab("Quantity")+
    ggtitle(good_types_name[i])+
    #geom_smooth(method=lm,se=FALSE, formula = "y~log(x)",linetype="dashed",color="#581845")+
    theme(axis.title.y =  element_text(margin = margin(r = 10)))+
    theme(axis.title.x = element_text(margin = margin(t = 10)))
  
  #geom_smooth(method=lm,se=FALSE, formula = "y~x",linetype="dashed",color="#c70039")
  
}

scatter_plots_income[[10]]

## plots 
scatter_plots_log[[1]]
scatter_plots_line[[10]]
scatter_plots_price[[1]]
anova_list_1[[1]]


## 4.0 LASSO Regression -----------

## 4.1 Data Prep -----

y_list <- list()
x_list <- list()

for(i in 1:length(good_types)){
  
  y_list[[i]] <- multi_list[[i]]$Quantity
  
  x_list[[i]] <- data.matrix(multi_list[[i]][,c("Income.Amount",
                                                "Price_1",
                                                "Price_2",
                                                "Price_3",
                                                "Price_4",
                                                "Price_5",
                                                "Price_6",
                                                "Price_7.1",
                                                "Price_7_other",
                                                "Price_8",
                                                "Price_9",      
                                                "Price_10",
                                                "Price_11",
                                                "Price_12")
  ])
  
}


#perform k-fold cross-validation to find optimal lambda value

cv_model_lasso <- list()
best_model_lasso <- list()
cv_df_lasso <-data.frame( "good" = good_types,
                          "min.lambda" =rep(0,length(good_types)))

cv_model_ridge <- list()
best_model_ridge <- list()
cv_df_ridge <-data.frame( "good" = good_types,
                          "min.lambda" =rep(0,length(good_types)))

coef_lasso <- data.frame(matrix(ncol = length(good_types), nrow = 15))
coef_ridge <- data.frame(matrix(ncol = length(good_types), nrow = 15))


for(i in 1:length(good_types)){
  
  cv_model_lasso[[i]] <- cv.glmnet(x_list[[i]], y_list[[i]], alpha = 1)
  
  cv_df_lasso[i,2] <- cv_model_lasso[[i]]$lambda.min
  
  best_model_lasso[[i]] <- glmnet(x_list[[i]], y_list[[i]], alpha = 1, lambda = cv_df_lasso[i,2])
  
  cv_model_ridge[[i]] <- cv.glmnet(x_list[[i]], y_list[[i]], alpha = 0)
  
  cv_df_ridge[i,2] <- cv_model_ridge[[i]]$lambda.min
  
  best_model_ridge[[i]] <- glmnet(x_list[[i]], y_list[[i]], alpha = 1, lambda = cv_df_ridge[i,2])
  
  coef_lasso[1:14,i] <- coef(best_model_lasso[[i]])[1:14]
  coef_ridge[1:14,i] <- coef(best_model_ridge[[i]])[1:14]
  
  coef_lasso[15,i] <- best_model_lasso[[1]]$dev.ratio
  coef_ridge[15,i] <- best_model_ridge[[1]]$dev.ratio
  
}


plot(best_model_lasso[[1]],xvar = "lambda",label = T)

plot(cv_model_lasso[[i]])

## 5.0 Save Outputs ----------

## 5.1 Remove transport -----------
anova_pr_df <- anova_pr_df %>% 
  filter(good!=7)

income_reg_0_df <- income_reg_0_df %>% 
  filter(good!=7)

income_reg_all_df <- income_reg_all_df %>% 
  filter(good!=7)

price_reg_df <- price_reg_df %>% 
  filter(good!=7)

income_price_reg_df <- income_price_reg_df %>% 
  filter(good!=7)

income_log_reg_df <- income_log_reg_df %>% 
  filter(good!=7)

income_log_price_log_reg_df <- income_log_price_log_reg_df %>% 
  filter(good!=7)

## 5.2 Coefs -------------
## ML
# write.csv(x = coef_lasso,file = "Output_3/coef_LASSO.csv")
# write.csv(x = coef_ridge,file = "Output_3/coef_Ridge.csv")
# ## Linear
# write.csv(x = anova_pr_df,file = "Output_3/Linear Coefs/anova.csv")
# write.csv(x = income_reg_0_df,file = "Output_3/Linear Coefs/income_reg_0_df.csv")
# write.csv(x = income_reg_all_df,file = "Output_3/Linear Coefs/income_reg_all_df.csv")
# write.csv(x = price_reg_df,file = "Output_3/Linear Coefs/price_reg_df.csv")
# write.csv(x = income_price_reg_df,file = "Output_3/Linear Coefs/income_price_reg_df.csv")
# write.csv(x = income_log_reg_df,file = "Output_3/Linear Coefs/income_log_reg_df.csv")
# write.csv(x = income_log_price_log_reg_df,file = "Output_3/Linear Coefs/income_log_price_log_reg_df.csv")



## Charts

for(i in 1:length(good_types)){
  
  ggplot2::ggsave(filename = paste0("Output_3/Plots Linear Income/plot_lin_",i,".png"),plot = scatter_plots_line[[i]], width = 6.17, height =6 )
  ggplot2::ggsave(filename = paste0("Output_3/Plots Log Income/plot_lin_",i,".png"),plot = scatter_plots_log[[i]],width = 6.17, height =6)
  ggplot2::ggsave(filename = paste0("Output_3/Plots Price/plot_lin_",i,".png"),plot = scatter_plots_price[[i]],width = 6.17, height =6)
  ggplot2::ggsave(filename = paste0("Output_3/Plots No Regression Lines/plot_income_",i,".png"),plot = scatter_plots_income[[i]],width = 6.17, height =6)

  }





