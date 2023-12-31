lm_list[[1]] <- lm(formula = X1_change ~ X1 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[1]])
lm_list[[2]] <- lm(formula = X2_change ~ X2 + X1 +  X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[2]])
lm_list[[3]] <- lm(formula = X3_change ~ X3 + X2 + X1 +  X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[3]])
lm_list[[4]] <- lm(formula = X4_change ~ X4 + X2 + X3 + X1 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[4]])
lm_list[[5]] <- lm(formula = X5_change ~ X5 + X2 + X3 + X4 + X1 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[5]])
lm_list[[6]] <- lm(formula = X6_change ~ X6 + X2 + X3 + X4 + X5 + X1 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[6]])
lm_list[[7]] <- lm(formula = X7.1_change ~ X7.1 + X2 + X3 + X4 + X5 + X6 +X1 + X7_other + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[7]])
lm_list[[8]] <- lm(formula = X7_other_change ~ X7_other + X2 + X3 + X4 + X5 + X6 +X7.1 + X1 + X8 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[8]])
lm_list[[9]] <- lm(formula = X8_change ~ X8 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X1 + X9 + X10 +X11 +X12 + 0, data = price_data_all[[9]])
lm_list[[10]] <- lm(formula = X9_change ~ X9 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X1 + X10 +X11 +X12 + 0, data = price_data_all[[10]])
lm_list[[11]] <- lm(formula = X10_change ~ X10 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X1 +X11 +X12 + 0, data = price_data_all[[11]])
lm_list[[12]] <- lm(formula = X11_change ~ X11 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X1 +X12 + 0, data = price_data_all[[12]])
lm_list[[13]] <- lm(formula = X12_change ~ X12 + X2 + X3 + X4 + X5 + X6 +X7.1 + X7_other + X8 + X9 + X10 +X11 +X1 + 0, data = price_data_all[[13]])

