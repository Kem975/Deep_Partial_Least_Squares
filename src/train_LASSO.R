# Function to calculate the LASSO models.
# 1) Perform cross-validation for the parameter lambda
# 2) Fit the LASSO model
# 3) Predict to obtain Y_hat

# Input: t_0, T, rescaled Data
# Output: Y_hat

train_LASSO <- function(t_0, T, Xs_train, Xs_test){
  Yhat_LASSO_oos_list <- list()
  Yhat_LASSO_is_list <- list()
  
  for (i in 1:T){
    print(i)
    train <- Xs_train[[t_0+i]]
    test <- Xs_test[[t_0+i]]
    
    x_train <-  as.matrix(subset(train, select = -c(xs_return))) 
    y_train <- train$xs_return
    
    x_test <-  as.matrix(subset(test, select = -c(xs_return))) 
    y_test <- test$xs_return
    
    lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, 
                           standardize = TRUE, nfolds=5)
    
    # Best 
    lambda_best <- lasso_reg$lambda.min 
    
    fit.lasso <-  glmnet(x_train, y_train, alpha = 1, lambda = lambda_best)
    
    yhat_is <- predict(fit.lasso, newx = x_train,  s = lambda_best)
    yhat_oos <- predict(fit.lasso, newx = x_test,  s = lambda_best)
    
    # Save Values
    Yhat_LASSO_oos_list[[i]] <- yhat_oos
    Yhat_LASSO_is_list[[i]] <- yhat_is
  }
  
  return(list(Yhat_LASSO_oos_list, Yhat_LASSO_is_list))
  
}