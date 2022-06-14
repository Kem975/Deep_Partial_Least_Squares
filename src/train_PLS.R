# Function to calculate the PLS models.
# 1) Fit a PLS model and perform cross-validation to obtain the number of components 
# 2) Predict Y_hat with this number of components

# Input: t_0, T, rescaled Data
# Output: Y_hat

train_PLS <- function(t_0, T, Xs_train, Xs_test){
  Y_hat_is_PLS <- list()
  Y_hat_oos_PLS <- list()
  comp_num_list_PLS <- c()
  
  for (i in 1:T){
    print(sprintf("%i / %i", i, T))
    validation <- Xs_train[[t_0+i-1]]
    train <- Xs_train[[t_0+i]]
    test <- Xs_test[[t_0+i]]
    
    fit <- plsr(formula = xs_return~., data=train, rescale = F, validation="CV")
    
    comp.num_CV <- optimal_K_PLS(validation, fit)
   
    if (comp.num_CV<2){
      comp.num_CV <- 1
    }
    
    comp_num_list_PLS[i] <- comp.num_CV
    
    Y_hat_is <- predict(fit, ncomp = comp.num_CV, newdata = train)[,1,]
    Y_hat_oos <- predict(fit, ncomp = comp.num_CV, newdata = test)[,1,]
    
    Y_hat_is_PLS[[i]] <- as.numeric(Y_hat_is)
    Y_hat_oos_PLS[[i]] <- as.numeric(Y_hat_oos)
  }
  
  return(list(Y_hat_is_PLS, Y_hat_oos_PLS, comp_num_list_PLS))
}



optimal_K_PLS <- function(validation, fit){
  MSE <- rep(0,10)
  for (k in 1:10){
    Y_hat <- predict(fit, ncomp = k, newdata = validation)[,1,]
    Y<-validation[,50]
    MSE[k] <- mean((Y_hat-Y)^2)
  }
  K_star <- which.min(MSE)
  return (which.min(MSE))
}
