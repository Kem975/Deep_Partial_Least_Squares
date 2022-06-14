# Function to calculate the DPLS models.
# 1) Perform PLS with a given (generally high) number of components. e.g 40 components on the saved models. Max is 49 with our dataset
# 2) Feed the X-score to a Neural Network to predict the Y-score
# 3) Perform Cross-validation on the number of components to use
# 4) Average the U_hat (predicted Y-score) columns over the cross-validated number of components to obtain the Y_hat

# Input: t_0, T, rescaled Data, bRetrain to train a new network, bSensitivities to calculate the sensitivities -> if false 0 arrays are returned
# Output: U_hat (Y-score predicted), Y_hat and list of number of components used

load_DPLS <- function(t_0, T, Xs_train, Xs_test, dates, bRetrain=FALSE, bSensitivities=FALSE){
  betas_matrix <- array(0, c(T,49))
  betas_interact_matrix <- array(0, c(T, 49, 49))
  
  Uhat_oos_list <- list()
  Uhat_is_list <- list()
  
  Y_hat_oos_list <- list()
  Y_hat_is_list <- list()
  
  comp_num_list <- c()
  comp_input_list <- c()
  
  df.attribution_DPLS <- data.frame()
  return_attribution_DPLS <- list()
  return_attribution_predicted_DPLS <- list()
  
  for (i in 1:T){
    print(sprintf("%i / %i", i, T))
    validation = Xs_train[[t_0+i-1]]
    train <- Xs_train[[t_0+i]]
    test <- Xs_test[[t_0+i]]
    
    #Used for the attribution plot
    x_train_original <- as.matrix(subset(train, select = -c(xs_return))) 
    x_test_original <- as.matrix(subset(test, select = -c(xs_return))) 
    
    ncomp = 40
    
    plsr.input <- plsr(formula = xs_return~., ncomp=ncomp, data=train, rescale = F)
    PLS_Score.input <- scores(plsr.input)
    
    x_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$loading.weights)
    y_train <- train$xs_return
    
    x_validation <- as.matrix(validation[,1:49]) %*% drop(plsr.input$loading.weights)
    y_validation <- validation$xs_return
    
    x_test <- as.matrix(test[,1:49]) %*% drop(plsr.input$loading.weights) 
    y_test <- test$xs_return
    
    U_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$coefficients)
    
    # If the user wants to train and use a new model
    if(bRetrain==TRUE){
      model.PLS <- keras_model_sequential() %>%
        layer_dense(units = 100, activation = "softplus", input_shape = ncomp, kernel_initializer='normal') %>%
        layer_dropout(rate = 0.5) %>%
        layer_dense(units = ncomp, kernel_initializer='normal') 
      #summary(model.PLS)
      
      model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")
      model.PLS %>% fit( x = x_train, y = U_train, verbose = 0, 
                         epochs=100,
                         callbacks = list(callback_early_stopping(monitor = "loss", 
                                                                  patience = 30, 
                                                                  min_delta = 1e-6, restore_best_weights = TRUE)))
    } else{
      model.PLS <- load_model_tf(sprintf("../data/Models_DPLS/%s.h5", dates[t_0+i]))
    }
    
    U_hat_is = model.PLS %>% predict( x_train )
    U_hat_validation = model.PLS %>% predict( x_validation )
    U_hat_oos = model.PLS %>% predict( x_test )
    
    
    comp.num_CV <- optimal_K(U_hat_validation, y_validation, 40)
    comp_num_list[i] <- comp.num_CV #Save the number of component used
    
    
    # Calculate Y_hat with the number of components chose by cross-validation
    if (comp.num_CV < 2){
      Y_hat_oos <- U_hat_oos[,1]
      Y_hat_is <- U_hat_is[,1]
    } else{
      Y_hat_oos <- rowMeans(U_hat_oos[,1:comp.num_CV])
      Y_hat_is <- rowMeans(U_hat_is[,1:comp.num_CV])
    }
    
    # Save the values
    Uhat_oos_list[[i]] <- U_hat_oos
    Uhat_is_list[[i]] <- U_hat_is
    
    Y_hat_oos_list[[i]] <- Y_hat_oos
    Y_hat_is_list[[i]] <- Y_hat_is
    
    if (bSensitivities == TRUE){
      list.sensitivities <- sensitivities_DPLS(model.PLS, x_train, plsr.input$loading.weights, ncomp, comp.num_CV)
      betas_matrix[i,] <- list.sensitivities[[1]]
      betas_interact_matrix[i,,] <- list.sensitivities[[2]]
    }
    
  }
  return(list(Uhat_oos_list, Uhat_is_list, Y_hat_oos_list, Y_hat_is_list, betas_matrix, betas_interact_matrix, comp_num_list))
}



# Calculate the Sensitivity and Interaction at x=0
sensitivities_DPLS <- function(model, x_train, loading.weights, max_comp_num, CV_comp_num){
  x <- tf$zeros(shape(1,dim(x_train)[2]))
  
  with(tf$GradientTape() %as% t2, {
    with(tf$GradientTape() %as% t1, {
      t1$watch(x)
      t2$watch(x)
      y <- model(x)
    })
    dy_tf <- t1$jacobian(y, x)
  })
  d2y_tf <- t2$jacobian(dy_tf, x)
  
  d2y <- drop(as.array(d2y_tf))
  dy <- drop(as.array(dy_tf))
  
  Q <- rep(0,max_comp_num)
  Q[1:CV_comp_num] <- 1/CV_comp_num
  
  J_0 <- t(dy %*% t(loading.weights)) %*% Q
  
  H_0 <- matrix(0,49,49)
  for (jj in 1:49){
    for (ii in 1:49){
      for (kk in 1:CV_comp_num){
        H_0[ii,jj] <- H_0[ii,jj] + t(d2y[kk,,] %*% loading.weights[jj,]) %*% (loading.weights[ii,]) * Q[kk]
      }  
    }
  }
  return(list(J_0, H_0))
}



optimal_K <- function(U_hat_oos, y_test, max_k){
  MSEs <- rep(0,max_k)
  MSEs[1] <- mean((U_hat_oos[,1] - y_test)^2)
  for (i in 2:max_k){
    MSEs[i] <- mean((rowMeans(U_hat_oos[,1:i]) - y_test)^2)
  }
  return (which.min(MSEs))
}




