# Function to calculate the NN models.
# 1) Feed the x_train data to the Neural Network 
# 2) Get the Y_hat by predicting on the NN

# Input: t_0, T, rescaled Data
# Output: Y_hat

load_NN <- function(t_0, T, Xs_train, Xs_test, dates, bRetrain=FALSE, bSensitivities=FALSE){
  nfactor <- 49
  betas_matrix <- array(0, c(T,nfactor))
  betas_interact_matrix <- array(0, c(T, nfactor, nfactor))
  
  Y_hat_is_list_NN <- list()
  Y_hat_oos_list_NN <- list()
  
  r2.vect_is_normal <- c()
  r2.vect_oos_normal <- c()
  
  mae.vect_is_normal <- c()
  mae.vect_oos_normal <- c()
  
  for (i in 1:T){
    print(sprintf("%i / %i", i, T))
    
    train <- Xs_train[[t_0+i]]
    test <- Xs_test[[t_0+i]]
    
    # Get the training and testing data
    x_train <-  as.matrix(subset(train, select = -c(xs_return))) 
    y_train <- train$xs_return
    
    x_test <-  as.matrix(subset(test, select = -c(xs_return))) 
    y_test <- test$xs_return
    
    
    # If the user wants to train and use a new model
    if(bRetrain==TRUE){
      model <- keras_model_sequential() %>%
        layer_dense(units = 100, activation = "softplus", input_shape = nfactor, kernel_initializer='normal') %>%
        layer_dropout(rate = 0.0) %>%
        layer_dense(units = 1, kernel_initializer='normal') 
      
      model %>% compile(loss = "mse", optimizer = "RMSprop")
      model %>% fit( x = x_train, y = y_train, verbose = 0, 
                     epochs=100,
                     callbacks = list(callback_early_stopping(monitor = "loss", 
                                                              patience = 30, 
                                                              min_delta = 1e-4))) #restore_best_weights = TRUE
    } else{
      model <- load_model_tf(sprintf("../data/Models_NN_Softplus/%s.h5", dates[t_0+i]))
    }
    
    # Test
    yhat_is = model %>% predict( x_train )
    yhat_oos = model %>% predict( x_test )
    
    # Save
    Y_hat_is_list_NN[[i]] <- yhat_is
    Y_hat_oos_list_NN[[i]] <- yhat_oos
    
    if (bSensitivities == TRUE){
      list.sensitivities <- sensitivities_NN(model, x_train)
      betas_matrix[i,] <- list.sensitivities[[1]]
      betas_interact_matrix[i,,] <- list.sensitivities[[2]]
    }
    
  }
  return(list(Y_hat_is_list_NN, Y_hat_oos_list_NN, betas_matrix, betas_interact_matrix))
}


sensitivities_NN <- function(model, x_train){
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
  
  H_0 <- drop(as.array(d2y_tf))
  J_0 <- drop(as.array(dy_tf))
  return(list(J_0, H_0))
}
