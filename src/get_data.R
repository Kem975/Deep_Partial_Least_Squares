get_all_data <- function(df.data, dates){
  Xs_train <- list()
  Ys_train <- list()
  Xs_test <- list()
  Ys_test <- list()
  
  for (i in 1:331){
    train <- subset(df.data, date==dates[i])
    test <- subset(df.data, date==dates[i+1])
    
    train$date <- c()
    test$date <- c()
    
    for (k in 1:49){
      var.col <- var(train[,k])
      mean.col <- mean(train[,k])
      
      if (var.col != 0){
        train[,k] <- (train[,k]-mean.col) / sqrt(var.col)
        test[,k] <- (test[,k]-mean.col) / sqrt(var.col) 
      }
    }
    Xs_train[[i]] <- train
    Ys_train[[i]] <- train$xs_return
    Xs_test[[i]] <- test
    Ys_test[[i]] <- test$xs_return
  }
  return(list(Xs_train, Ys_train, Xs_test, Ys_test))
}