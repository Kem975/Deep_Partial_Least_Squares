# Functions to obtain the Table 1 of the paper.
# 1) For each model, calculate Y_hat with the specific value of k
#   1.1) PCA and PLS are straighforward
#   1.2) For DPLS, we use the U_hat (Y-scores predicted) saved during the training. 
#        We obtain Y_hat by averaging over k columns
# 2) Calculate the R² values for in-sample all stocks and portfolio.
#    Same for out-of-sample unless for PCA where it can't be done.

# Input: Values of k, t_0, T, portfolio size, Y_hat calculated by the models, U_hat calculated for DPLS, X_trains, X_tests and the observed Y_test
# Output: Figure of the L_infinity norm over time with the average in the legend

table <- function(T, t_0, port_size, Uhat_is_list, Uhat_oos_list, x_train_list, x_test_list, y_train_list, y_test_list, comp.num_list, dates){
  df.table <- matrix(0, nrow=10, ncol=15)
  
  print("PCA")
  
  df.table[1,] <- table.PCA(T, t_0, port_size=40, Xs_train, dates, bPortfolio=FALSE)
  df.table[2,] <- table.PCA(T, t_0, port_size=40, Xs_train, dates, bPortfolio=TRUE)
  
  print("PLS")
  
  df.table[3,] <- table.PLS(T, t_0, port_size, x_train_list, x_test_list,  bTest=FALSE, bPortfolio=FALSE)
  df.table[4,] <- table.PLS(T, t_0, port_size, x_train_list, x_test_list,  bTest=FALSE, bPortfolio=TRUE)
  df.table[5,] <- table.PLS(T, t_0, port_size, x_train_list, x_test_list,  bTest=TRUE, bPortfolio=FALSE)
  df.table[6,] <- table.PLS(T, t_0, port_size, x_train_list, x_test_list,  bTest=TRUE, bPortfolio=TRUE)
  
  print("DPLS")
  
  df.table[7,] <- table.DPLS(T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list, bTest=FALSE, bPortfolio=FALSE)
  df.table[8,] <- table.DPLS(T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list, bTest=FALSE, bPortfolio=TRUE)
  df.table[9,] <- table.DPLS(T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list, bTest=TRUE, bPortfolio=FALSE)
  df.table[10,] <- table.DPLS(T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list, bTest=TRUE, bPortfolio=TRUE)

  
  colnames(df.table) <- seq(1,15)
  rownames(df.table) <- c("PCA all (is)","PCA portfolio (is)","PLS all (is)", "PLS portfolio (is)", "PLS all (oos)", "PLS portfolio (oos)",
                          "DPLS all (is)", "DPLS portfolio (is)", "DPLS all (oos)", "DPLS portfolio (oos)")
  return(df.table)
}


### --------------- DPLS --------------- ###


table.DPLS <- function(T, t_0, port_size, Uhat_is_list, Uhat_oos_list, y_train_list, y_test_list, comp.num_list, bTest, bPortfolio){
  rsq.DPLS <- rep(0,max(comp.num_list))
  
  rss <- rep(0,max(comp.num_list))
  tss <- rep(0,max(comp.num_list))

  r_p <- matrix(0, T, max(comp.num_list))
  r_hat_p <- matrix(0, T, max(comp.num_list))

  
  for (t in 1:T){
    
    for (k in 1:comp.num_list[t]){
      if (k<2){
        Y_hat_is <- Uhat_is_list[[t]][,1]
        Y_hat_oos <- Uhat_oos_list[[t]][,1]
      } else{
        Y_hat_is <- rowMeans(Uhat_is_list[[t]][,1:k])
        Y_hat_oos <- rowMeans(Uhat_oos_list[[t]][,1:k])
      }
      
      if(bTest){
        Y_hat <- Y_hat_oos
        Y <- y_test_list[[t_0+t]]
      }
      else{
        Y_hat <- Y_hat_is
        Y <- y_train_list[[t_0+t]]
      }
      
      if(bPortfolio){
        idx <- order(Y_hat)[length(Y_hat):1]
        w <- rep(0,length(Y_hat))
        w[idx[1:port_size]] <- 1/port_size
        r_p[t,k] <- array(t(w)%*% Y)
        r_hat_p[t,k] <- array(t(w)%*%Y_hat)
      }
      else{
        rss[k] <- rss[k] + sum((Y_hat - Y)^2)  # Save the sum of rss in-sample for each k
        tss[k] <- tss[k] + sum((Y - mean(Y))^2) # Add the in-sample TSS for this date 
      }
    }
    
  }
  
  #print(r_hat_p_oos)
  #print(r_p_oos)
  for (k in 1:max(comp.num_list)){
    # Portfolio
    r <- r_p[,k][r_p[,k]!=0]
    r_hat <- r_hat_p[,k][r_hat_p[,k]!=0]
    
    if(bPortfolio){
      rsq.DPLS[k] <- 1 - (sum((r - r_hat)^2)/sum((r - mean(r))^2))
    }
    else{
      rsq.DPLS[k] <- 1 - rss[k]/tss[k]
    }
  }
  return(round(rsq.DPLS,4)[1:15])
}



### --------------- PLS --------------- ###


table.PLS <- function(T, t_0, port_size, x_train_list, x_test_list, comp_num_list_PLS, bTest, bPortfolio){
  Ks <- seq(1,15)
  rsq.PLS <- rep(0,length(Ks))
  rss <- rep(0,length(Ks))
  tss <- rep(0,length(Ks))
  r_p <- matrix(0, T, length(Ks))
  r_hat_p <- matrix(0, T, length(Ks))
  
  for (t in 1:T){
    validation <- x_train_list[[t_0+t-1]]
    train = x_train_list[[t_0+t]]
    test = x_test_list[[t_0+t]]
    
    fit <- plsr(formula = xs_return~., ncomp = Ks[length(Ks)], data=train, rescale = F)
    MSE <- rep(0, length(Ks))
    for (k in 1:length(Ks)){
      Y_hat <- predict(fit, ncomp = Ks[k], newdata = validation)[,1,]
      Y<-validation[,50]
      MSE[k] <- mean((Y_hat-Y)^2)
    }
    K_star <- which.min(MSE)

    for (k in 1:K_star){
      if (bTest){
        Y_hat <- predict(fit, ncomp = k, newdata = test)[,1,]
        Y<-test[,50]
      }
      else{
        Y_hat <- predict(fit, ncomp = Ks[k], newdata = train)[,1,]
        Y<-train[,50]
      }
      
      if (bPortfolio){
        idx <- order(Y_hat)[length(Y_hat):1]
        w <- rep(0,length(Y_hat))
        w[idx[1:port_size]] <- 1/port_size
        r_p[t,k] <- array(t(w)%*%Y)
        r_hat_p[t,k] <- array(t(w)%*%Y_hat)
      }
      else{
        rss[k] <- rss[k] + sum((Y_hat - Y)^2)
        tss[k] <- tss[k] + sum((Y-mean(Y))^2)
      }
    }   
    
  }
  for (k in 1:length(Ks)){
    r <- r_p[,k][r_p[,k]!=0]
    r_hat <- r_hat_p[,k][r_hat_p[,k]!=0]
    if (bPortfolio){
      rsq.PLS[k] <- 1 - (sum((r - r_hat)^2)/sum((r - mean(r))^2))
    }
    else{
      rsq.PLS[k] <- 1 - rss[k]/tss[k]
    }
  }
  return(round(rsq.PLS,4))
}



### --------------- PCA --------------- ###


table.PCA <- function(T, t_0, port_size, x_train_list, dates, bPortfolio){
  Ks <- seq(1,15)
  rsq.PCA <- rep(0,length(Ks))
  y_<-list()
  for (t in 1:T){
    train <- x_train_list[[t_0+t]]
    y_train <- train[,50]
    y_[[t]]<- y_train # Xs return
  }
  minN <-1000 
  
  for (t in 1:T){
    if (length(y_[[t]])<minN){ # Finding the minimum number of stocks  
      minN=length(y_[[t]])
    }
  }
  
  Y_train <- matrix(0, T, minN) # Matrix of Xs_return
  for (t in 1:T){
    Y_train[t,] <- y_[[t]][1:minN] # Taking the min number 
  }
  
  N <- minN
  
  Y_train <- xts(Y_train, order.by=dates[(t_0+1):(t_0+T)]) # Transform to time series object 
  
  for (k in 1:length(Ks)){
    factor_model <- factorModel(Y_train, type = "S", K = Ks[k], max_iter = 10)
    Y_hat <- with(factor_model, matrix(alpha, T, N, byrow = TRUE) + factors %*% t(beta)) # + residual)
    
    if (bPortfolio)
    { 
      r_p<-rep(0,T)
      r_hat_p<-rep(0,T)
      for (t in 1:T){
        idx<-order(Y_hat[t,])[N:1]
        w<-rep(0,N)
        w[idx[1:port_size]]<-1/port_size
        r_p[t]<-Y_train[t,]%*%w
        r_hat_p[t]<-Y_hat[t,]%*%w
      }
      
      rsq.PCA[k] <- 1 - (sum((r_p - r_hat_p)^2)/sum((r_p - mean(r_p))^2))
    }
    else
    {
      rss <- sum((Y_hat - Y_train)^2)  ## residual sum of squares
      alpha<-colMeans(Y_train)
      tss <- sum((Y_train- matrix(alpha, T, N, byrow = TRUE)) ^ 2)  ## total sum of squares
      rsq.PCA[k] <- 1 - rss/tss
      
    }
  }
  return(round(rsq.PCA,4))
}
