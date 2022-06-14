# Functions to obtain the Table 1 of the paper.
# 1) For each model, calculate Y_hat with the specific value of k
#   1.1) PCA and PLS are straighforward
#   1.2) For DPLS, we use the U_hat (Y-scores predicted) saved during the training. 
#        We obtain Y_hat by averaging over k columns
# 2) Calculate the R² values for in-sample all stocks and portfolio.
#    Same for out-of-sample unless for PCA where it can't be done.

# Input: Values of k, t_0, T, portfolio size, Y_hat calculated by the models, U_hat calculated for DPLS, X_trains, X_tests and the observed Y_test
# Output: Figure of the L_infinity norm over time with the average in the legend

table <- function(Ks, T, t_0, port_size, df.data, Uhat_is_list, Uhat_oos_list, x_train_list, x_test_list, y_train_list, y_test_list, comp.num_list){
  dates <- unique(as.Date(df.data$date))
  dates <- na.omit(dates)
  
  df.table <- matrix(0, nrow=10, ncol=length(Ks))
  
  print("PCA")
  
  df.table[1,] <- table.PCA(Ks, T, t_0, port_size, df.data, dates, bPortfolio=FALSE)
  df.table[2,] <- table.PCA(Ks, T, t_0, port_size, df.data, dates, bPortfolio=TRUE)
  
  print("PLS")
  
  df.table[3,] <- table.PLS(Ks, T, t_0, port_size, x_train_list, x_test_list,  bTest=FALSE, bPortfolio=FALSE)
  df.table[4,] <- table.PLS(Ks, T, t_0, port_size, x_train_list, x_test_list,  bTest=FALSE, bPortfolio=TRUE)
  df.table[5,] <- table.PLS(Ks, T, t_0, port_size, x_train_list, x_test_list,  bTest=TRUE, bPortfolio=FALSE)
  df.table[6,] <- table.PLS(Ks, T, t_0, port_size, x_train_list, x_test_list,  bTest=TRUE, bPortfolio=TRUE)
  
  print("DPLS")
  
  list_table_DPLS <- table.DPLS(Ks, T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, y_train_list, y_test_list, comp.num_list)
  df.table[7,] <- list_table_DPLS[[1]]
  df.table[8,] <- list_table_DPLS[[2]]
  df.table[9,] <- list_table_DPLS[[3]]
  df.table[10,] <- list_table_DPLS[[4]]
  
  colnames(df.table) <- c("1",  "2",  "3",  "4",  "5", "10", "20", "30", "40")
  rownames(df.table) <- c("PCA all (is)","PCA portfolio (is)","PLS all (is)", "PLS portfolio (is)", "PLS all (oos)", "PLS portfolio (oos)",
                          "DPLS all (is)", "DPLS portfolio (is)", "DPLS all (oos)", "DPLS portfolio (oos)")
  return(df.table)
}


### --------------- DPLS --------------- ###


table.DPLS <- function(Ks, T, t_0, port_size, Uhat_is_list, Uhat_oos_list, y_train_list, y_test_list, comp.num_list){
  rsq.DPLS_port_is <- rep(0,max(comp.num_list))
  rsq.DPLS_port_oos <- rep(0,max(comp.num_list))
  rsq.DPLS_is <- rep(0,max(comp.num_list))
  rsq.DPLS_oos <- rep(0,max(comp.num_list))
  
  rss_is <- rep(0,max(comp.num_list))
  tss_is <- rep(0,max(comp.num_list))
  rss_oos <- rep(0,max(comp.num_list))
  tss_oos <- rep(0,max(comp.num_list))
  
  r_p_is <- matrix(0, T, max(comp.num_list))
  r_hat_p_is <- matrix(0, T, max(comp.num_list))
  r_p_oos <- matrix(0, T,max(comp.num_list))
  r_hat_p_oos <- matrix(0, T, max(comp.num_list))
  
  
  
  for (t in 1:T){
    
    for (k in 1:comp.num_list[t]){
      if (k<2){
        Y_hat_is <- Uhat_is_list[[t]][,1]
        Y_hat_oos <- Uhat_oos_list[[t]][,1]
      } else{
        Y_hat_is <- rowMeans(Uhat_is_list[[t]][,1:k])
        Y_hat_oos <- rowMeans(Uhat_oos_list[[t]][,1:k])
      }
      
      # Portfolio IS
      idx_is <- order(Y_hat_is)[length(Y_hat_is):1]
      w_is <- rep(0,length(Y_hat_is))
      w_is[idx_is[1:port_size]] <- 1/port_size
      r_p_is[t,k] <- array(t(w_is)%*% y_train_list[[t_0+t]])
      r_hat_p_is[t,k] <- array(t(w_is)%*%Y_hat_is)
      
      # All stocks IS
      rss_is[k] <- rss_is[k] + sum((Y_hat_is - y_train_list[[t_0+t]])^2)  # Save the sum of rss in-sample for each k
      
      # Portfolio OOS
      idx_oos <- order(Y_hat_oos)[length(Y_hat_oos):1]
      w_oos <- rep(0,length(Y_hat_oos))
      w_oos[idx_oos[1:port_size]] <- 1/port_size
      #r_p_oos[t,k] <- array(t(w_oos)%*%y_test_list[[t_0+t]])
      #r_hat_p_oos[t,k] <- array(t(w_oos)%*%Y_hat_oos)
      r_p_oos[t,k] <- array(t(w_oos)%*%y_test_list[[t_0+t]])
      r_hat_p_oos[t,k] <- array(t(w_oos)%*%Y_hat_oos)
      
      #modelmetrics(y,yhat)
      
      # All stocks OOS
      rss_oos[k] <- rss_oos[k] + sum((Y_hat_oos - y_test_list[[t_0+t]])^2) # Save the sum of rss out-of-sample for each k
      
      # Total Sum of Squares
      tss_is[k] <- tss_is[k] + sum((y_train_list[[t_0+t]]-mean(y_train_list[[t_0+t]]))^2) # Add the in-sample TSS for this date 
      tss_oos[k] <- tss_oos[k] + sum((y_test_list[[t_0+t]]-mean(y_test_list[[t_0+t]]))^2) # Add the out-of-sample TSS for this date
    }
    
  }
  
  #print(r_hat_p_oos)
  #print(r_p_oos)
  for (k in 1:max(comp.num_list)){
    # Portfolio
    r_oos <- r_p_oos[,k][r_p_oos[,k]!=0]
    r_hat_oos <- r_hat_p_oos[,k][r_hat_p_oos[,k]!=0]
    r_is <- r_p_is[,k][r_p_is[,k]!=0]
    r_hat_is <- r_hat_p_is[,k][r_hat_p_is[,k]!=0]
    
    #print(length(r_hat_oos))
    rsq.DPLS_port_is[k] <- 1 - (sum((r_is - r_hat_is)^2)/sum((r_is - mean(r_is))^2))
    rsq.DPLS_port_oos[k] <- 1 - (sum((r_oos - r_hat_oos)^2)/sum((r_oos - mean(r_oos))^2))
    
    # All stocks
    rsq.DPLS_is[k] <- 1 - rss_is[k]/tss_is[k]
    rsq.DPLS_oos[k] <- 1 - rss_oos[k]/tss_oos[k]
  }
  return(list(rsq.DPLS_is, rsq.DPLS_port_is, rsq.DPLS_oos, rsq.DPLS_port_oos))
}



### --------------- PLS --------------- ###


table.PLS <- function(Ks, T, t_0, port_size, x_train_list, x_test_list, comp_num_list_PLS, bTest, bPortfolio){
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
    #print(K_star)
    
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
  return(rsq.PLS)
}



### --------------- PCA --------------- ###


table.PCA <- function(Ks, T, t_0, port_size, df.data, dates, bPortfolio){
  rsq.PCA <- rep(0,length(Ks))
  df.y <- df.data[,50:51]
  y_<-list()
  for (t in 1:T){
    y_[t]<-subset(df.y, date==dates[t_0+t]) # Xs return
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
  Ks<-c(1,2,3,4,5,10,20,30,40)
  
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
  return(rsq.PCA)
}
