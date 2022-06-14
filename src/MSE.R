# Functions to calculate the Mean Squared Error of the different models.

# Input: t_0, T, Y_hat calculated by the models and the observed Y_test
# Output: Figure of the Mean Squared Error over time with the average in the legend

MSE_is = function(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, y_train_list, dates){
  mse.vect_is <- c()
  mse.lasso_vect_is <- c()
  mse.nn_vect_is <- c()
  mse.pls_vect_is <- c()
  
  for (i in 1:T){
    Y_hat_train <- Y_hat_is_list[[i]]
    
    mse.iter_is <- mean((y_train_list[[t_0+i]]-Y_hat_train)^2)
    mse.lasso_iter_is <- mean((y_train_list[[t_0+i]]-Yhat_LASSO_is_list[[i]])^2)
    mse.nn_iter_is <- mean((y_train_list[[t_0+i]]-Y_hat_is_list_NN[[i]])^2)
    mse.pls_iter_is <- mean((y_train_list[[t_0+i]]-Y_hat_PLS_is_list[[i]])^2)
    
    
    mse.vect_is[i] <- mse.iter_is
    mse.lasso_vect_is[i] <- mse.lasso_iter_is
    mse.nn_vect_is[i] <- mse.nn_iter_is
    mse.pls_vect_is[i] <- mse.pls_iter_is
  }
  
  label.is <- sprintf("DPLS In-sample (%.3f)", mean(mse.vect_is))
  lasso.label.is <- sprintf("LASSO In-sample (%.3f)", mean(mse.lasso_vect_is))
  nn.label.is <- sprintf("NN In-sample (%.3f)", mean(mse.nn_vect_is))
  pls.label.is <- sprintf("PLS In-sample (%.3f)", mean(mse.pls_vect_is))
  
  
  x <- 1:length(mse.vect_is)
  df.toplot_mse <- data.frame(x)
  ggplot(df.toplot_mse, aes(x)) + 
    geom_line(aes(y=mse.vect_is, color="1"), size=0.8) + 
    geom_line(aes(y=mse.lasso_vect_is, color="2"), size=0.8) +
    geom_line(aes(y=mse.nn_vect_is, color="3"), size=0.8) +
    geom_line(aes(y=mse.pls_vect_is, color="4"), size=0.8) +
    scale_colour_manual(name = "Legend", 
                        values=c("cadetblue3", "tomato", "orange", "green"), 
                        labels = c(label.is, lasso.label.is, nn.label.is, pls.label.is)) + 
    scale_x_continuous(breaks = seq(t_0+1,t_0+T,T/20), labels = dates[seq(t_0+1,t_0+T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.8), legend.background = element_rect(fill = "white", color = "black")) +
    labs(y ="MSE In Sample", x="")  +  scale_y_continuous(trans='log10')
  ggsave(file="../png/In_sample_MSE_tuned.png", width=10, height=5, dpi=400)
  ggsave(file="../pdf/In_sample_MSE_tuned.pdf", width=10, height=5)
}


MSE_oos = function(t_0,T,Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, y_test_list, dates){
  mse.vect_oos <- c()
  mse.lasso_vect_oos <- c()
  mse.nn_vect_oos <- c()
  mse.pls_vect_oos <- c()
  
  for (i in 1:T){
    Y_hat_test <- Y_hat_oos_list[[i]]
    
    mse.iter_oos <- mean((y_test_list[[t_0+i]]-Y_hat_test)^2)
    mse.lasso_iter_oos <- mean((y_test_list[[t_0+i]]-Yhat_LASSO_oos_list[[i]])^2)
    mse.nn_iter_oos <- mean((y_test_list[[t_0+i]]-Y_hat_oos_list_NN[[i]])^2)
    mse.pls_iter_oos <- mean((y_test_list[[t_0+i]]-Y_hat_PLS_oos_list[[i]])^2)
    
    mse.vect_oos[i] <- mse.iter_oos
    mse.lasso_vect_oos[i] <- mse.lasso_iter_oos
    mse.nn_vect_oos[i] <- mse.nn_iter_oos
    mse.pls_vect_oos[i] <- mse.pls_iter_oos
  }
  
  label.oos <- sprintf("DPLS Out-of-sample (%.3f)", mean(mse.vect_oos))
  lasso.label.oos <- sprintf("LASSO Out-of-sample (%.3f)", mean(mse.lasso_vect_oos))
  nn.label.oos <- sprintf("NN Out-of-sample (%.3f)", mean(mse.nn_vect_oos))
  pls.label.oos <- sprintf("PLS Out-of-sample (%.3f)", mean(mse.pls_vect_oos))
  
  x <- 1:length(mse.vect_oos)
  df.toplot_mse <- data.frame(x)
  ggplot(df.toplot_mse, aes(x)) + 
    geom_line(aes(y=mse.vect_oos, color="1"), size=0.8) + 
    geom_line(aes(y=mse.lasso_vect_oos, color="2"), size=0.8) +
    geom_line(aes(y=mse.nn_vect_oos, color="3"), size=0.8) +
    geom_line(aes(y=mse.pls_vect_oos, color="4"), size=0.8) +
    scale_colour_manual(name = "Legend", 
                        values=c("cadetblue3","tomato", "orange", "green"), 
                        labels = c(label.oos, lasso.label.oos, nn.label.oos, pls.label.oos)) + 
    scale_x_continuous(breaks = seq(t_0+1,t_0+T,T/20), labels = dates[seq(t_0+1,t_0+T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.8), legend.background = element_rect(fill = "white", color = "black")) +
    labs(y ="MSE Out of Sample", x="")  +  scale_y_continuous(trans='log10')
  ggsave(file="../png/Out_of_sample_MSE_tuned.png", width=10, height=5, dpi=400)
  ggsave(file="../pdf/Out_of_sample_MSE_tuned.pdf", width=10, height=5)
  
}