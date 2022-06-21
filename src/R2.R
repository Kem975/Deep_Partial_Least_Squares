# Functions to calculate the R² statistic of the different models.

# Input: t_0, T, Y_hat calculated by the models and the observed Y_test
# Output: Figure of the R² statistic over time with the average in the legend

r2_is = function(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, y_train_list, dates){
  r2.vect_is <- c()
  r2.lasso_vect_is <- c()
  r2.nn_vect_is <- c()
  r2.pls_vect_is <- c()
  
  for (i in 1:T){
    Y_hat_train <- Y_hat_is_list[[i]]
    
    rss <- (sum((y_train_list[[t_0+i]]-mean(y_train_list[[t_0+i]]))^2))
    
    r2.iter_is <- 1 - ((sum((y_train_list[[t_0+i]]-Y_hat_train)^2))/rss)
    r2.lasso_iter_is <- 1 - ((sum((y_train_list[[t_0+i]]-Yhat_LASSO_is_list[[i]])^2))/rss)
    r2.nn_iter_is <- 1 - ((sum((y_train_list[[t_0+i]]-Y_hat_is_list_NN[[i]])^2))/rss)
    r2.pls_iter_is <- 1 - ((sum((y_train_list[[t_0+i]]-Y_hat_PLS_is_list[[i]])^2))/rss)
    
    r2.vect_is[i] <- r2.iter_is
    r2.lasso_vect_is[i] <- r2.lasso_iter_is
    r2.nn_vect_is[i] <- r2.nn_iter_is
    r2.pls_vect_is[i] <- r2.pls_iter_is
  }
  
  label.is <- sprintf("DPLS In-sample (%.3f)", mean(r2.vect_is))
  lasso.label.is <- sprintf("LASSO In-sample (%.3f)", mean(r2.lasso_vect_is))
  nn.label.is <- sprintf("NN In-sample (%.3f)", mean(r2.nn_vect_is))
  pls.label.is <- sprintf("PLS In-sample (%.3f)", mean(r2.pls_vect_is))
  
  x <- 1:length(r2.vect_is)
  df.toplot_r2 <- data.frame(x)
  ggplot(df.toplot_r2, aes(x)) + 
    geom_line(aes(y=r2.lasso_vect_is, color="1"), size=0.8) +
    geom_line(aes(y=r2.nn_vect_is, color="2"), size=0.8) +
    geom_line(aes(y=r2.pls_vect_is, color="3"), size=0.8) +
    geom_line(aes(y=r2.vect_is, color="4"), size=0.3) + 
    scale_colour_manual(name = "Legend", 
                        values=c("tomato", "orange", "cadetblue3", "black"), 
                        labels = c(lasso.label.is, nn.label.is, pls.label.is, label.is)) + 
    scale_x_continuous(breaks = seq(t_0+1,T,T/20), labels = dates[seq(t_0+1,T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.2), legend.background = element_rect(fill = "white", color = "black")) +
    ylim(-1, 1) +
    labs(y = TeX("$R^2$ In Sample"), x="")
  ggsave(file="../figure/png/In_sample_R2.png", width=10, height=5, dpi=400)
  ggsave(file="../figure/pdf/In_sample_R2.pdf", width=10, height=5)
}


r2_oos = function(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, y_test_list, dates){
  r2.vect_oos <- c()
  r2.lasso_vect_oos <- c()
  r2.nn_vect_oos <- c()
  r2.pls_vect_oos <- c()
  
  for (i in 1:T){
    Y_hat_test <- Y_hat_oos_list[[i]]
    
    rss <- (sum((y_test_list[[t_0+i]]-mean(y_test_list[[t_0+i]]))^2))
    
    r2.iter_oos <- 1 - ((sum((y_test_list[[t_0+i]]-Y_hat_test)^2))/rss)
    r2.lasso_iter_oos <- 1 - ((sum((y_test_list[[t_0+i]]-Yhat_LASSO_oos_list[[i]])^2))/rss)
    r2.nn_iter_oos <- 1 - ((sum((y_test_list[[t_0+i]]-Y_hat_oos_list_NN[[i]])^2))/rss)
    r2.pls_iter_oos <- 1 - ((sum((y_test_list[[t_0+i]]-Y_hat_PLS_oos_list[[i]])^2))/rss)
    
    r2.vect_oos[i] <- r2.iter_oos
    r2.lasso_vect_oos[i] <- r2.lasso_iter_oos
    r2.nn_vect_oos[i] <- r2.nn_iter_oos
    r2.pls_vect_oos[i] <- r2.pls_iter_oos
  }
  
  label.oos <- sprintf("DPLS Out-of-sample (%.3f)", mean(r2.vect_oos))
  lasso.label.oos <- sprintf("LASSO Out-of-sample (%.3f)", mean(r2.lasso_vect_oos))
  nn.label.oos <- sprintf("NN Out-of-sample (%.3f)", mean(r2.nn_vect_oos))
  pls.label.oos <- sprintf("PLS Out-of-sample (%.3f)", mean(r2.pls_vect_oos))
  
  x <- 1:length(r2.vect_oos)
  df.toplot_r2 <- data.frame(x)
  ggplot(df.toplot_r2, aes(x)) + 
    geom_line(aes(y=r2.lasso_vect_oos, color="1"), size=0.8) +
    geom_line(aes(y=r2.nn_vect_oos, color="2"), size=0.8) +
    geom_line(aes(y=r2.pls_vect_oos, color="3"), size=0.8) +
    geom_line(aes(y=r2.vect_oos, color="4"), size=0.3) + 
    scale_colour_manual(name = "Legend", 
                        values=c("tomato", "orange","cadetblue3","black"), 
                        labels = c(lasso.label.oos, nn.label.oos, pls.label.oos, label.oos)) + 
    scale_x_continuous(breaks = seq(t_0+1,T,T/20), labels = dates[seq(t_0+1,T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.8), legend.background = element_rect(fill = "white", color = "black")) +
    ylim(-1, 1) +
    labs(y = TeX("$R^2$ Out of sample"), x="") 
  ggsave(file="../figure/png/Out_of_sample_r2.png", width=10, height=5, dpi=400)
  ggsave(file="../figure/pdf/Out_of_sample_r2.pdf", width=10, height=5)
  
}