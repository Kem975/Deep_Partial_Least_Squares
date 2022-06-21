# Functions to calculate the L_infinity norm of the different models.

# Input: t_0, T, Y_hat calculated by the models and the observed Y_test
# Output: Figure of the L_infinity norm over time with the average in the legend

L_infty_is = function(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, y_train_list, dates) {
  l_inf_LASSO.vect_is <- c()
  l_inf_NN.vect_is <- c()
  l_inf_PLS.vect_is <- c()
  l_inf.vect_is <- c()
  
  for (i in 1:T){
    yhat_is <- Y_hat_is_list[[i]]
    yhat_is_LASSO <- Yhat_LASSO_is_list[[i]]
    yhat_is_NN <- Y_hat_is_list_NN[[i]]
    yhat_is_PLS <- Y_hat_PLS_is_list[[i]]
    
    l_inf.vect_is[i] <- max(abs(yhat_is - y_train_list[[t_0+i]]))
    l_inf_LASSO.vect_is[i] <- max(abs(yhat_is_LASSO - y_train_list[[t_0+i]]))
    l_inf_NN.vect_is[i] <- max(abs(yhat_is_NN - y_train_list[[t_0+i]]))
    l_inf_PLS.vect_is[i] <- max(abs(yhat_is_PLS - y_train_list[[t_0+i]]))
  }
  label.is <- sprintf("DPLS In-sample (%.3f)", mean(l_inf.vect_is))
  lasso.label.is <- sprintf("LASSO In-sample (%.3f)", mean(l_inf_LASSO.vect_is))
  nn.label.is <- sprintf("NN In-sample (%.3f)", mean(l_inf_NN.vect_is))
  pls.label.is <- sprintf("PLS In-sample (%.3f)", mean(l_inf_PLS.vect_is))
  
  x <- 1:length(l_inf.vect_is)
  df.toplot_linf <- data.frame(x)
  ggplot(df.toplot_linf, aes(x)) + 
    geom_line(aes(y=l_inf_LASSO.vect_is, color="1"), size=0.8) +
    geom_line(aes(y=l_inf_NN.vect_is, color="2"), size=0.8) +
    geom_line(aes(y=l_inf_PLS.vect_is, color="3"), size=0.8) +
    geom_line(aes(y=l_inf.vect_is, color="4"), size=0.3) +
    scale_colour_manual(name = "Legend", 
                        values=c("tomato", "orange", "cadetblue3", "black"), 
                        labels = c(lasso.label.is, nn.label.is, pls.label.is, label.is)) + 
    scale_x_continuous(breaks = seq(t_0+1,t_0+T,T/20), labels = dates[seq(t_0+1,t_0+T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.8), legend.background = element_rect(fill = "white", color = "black")) +
    labs(y =TeX("$L_{\\infty}$ In Sample"), x="")  +  scale_y_continuous(trans='log10')
  ggsave(file="../figure/png/In_sample_Linfty_abs.png", width=10, height=5, dpi=400)
  ggsave(file="../figure/pdf/In_sample_Linfty_abs.pdf", width=10, height=5)
}



L_infty_oos = function(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, y_test_list, dates) {
  l_inf_LASSO.vect_oos <- c()
  l_inf_PLS.vect_oos <- c()
  l_inf_NN.vect_oos <- c()
  l_inf.vect_oos <- c()
  
  for (i in 1:T){
    yhat_oos <- Y_hat_oos_list[[i]]
    yhat_oos_LASSO <- Yhat_LASSO_oos_list[[i]]
    yhat_oos_NN <- Y_hat_oos_list_NN[[i]]
    yhat_oos_PLS <- Y_hat_PLS_oos_list[[i]]
    
    y_test <- y_test_list[[t_0+i]]
    
    l_inf.vect_oos[i] <- max(abs(yhat_oos - y_test))
    l_inf_NN.vect_oos[i] <- max(abs(yhat_oos_NN - y_test))
    l_inf_LASSO.vect_oos[i] <- max(abs(yhat_oos_LASSO - y_test))
    l_inf_PLS.vect_oos[i] <- max(abs(yhat_oos_PLS - y_test))
  }
  
  label.oos <- sprintf("DPLS Out-of-sample (%.3f)", mean(l_inf.vect_oos))
  lasso.label.oos <- sprintf("LASSO Out-of-sample (%.3f)", mean(l_inf_LASSO.vect_oos))
  nn.label.oos <- sprintf("NN Out-of-sample (%.3f)", mean(l_inf_NN.vect_oos))
  pls.label.oos <- sprintf("PLS Out-of-sample (%.3f)", mean(l_inf_PLS.vect_oos))
  
  x <- 1:length(l_inf.vect_oos)
  df.toplot_linf <- data.frame(x)
  ggplot(df.toplot_linf, aes(x)) + 
    geom_line(aes(y=l_inf_LASSO.vect_oos, color="1"), size=0.8) +
    geom_line(aes(y=l_inf_NN.vect_oos, color="2"), size=0.8) +
    geom_line(aes(y=l_inf_PLS.vect_oos, color="3"), size=0.8) +
    geom_line(aes(y=l_inf.vect_oos, color="4"), size=0.3) +
    scale_colour_manual(name = "Legend", 
                        values=c("tomato", "orange", "cadetblue3", "black"), 
                        labels = c(lasso.label.oos, nn.label.oos, pls.label.oos, label.oos)) + 
    scale_x_continuous(breaks = seq(t_0+1,t_0+T,T/20), labels = dates[seq(t_0+1,t_0+T,T/20)]) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2), legend.position = c(0.13, 0.8), legend.background = element_rect(fill = "white", color = "black")) +
    labs(y = TeX("$L_{\\infty}$ Out of sample"), x="")  +  scale_y_continuous(trans='log10')
  
  ggsave(file="../figure/png/Out_of_sample_Linfty_abs.png", width=10, height=5, dpi=400)
  ggsave(file="../figure/pdf/Out_of_sample_Linfty_abs.pdf", width=10, height=5)
}