# Function to calculate the information ratio of the different models.

# Input: t_0, T, Y_hat calculated by the models and the observed Y_test
# Output: Figure of the information ratio for different size of portfolios

IR <- function(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, y_test_list){
  info_ratio_LASSO <- c()
  info_ratio_DPLS <- c()
  info_ratio_NN <- c()
  info_ratio_PLS <- c()
  info_ratio_wn <- c()
  
  Ms <- c(10,15,20,25,30,35,40,45,50)
  
  for (m in 1:length(Ms)){
    
    excess_return_LASSO <- c()
    excess_return_DPLS <- c()
    excess_return_NN <- c()
    excess_return_PLS <- c()
    excess_return_wn <- c()
    
    for (i in 1:T){
      yhat_LASSO <- Yhat_LASSO_oos_list[[i]]
      sorted_LASSO <- sort(yhat_LASSO, index.return=TRUE, decreasing=TRUE)
      idx_LASSO <- sorted_LASSO$ix[1:Ms[m]]
      excess_return_LASSO <- append( excess_return_LASSO, mean(y_test_list[[t_0+i]][idx_LASSO]) )
      
      yhat_PLS <- Y_hat_PLS_oos_list[[i]]
      sorted_PLS <- sort(yhat_PLS, index.return=TRUE, decreasing=TRUE)
      idx_PLS <- sorted_PLS$ix[1:Ms[m]]
      excess_return_PLS <- append( excess_return_PLS, mean(y_test_list[[t_0+i]][idx_PLS]) )
      
      yhat_oos_DPLS <- Y_hat_oos_list[[i]]
      sorted_DPLS <- sort(yhat_oos_DPLS, index.return=TRUE, decreasing=TRUE)
      idx_DPLS <- sorted_DPLS$ix[1:Ms[m]]
      excess_return_DPLS <- append(excess_return_DPLS, mean( y_test_list[[t_0+i]][idx_DPLS] ))
      
      yhat_oos_NN <- Y_hat_oos_list_NN[[i]]
      sorted_NN <- sort(yhat_oos_NN, index.return=TRUE, decreasing=TRUE)
      idx_NN <- sorted_NN$ix[1:Ms[m]]
      excess_return_NN <- append(excess_return_NN, mean( y_test_list[[t_0+i]][idx_NN] ))
      
      idx_wn <- sample(1:length(yhat_oos_NN), Ms[m])
      excess_return_wn <- append(excess_return_wn, mean( y_test_list[[t_0+i]][idx_wn] ))
      
    }
    
    info_ratio_LASSO[m] <- mean(excess_return_LASSO)/sd(excess_return_LASSO)
    info_ratio_DPLS[m] <- mean(excess_return_DPLS)/sd(excess_return_DPLS)
    info_ratio_NN[m] <- mean(excess_return_NN)/sd(excess_return_NN)
    info_ratio_PLS[m] <- mean(excess_return_PLS)/sd(excess_return_PLS)
    info_ratio_wn[m] <- mean(excess_return_wn)/sd(excess_return_wn)
  }
  
  label_LASSO <- sprintf("LASSO (%.2f)", mean(info_ratio_LASSO))
  label_NN <- sprintf("NN (%.2f)", mean(info_ratio_NN))
  label_DPLS <- sprintf("DPLS (%.2f)", mean(info_ratio_DPLS))
  label_PLS <- sprintf("PLS (%.2f)", mean(info_ratio_PLS))
  label_wn <- sprintf("Random (%.2f)", mean(info_ratio_wn))
  
  df.IR <- data.frame(x=seq(10,50,5))
  ggplot(df.IR, aes(x)) + 
    geom_line(aes(y=info_ratio_DPLS, color="1"), size=0.8) +
    geom_line(aes(y=info_ratio_LASSO, color="2"), size=0.8) +
    geom_line(aes(y=info_ratio_NN, color="3"), size=0.8) +
    geom_line(aes(y=info_ratio_PLS, color="4"), size=0.8) +
    geom_line(aes(y=info_ratio_wn, color="5"), size=0.8) +   
    scale_colour_manual(name = "Legend", 
                        labels = c(label_DPLS, 
                                   label_LASSO,
                                   label_NN,
                                   label_PLS,
                                   label_wn),
                        values=c("cadetblue3", "tomato", "orange", "green", "black")) + 
    labs(x="Portfolio Size", y="Information Ratio") +
    theme(legend.position = c(0.9, 0.4), legend.background = element_rect(fill = "white", color = "black"))
    
  
  ggsave(file="../figure/png/IR_full_date_tuneddqzdqz.png", width=10, height=5, dpi=800)
  ggsave(file="../figure/pdf/IR_full_date_tuneddqdqz.pdf", width=10, height=5)
  
}