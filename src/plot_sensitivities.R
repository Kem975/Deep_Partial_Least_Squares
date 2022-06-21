plot_sensitivities <- function(T, betas_matrix, name){
  factors_names  <- c('B/P', 'CF/P', 'E/P', 'S/EV', 'EB/EV', 'FE/P', 'TAG', 'MC', 'S',
                      'TA', 'TrA', 'EaV/TA', 'CFV/TA', 'SV/TA', 'RV', 'CB', 'DIV', 'EG',
                      'I0', 'I1', 'I2', 'I3', 'I4', 'I5', 'SI0', 'SI1', 'SI2', 'SI3',
                      'SI4', 'SI5', 'SI6', 'SI7', 'SI8', 'SI9', 'SI10', 'S0', 'S1',
                      'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'IG0',
                      'IG1', 'IG2', 'IG3')
  
  betas_matrix_median <- apply(betas_matrix, 2, mean)
  
  df.betas <- data.frame(value=(betas_matrix_median), factor=factors_names)
  df.betas <- df.betas[order(-df.betas$value),]
  rownames(df.betas) <- seq(1,49)
  
  ggplot(df.betas, aes(x=reorder(factor, value), y = value)) + geom_bar(stat = "identity", fill="dodgerblue1") + 
    labs(x="Factor", y="Sensitivity") + coord_flip() 
  
  ggsave(file=sprintf("../figure/png/sensitivity_%s.png", name), width=5.5, height=7, dpi=800)
  ggsave(file=sprintf("../figure/pdf/sensitivity_%s.pdf", name), width=5.5, height=7)
  
  
  #increasing <- order(betas_matrix_median)
  
  #betas_sorted <- apply(betas_matrix, 2, sort)
  #num_dates <- T/4
  #betas_sorted <- betas_matrix[,increasing] #rbind(betas_sorted[1:num_dates,], betas_sorted[((T-num_dates):T),])
  
  df.betas_boxplot <- data.frame()
  for (i in 1:dim(betas_matrix)[1]){
    df.betas_boxplot <- rbind(df.betas_boxplot, data.frame(value=betas_matrix[i,], factor=factors_names))
  }
  
  ggplot(df.betas_boxplot, aes(x=reorder(factor,-value), y = value)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust=1.2)) + labs(x="Factor", y="Sensitivity")
  ggsave(file=sprintf("../figure/png/sensitivity_box_plot_%s.png", name), width=10, height=5, dpi=800)
  ggsave(file=sprintf("../figure/pdf/sensitivity_box_plot_%s.pdf", name), width=10, height=5)
}




plot_interaction <- function(T, betas_interact_matrix, num_to_plot= 30, name){
  factors_names  <- c('B/P', 'CF/P', 'E/P', 'S/EV', 'EB/EV', 'FE/P', 'TAG', 'MC', 'S',
                      'TA', 'TrA', 'EaV/TA', 'CFV/TA', 'SV/TA', 'RV', 'CB', 'DIV', 'EG',
                      'I0', 'I1', 'I2', 'I3', 'I4', 'I5', 'SI0', 'SI1', 'SI2', 'SI3',
                      'SI4', 'SI5', 'SI6', 'SI7', 'SI8', 'SI9', 'SI10', 'S0', 'S1',
                      'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'IG0',
                      'IG1', 'IG2', 'IG3')
  
  interaction_names <- c()
  for (i in 1:length(factors_names)){
    for (j in i:length(factors_names)){
      interaction_names <- append(interaction_names, sprintf("%s - %s",factors_names[i], factors_names[j]))
    }
  }
  
  all_interactions = matrix(0, nrow=T, ncol=length(interaction_names))
  for (i in 1:T){
    all_interactions[i,] <- betas_interact_matrix[i,,][upper.tri(betas_interact_matrix[i,,], diag=TRUE)]
  }
  
  all_interactions_median <- apply(all_interactions, 2, mean)
  df.interactions_all <- data.frame(value=(all_interactions_median), factor=interaction_names)
  df.interactions_all_sorted <- df.interactions_all[order(-df.interactions_all$value),]
  
  df.interaction_to_plot <- rbind(df.interactions_all_sorted[1:num_to_plot,], df.interactions_all_sorted[((length(interaction_names)-num_to_plot):length(interaction_names)),])
  
  ggplot(df.interaction_to_plot, aes(x=reorder(factor, value), y = value)) + geom_bar(stat = "identity", fill="dodgerblue1") + 
    labs(x="Factor", y="Sensitivity") + coord_flip() 
  ggsave(file=sprintf("../figure/png/interaction_%s.png",name), width=5.5, height=7, dpi=800)
  ggsave(file=sprintf("../figure/pdf/interaction_%s.pdf",name), width=5.5, height=7)
  
  
  
  #betas_interact_sorted <- apply(all_interactions, 2, sort)
  #num_dates <- T/4
  #betas_interact_sorted <- rbind(betas_interact_sorted[1:num_dates,], betas_interact_sorted[((T-num_dates):T),])
  
  increasing_interaction <- order(all_interactions_median)
  
  all_interactions_ordered <- all_interactions[,increasing_interaction] 
  
  num_to_plot <- 30
  
  order_min <- order(all_interactions_median)[1:num_to_plot]
  order_max <- order(-all_interactions_median)[1:num_to_plot]
  
  df.betas_interact_boxplot <- data.frame()
  for (i in 1:dim(all_interactions)[1]){
    df.betas_interact_boxplot <- rbind(df.betas_interact_boxplot, data.frame(value=all_interactions[i,][order_min], factor=interaction_names[order_min]))
    df.betas_interact_boxplot <- rbind(df.betas_interact_boxplot, data.frame(value=all_interactions[i,][order_max], factor=interaction_names[order_max]))
  }
  
  ggplot(df.betas_interact_boxplot, aes(x=reorder(factor,-value), y = value)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust=1.2)) + labs(x="Factor", y="Sensitivity")
  ggsave(file=sprintf("../figure/png/interaction_box_plot%s.png", name), width=10, height=5, dpi=800)
  ggsave(file=sprintf("../figure/pdf/interaction_box_plot%s.pdf", name), width=10, height=5)
  
}