sensitivities_DPLS <- function(model, x_train, max_comp_num, CV_comp_num){
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
  
  J_0 <- t(dy %*% t(plsr.input$loading.weights)) %*% Q
  
  betas_matrix[i,] <- J_0
  
  H_0 <- matrix(0,49,49)
  for (jj in 1:49){
    for (ii in 1:49){
      for (kk in 1:max_comp_num){
        H_0[ii,jj] <- H_0[ii,jj] + t(d2y[kk,,] %*% plsr.input$loading.weights[ii,]) %*% (plsr.input$loading.weights[jj,]) * Q[kk]
      }  
    }
  }
  
  return(list(J_0, H_0))
}


# Factor categories
factors_cat  <- c('Value', 'Value', 'Value', 'Value', 'Value', 'Value','Growth', 'Size', 'Size', 'Size', 'Trading Activity', 
                  'Earnings Variability', 'Earnings Variability', 'Earnings Variability','Volatility', 'Volatility', 'Value', 'Growth',
                  'GICS', 'GICS', 'GICS', 'GICS','GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 
                  'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 
                  'GICS', 'GICS', 'GICS', 'GICS', 'GICS', 'GICS')


Uhat_oos_list <- list()
Uhat_is_list <- list()

portfolio_size <- 20

Y_hat_oos_list <- list()
Y_hat_is_list <- list()

comp_num_list <- c()

df.attribution_DPLS <- data.frame()
return_attribution_DPLS <- list()
return_attribution_predicted_DPLS <- list()

bRetrain = TRUE
bAttribution_plot = TRUE

t_0 <- 0
T <- 300

betas_matrix <- array(0, c(T,49))
betas_interact_matrix <- array(0, c(T, 49, 49))

for (i in 1:T){
  print(i)
  pls.options(plsralg = "k")
  train <- Xs_train[[t_0+i]]
  test <- Xs_test[[t_0+i]]
  
  x_train_original <- as.matrix(subset(train, select = -c(xs_return))) 
  x_test_original <- as.matrix(subset(test, select = -c(xs_return))) 
  
  
  plsr.fit <- plsr(formula = xs_return~., data=train, rescale = F, validation="CV")
  cverr <- RMSEP(plsr.fit)$val[1,,]
  comp.num_CV <- which.min(cverr) - 1
  
  
  if (comp.num_CV<3){
    comp.num_CV <- 3
  }
  #comp_num_list[i] <- comp.num_CV
  
  plsr.input <- plsr(formula = xs_return~., ncomp=40, data=train, rescale = F)
  PLS_Score.input <- scores(plsr.input)
  
  x_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$loading.weights)
  y_train <- train$xs_return
  
  x_test <- as.matrix(test[,1:49]) %*% drop(plsr.input$loading.weights) 
  y_test <- test$xs_return
  
  U_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$coefficients)
  
  if(bRetrain==TRUE){
    model.PLS <- keras_model_sequential() %>%
      layer_dense(units = 100, activation = "softplus", input_shape = dim(x_train)[2], kernel_initializer='normal') %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = dim(x_train)[2], kernel_initializer='normal') 
    #summary(model.PLS)
    
    model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")
    model.PLS %>% fit( x = x_train, y = U_train, verbose = 0, 
                       epochs=100,
                       callbacks = list(callback_early_stopping(monitor = "loss", 
                                                                patience = 30, 
                                                                min_delta = 1e-4))) #restore_best_weights = TRUE
    
  } else{
    model.PLS <- load_model_tf(sprintf("Models/%s.h5", dates[t_0+i]))
  }
  
  U_hat_is = model.PLS %>% predict( x_train )
  U_hat_oos = model.PLS %>% predict( x_test )
  
  
  if (comp.num_CV > 40){
    comp.num_CV <- 40
    Y_hat_oos <- rowMeans(U_hat_oos[,1:comp.num_CV])
    Y_hat_is <- rowMeans(U_hat_is[,1:comp.num_CV])
  } else{
    Y_hat_oos <- rowMeans(U_hat_oos[,1:comp.num_CV])
    Y_hat_is <- rowMeans(U_hat_is[,1:comp.num_CV])
  }
  
  Uhat_oos_list[[i]] <- U_hat_oos
  Uhat_is_list[[i]] <- U_hat_is
  
  Y_hat_oos_list[[i]] <- Y_hat_oos
  Y_hat_is_list[[i]] <- Y_hat_is
  
  
  
  if(bAttribution_plot){
    x <- tf$zeros(shape(1,dim(x_train)[2]))
    
    with(tf$GradientTape() %as% t2, {
      with(tf$GradientTape() %as% t1, {
        t1$watch(x)
        t2$watch(x)
        y <- model.PLS(x)
      })
      dy_tf <- t1$jacobian(y, x)
    })
    d2y_tf <- t2$jacobian(dy_tf, x)
    
    d2y <- drop(as.array(d2y_tf))
    dy <- drop(as.array(dy_tf))
    
    Q <- rep(0,40)
    Q[1:comp.num_CV] <- 1/comp.num_CV
    #dy <- colMeans(drop(as.array(dy_tf))) 
    
    J_0 <- t(dy %*% t(plsr.input$loading.weights)) %*% Q
    
    betas_matrix[i,] <- J_0
    
    H_0 <- matrix(0,49,49)
    for (jj in 1:49){
      for (ii in 1:49){
        for (kk in 1:40){
            H_0[ii,jj] <- H_0[ii,jj] + t(d2y[kk,,] %*% plsr.input$loading.weights[ii,]) %*% (plsr.input$loading.weights[jj,]) * Q[kk]
        }  
      }
    }
    
    betas_interact_matrix[i,,] <- H_0
    
    x_zeros_vect <- matrix(0, 1, dim(x_test)[2])
    intercept <- mean(model.PLS %>% predict( x_zeros_vect ))
    
    
    DPLS.stocks <- matrix(0, dim(x_test_original)[1], 49)
    for (k in 1:49){
      DPLS.stocks[,k] <- x_test_original[,k] * J_0[k] + (0.5 * x_test_original[,k]^2 * H_0[k,k])
    }
    
    sorted = sort(Y_hat_oos, index.return=TRUE, decreasing=TRUE)
    
    new_rows <- data.frame(factors_cat, colMeans(DPLS.stocks[sorted$ix[1:portfolio_size],]), rep(dates[t_0+i],49))
    new_rows <- aggregate(new_rows[,2], list(new_rows$factors_cat), FUN=sum)
    
    # We add the date to the dataframe
    new_rows$date <- rep(dates[t_0+i],7) #7 because there are 7 factor categories
    
    w <- array(0, length(Y_hat_oos))
    w[sorted$ix[1:portfolio_size]] <- 1/portfolio_size
    
    interaction_matrix <- 0.5 * x_test_original %*% (H_0 - diag(diag(H_0))) %*% t(x_test_original)
    interaction <- t(w) %*% interaction_matrix %*% w
    
    HOT <- mean(Y_hat_oos[sorted$ix[1:portfolio_size]]) - (interaction + intercept + sum(new_rows$x))
    residual <- mean( Y_hat_oos[sorted$ix[1:portfolio_size]] - y_test[sorted$ix[1:portfolio_size]] )
    
    df.attribution_DPLS <- rbind(df.attribution_DPLS,new_rows)
    df.attribution_DPLS <- rbind(df.attribution_DPLS, data.frame(Group.1="Intercept", x=intercept, date=dates[t_0+i]))
    df.attribution_DPLS <- rbind(df.attribution_DPLS, data.frame(Group.1="Interaction", x=interaction, date=dates[t_0+i]))
    df.attribution_DPLS <- rbind(df.attribution_DPLS, data.frame(Group.1="H.O.T", x=HOT, date=dates[t_0+i]))
    df.attribution_DPLS <- rbind(df.attribution_DPLS, data.frame(Group.1="Residual", x=residual, date=dates[t_0+i]))
    
    return_attribution_DPLS <- append(return_attribution_DPLS, mean(y_test[sorted$ix[1:portfolio_size]]))
    return_attribution_predicted_DPLS <- append(return_attribution_predicted_DPLS, mean(Y_hat_oos[sorted$ix[1:portfolio_size]]))
    
    
  }
}
names(df.attribution_DPLS) = c("factors", "values", "date")


attribution_plot(t_0, T, df.attribution_DPLS, return_attribution_DPLS, return_attribution_predicted_DPLS, dates)

ymax <- 1.6#max(df.return$return)
ymin <- -1#min(df.return$return)
yticks <- seq(ymin, ymax, 0.2)
yticks_label <- paste(as.character(seq(ymin*100, ymax*100, 20) ), "%", sep=" ")

df.return_real <- data.frame(return=as.numeric(return_attribution_DPLS), dates=dates[seq(t_0+1, t_0+T)], type = "Observed")
df.return_predicted <- data.frame(return=as.numeric(return_attribution_predicted_DPLS), dates=dates[seq(t_0+1, t_0+T)], type = "Predicted")
df.return <- rbind(df.return_real, df.return_predicted)

#ggplot(df.attribution_DPLS[((df.attribution_DPLS$date > dates[t_0]) & (df.attribution_DPLS$date < dates[t_0+T])), ]) + 
ggplot(df.attribution_DPLS) + 
  geom_bar(aes(x=date, y=values, fill=factors), stat="identity", position = "stack")+
  #scale_fill_discrete(name="Factors") +
  
  geom_line(data=df.return,aes(x=dates,y=return, color=type, linetype = type),size=0.4) +
  
  #geom_vline(xintercept=as.numeric(dates[112]), size=0.5, color="black") +
  #geom_vline(xintercept=as.numeric(dates[83]), size=0.5, color="blue") +
  #geom_vline(xintercept=as.numeric(dates[226]), size=0.5, color="red") +
  
  scale_colour_manual(name="Portfolio Returns (10)", values=c("blue", "red")) + 
  scale_linetype_manual(name="Portfolio Returns (10)", values=c("solid", "dashed"))+
  scale_fill_brewer(name="Factors", palette="Set3", direction=-1) + ylim(-1,1)
  #scale_y_continuous("Excess Return", breaks = yticks, labels=yticks_label, limits=c(ymin,ymax)) + 
  scale_x_date("Date", date_breaks = "1 year", date_minor_breaks = "6 month")


ggsave(file="figure/attribution_U_hat_fix_lim.png", width=16.25, height=6.5, dpi=800)






betas_matrix_median <- apply(betas_matrix, 2, mean)

df.betas <- data.frame(value=(betas_matrix_median), factor=factors_names)
df.betas <- df.betas[order(-df.betas$value),]
rownames(df.betas) <- seq(1,49)

ggplot(df.betas, aes(x=reorder(factor, value), y = value)) + geom_bar(stat = "identity", fill="dodgerblue1") + 
  labs(x="Factor", y="Sensitivity") + coord_flip() 
ggsave(file="figure/sensitivity_DPLS_mean.png", width=5.5, height=7, dpi=800)



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

num_to_plot = 30

df.interaction_to_plot <- rbind(df.interactions_all_sorted[1:num_to_plot,], df.interactions_all_sorted[((length(interaction_names)-num_to_plot):length(interaction_names)),])

ggplot(df.interaction_to_plot, aes(x=reorder(factor, value), y = value)) + geom_bar(stat = "identity", fill="dodgerblue1") + 
  labs(x="Factor", y="Sensitivity") + coord_flip()
ggsave(file="figure/interaction_DPLS_mean.png", width=5.5, height=7, dpi=800)






attribution_plot <- function(t_0, T, df.attribution, return_observed, return_predicted, dates){
  ymax <- 1.6#max(df.return$return)
  ymin <- -1#min(df.return$return)
  yticks <- seq(ymin, ymax, 0.2)
  yticks_label <- paste(as.character(seq(ymin*100, ymax*100, 20) ), "%", sep=" ")
  
  df.return_real <- data.frame(return=return_observed, dates=dates[seq(t_0, t_0+T)], type = "Observed")
  df.return_predicted <- data.frame(return=return_predicted, dates=dates[seq(t_0, t_0+T)], type = "Predicted")
  df.return <- rbind(df.return_real, df.return_predicted)
  
  ggplot(df.attribution[((df.attribution$date > dates[t_0]) & (df.attribution$date < dates[t_0+T])), ]) + 
    geom_bar(aes(x=date, y=values, fill=factors), stat="identity", position = "stack")+
    scale_fill_discrete(name="Factors") +
    
    geom_line(data=df.return[((df.return$date > dates[t_0]) & (df.return$date < dates[t_0+T])), ], 
              aes(x=dates,y=return, color=type, linetype = type),size=0.4) +
    
    #geom_vline(xintercept=as.numeric(dates[112]), size=0.5, color="black") +
    #geom_vline(xintercept=as.numeric(dates[83]), size=0.5, color="blue") +
    #geom_vline(xintercept=as.numeric(dates[226]), size=0.5, color="red") +
    
    scale_colour_manual(name="Portfolio Returns (10)", values=c("blue", "red")) + 
    scale_linetype_manual(name="Portfolio Returns (10)", values=c("solid", "dashed"))+
    scale_fill_brewer(name="Factors", palette="Set3", direction=-1) + 
    scale_y_continuous("Excess Return", breaks = yticks, labels=yticks_label, limits=c(ymin,ymax)) + 
    scale_x_date("Date", date_breaks = "1 year", date_minor_breaks = "6 month")
  ggsave(file="figure/attribution.png", width=16.25, height=6.5, dpi=800)
}