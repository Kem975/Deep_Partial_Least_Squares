# Get the risk plot

df.histo_dicrete_DPLS_40 <- data.frame()
return_histo_predicted_DPLS_40 <- c()
return_histo_DPLS_40 <- c() 
max_k <- 49
t_0 <- 211
T <- 120
for (i in 1:T){ #(length(dates)-1)
  # Get the Data
  train = Xs_train[[t_0+i]]
  test = Xs_test[[t_0+i]]
  
  x_train_original <- as.matrix(subset(train, select = -c(xs_return))) 
  y_train_original <- train$xs_return
  
  x_test_original <- as.matrix(subset(test, select = -c(xs_return))) 
  y_test_original <- test$xs_return
  
  plsr.input <- plsr(formula = xs_return~., data=train, rescale = F)
  PLS_Score.input <- scores(plsr.input)
  
  x_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$loading.weights)
  y_train <- train$xs_return
  
  x_test <- as.matrix(test[,1:49]) %*% drop(plsr.input$loading.weights) 
  y_test <- test$xs_return
  
  U_train <- as.matrix(train[,1:49]) %*% drop(plsr.input$coefficients)
  
  #model.PLS <- load_model_tf(sprintf("Models/%s.h5", dates[i]))
  model.PLS <- keras_model_sequential() %>%
    layer_dense(units = 100, activation = "softplus", input_shape = 49, kernel_initializer='normal') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 49, kernel_initializer='normal') 
  #summary(model.PLS)
  
  model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")
  model.PLS %>% fit( x = x_train, y = U_train, verbose = 0, 
                     epochs=100,
                     callbacks = list(callback_early_stopping(monitor = "loss", 
                                                              patience = 30, 
                                                              min_delta = 1e-4))) #restore_best_weights = TRUE
  
  #### Test
  U_hat_is <- model.PLS %>% predict( x_train )
  U_hat_oos <- model.PLS %>% predict( x_test )
  
  MSEs <- rep(0,max_k)
  MSEs[1] <- mean((U_hat_oos[,1] - y_test)^2)
  for (K in 2:max_k){
    MSEs[K] <- mean((rowMeans(U_hat_oos[,1:K]) - y_test)^2)
  }
  comp.num_CV <- which.min(round(MSEs, 6))
  
  if (comp.num_CV<4){
    comp.num_CV <- 4
  }
  
  Y_hat_is <- rowMeans(U_hat_is[,1:comp.num_CV])
  Y_hat_oos <- rowMeans(U_hat_oos[,1:comp.num_CV])
  
  idx <- order(Y_hat_is)[length(Y_hat_is):1]
  w <- rep(0,length(Y_hat_is))
  w[idx[1:10]] <- 0.1
  
  
  x_zeros_vect <- matrix(0, 1, dim(x_test)[2])
  intercept <- mean(model.PLS %>% predict( x_zeros_vect ))
  
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
  
  
  J_0 <- colMeans(drop(as.array(dy_tf))[1:comp.num_CV,])
  
  d2y <- drop(as.array(d2y_tf))
  H_0 <- matrix(0, nrow=49, ncol=49)
  for (fact in 1:comp.num_CV){
    H_0 <- H_0 + d2y[fact,,]
  }
  H_0 <- H_0/comp.num_CV
  
  x_score <- x_train
  
  DPLS.stocks <- matrix(0, comp.num_CV, 1)
  
  for (k in 1:comp.num_CV){
    DPLS.stocks[k] <- w %*% (x_score[,k] * J_0[k])
  } 
  
  risk_factors_names <- c("Risk Factor 1", "Risk Factor 2", "Risk Factor 3", rep("Other Risk Factors", comp.num_CV-3))
  risk_values_iteration <- data.frame(factors=risk_factors_names,
                                      values=DPLS.stocks,
                                      date=rep(dates[t_0+i],comp.num_CV))
  
  
  quadratic<- 0.5 * w %*% x_score %*% (H_0) %*% t(x_score) %*% w
  
  HOT <- (w %*% Y_hat_is) - (quadratic + intercept + sum(risk_values_iteration$values))
  
  df.histo_dicrete_DPLS_40 <- rbind(df.histo_dicrete_DPLS_40, risk_values_iteration)
  df.histo_dicrete_DPLS_40 <- rbind(df.histo_dicrete_DPLS_40, data.frame(factors="Intercept", values=as.numeric(intercept), date=dates[t_0+i]))
  df.histo_dicrete_DPLS_40 <- rbind(df.histo_dicrete_DPLS_40, data.frame(factors="Quadratic", values=quadratic, date=dates[t_0+i]))
  df.histo_dicrete_DPLS_40 <- rbind(df.histo_dicrete_DPLS_40, data.frame(factors="H.O.T", values= as.numeric(HOT), date=dates[t_0+i]))
  
  return_histo_DPLS_40 <- append(return_histo_DPLS_40, as.numeric(w %*% y_train))
  return_histo_predicted_DPLS_40 <- append(return_histo_predicted_DPLS_40, as.numeric(w %*% Y_hat_is))

}
names(df.histo_dicrete_DPLS_40) = c("factors", "values", "date")


mean_HOT <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "H.O.T", ]$values)
mean_intercept <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Intercept", ]$values)
mean_quadratic <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Quadratic", ]$values)

mean_factor_1 <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Risk Factor 1", ]$values)
mean_factor_2 <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Risk Factor 2", ]$values)
mean_factor_3 <- mean(df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Risk Factor 3", ]$values)


df_other_factors <- df.histo_dicrete_DPLS_40[ df.histo_dicrete_DPLS_40$factors == "Other Risk Factors", ]
tmp <- aggregate(df_other_factors[,2], list(df_other_factors$date), FUN=sum)
mean_factor_other <- mean(tmp$x)
#mean_factor_other <- mean(df.histo_dicrete_DPLS_40[[1]][ df.histo_dicrete_DPLS_40[[1]]$factors == "Other Risk Factors", ]$values)



label_factor_HOT <- sprintf("H.O.T (%.3f)", mean_HOT)
label_factor_intercept <- sprintf("Intercept (%.3f)", mean_intercept)
label_factor_quadratic <- sprintf("Quadratic (%.3f)", mean_quadratic)

label_factor_1 <- sprintf("Risk Factor 1 (%.3f)", mean_factor_1)
label_factor_2 <- sprintf("Risk Factor 2 (%.3f)", mean_factor_2)
label_factor_3 <- sprintf("Risk Factor 3 (%.3f)", mean_factor_3)
label_factor_other <- sprintf("Other Risk Factors (%.3f)", mean_factor_other)

df_observed_excess_return <- data.frame(return=return_histo_DPLS_40, dates=dates[seq(t_0+1,t_0+T)], type="Observed")
df_predicted_excess_return <- data.frame(return=return_histo_predicted_DPLS_40, dates=dates[seq(t_0+1,t_0+T)], type="Predicted")
df_excess_return <- rbind(df_observed_excess_return, df_predicted_excess_return)

ggplot(df.histo_dicrete_DPLS_40) + 
  geom_bar(aes(x=date, y=values, fill=factors), stat="identity", position = "stack")+
  geom_line(data=df_excess_return, aes(x=dates,y=return, color=type, linetype=type),size=0.4) + 
  scale_fill_discrete(name="Latent Risk Factors", labels = c(label_factor_HOT, label_factor_intercept, label_factor_other, label_factor_quadratic, label_factor_1, label_factor_2, label_factor_3)) +
  scale_colour_manual(name="Monthly Portfolio Returns (10)", values=c("black", "blue"))+
  scale_linetype_manual(name="Monthly Portfolio Returns (10)", values=c("dashed", "solid"))+
  scale_fill_brewer(name="Latent Risk Factors", labels = c(label_factor_HOT, label_factor_intercept, label_factor_other, label_factor_quadratic, label_factor_1, label_factor_2, label_factor_3), palette="Set3", direction=-1) +
  #scale_y_continuous("Monthly Portfolio Return") + 
  scale_x_date("Date")#, date_breaks = "1 year", date_minor_breaks = "6 month")

ggsave(file="figure/pdf/Risk_factors_DPLS.pdf", width=16.25, height=6.5, dpi=800)
ggsave(file="figure/png/Risk_factors_DPLS.png", width=16.25, height=6.5)









