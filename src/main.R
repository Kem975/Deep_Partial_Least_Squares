set.seed(10)
library(keras)
library(tensorflow)
set_random_seed(10)
library(pls)
library(corrplot)
library(ggplot2)
library(latex2exp)
library(glmnet)
library("covFactorModel")
library("xts")

source("get_data.R")
source("train_DPLS.R")
source("train_LASSO.R")
source("train_NN.R")
source("train_PLS.R")
source("MSE.R")
source("R2.R")
source("IR.R")
source("L_infty.R")
source("table.R")
source("num_components.R")
source("plot_sensitivities.R")


df.data <- read.csv("../data/Data.csv")

dates <- unique(as.Date(df.data$date))
dates <- na.omit(dates)
df.data$X <- c()


list.data <- get_all_data(df.data, dates) # Rescaling of the data
Xs_train <- list.data[[1]]
Ys_train <- list.data[[2]]
Xs_test <- list.data[[3]]
Ys_test <- list.data[[4]]


### --------------- PARAMETERS --------------- ###

t_0 <- 1 # Initial period (max=331)
T <- 330 # Number of periods (max=331)


### --------------- LASSO TRAINING --------------- ###

list.LASSO <- train_LASSO(t_0, T, Xs_train, Xs_test)
Yhat_LASSO_oos_list <- list.LASSO[[1]]
Yhat_LASSO_is_list <- list.LASSO[[2]]


### --------------- PLS TRAINING --------------- ###

list.PLS <- train_PLS(t_0, T, Xs_train, Xs_test)
Y_hat_PLS_is_list <- list.PLS[[1]]
Y_hat_PLS_oos_list <- list.PLS[[2]]
comp_num_list_PLS <- list.PLS[[3]]

### --------------- NN LOADING --------------- ###

list.NN <- load_NN(t_0, T, Xs_train, Xs_test, dates, bRetrain=FALSE, bSensitivities=FALSE)
Y_hat_is_list_NN <- list.NN[[1]]
Y_hat_oos_list_NN <- list.NN[[2]]
betas_NN <- list.NN[[3]]
betas_interact_NN <- list.NN[[4]]


### --------------- DPLS LOADING --------------- ###

list.DPLS <-load_DPLS(t_0, T, Xs_train, Xs_test, dates, bRetrain=FALSE, bSensitivities=FALSE)
Uhat_oos_list <- list.DPLS[[1]]
Uhat_is_list <- list.DPLS[[2]]
Y_hat_oos_list <- list.DPLS[[3]]
Y_hat_is_list <- list.DPLS[[4]]
betas_DPLS <- list.DPLS[[5]]
betas_interact_DPLS <- list.DPLS[[6]]
comp.num_list <- list.DPLS[[7]]


#comp.num_list_input <- list.DPLS[[8]]


### --------------- PLOT Number of Components --------------- ###

num_components(t_0, T, comp.num_list, dates)


### --------------- PLOT Sensitivities --------------- ###

plot_sensitivities(T, betas_NN, "NN_")
plot_sensitivities(T, betas_DPLS, "DPLS_")

plot_interaction(T, betas_interact_NN, num_to_plot= 30, "NN_")
plot_interaction(T, betas_interact_DPLS, num_to_plot= 30, "DPLS_")


### --------------- PLOT L_infinity --------------- ###

L_infty_is(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, Ys_train, dates)
L_infty_oos(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test, dates)


### --------------- PLOT MSE --------------- ###

MSE_is(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, Ys_train, dates)
MSE_oos(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test, dates)


### --------------- PLOT R² --------------- ###

r2_is(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, Ys_train, dates)
r2_oos(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test, dates)


### --------------- Information Ratio  --------------- ###

IR(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test)


### --------------- Table  --------------- ###
# Calculate the Table 1 of the paper

Ks=c(1,2,3,4,5,6,7,8,9,10)

table.DPLS(c(0), T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, y_train_list, y_test_list, )

table.DPLS(Ks, T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list)
table.PLS(Ks, T, t_0, port_size=40, Xs_train, Xs_test,  comp_num_list_PLS, bTest=FALSE, bPortfolio=FALSE)
table.PLS(Ks, 330,  t_0, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=FALSE, bPortfolio=TRUE)
table.PLS(Ks, 330, 1, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=TRUE, bPortfolio=FALSE)
table.PLS(Ks, 330, 1, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=TRUE, bPortfolio=TRUE)

df.table <- table(Ks, 330, t_0=1, port_size=40, df.data, 
                  Uhat_is_list, Uhat_oos_list, Xs_train, Xs_test, Ys_train, Ys_test, comp.num_list)


table.PCA(Ks, 330, 1, port_size=40, df.data, dates, bPortfolio=FALSE)
table.PCA(Ks, 330, 1, port_size=40, df.data, dates, bPortfolio=TRUE)








################## LITTLE TESTS TO MAKE THE TABLE WORK #####################


Ks=1:40

MSE <- rep(0,length(Ks))

fit <- plsr(formula = xs_return~., ncomp = Ks[length(Ks)], data=train, rescale = F, validation="LOO")

cverr <- plot(RMSEP(plsr.fit)$val[1,,])
comp.num_CV <- which.min(cverr)

#comp.num_input <- comp.num_CV
#if (comp.num_CV<10){
#  comp.num_input <- 10
#}
#comp_input_list[i] <- comp.num_input

for (k in 1:length(Ks)){
  Y_hat <- predict(fit, ncomp = Ks[k], newdata = test)[,1,]
  Y<-test[,50]
  MSE[k] <- mean((Y_hat-Y)^2)
}

K_star <- which.min(MSE)
plot(MSE, xlab = "K")


model.PLS <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "softplus", input_shape = 40, kernel_initializer='normal') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 40, kernel_initializer='normal') 
#summary(model.PLS)

model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")


# First approach

train = Xs_train[[2]]
validation = Xs_train[[3]]
test = Xs_test[[3]]

fit.train <- plsr(formula = xs_return~., ncomp = 40, data=train, rescale = F)

x_train <- as.matrix(train[,1:49]) %*% drop(fit.train$loading.weights)
y_train <- train$xs_return

x_validation <- as.matrix(validation[,1:49]) %*% drop(fit.train$loading.weights)
y_validation <- validation$xs_return

U_train <- as.matrix(train[,1:49]) %*% drop(fit.train$coefficients)

model.PLS <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "softplus", input_shape = 40, kernel_initializer='normal') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 40, kernel_initializer='normal') 

model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")
model.PLS %>% fit( x = x_train, y = U_train, verbose = 0, 
                   epochs=150,
                   callbacks = list(callback_early_stopping(monitor = "loss", 
                                                            patience = 30, 
                                                            min_delta = 1e-6, restore_best_weights = TRUE)))

U_hat_is = model.PLS %>% predict( x_train )
U_hat_validation = model.PLS %>% predict( x_validation )

MSEs <- rep(0,40)
MSEs[1] <- mean((U_hat_validation[,1] - y_validation)^2)
for (i in 2:40){
  MSEs[i] <- mean((rowMeans(U_hat_validation[,1:i]) - y_validation)^2)
}
plot(MSEs, xlab = "K")
K_star <- which.min(MSEs)

fit.validation <- plsr(formula = xs_return~., ncomp = 40, data=validation, rescale = F)

x_validation <- as.matrix(validation[,1:49]) %*% drop(fit.validation$loading.weights)
y_validation <- validation$xs_return

x_test <- as.matrix(test[,1:49]) %*% drop(fit.validation$loading.weights)
y_test <- test$xs_return

U_validation <- as.matrix(validation[,1:49]) %*% drop(fit.validation$coefficients)

model.PLS <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "softplus", input_shape = 40, kernel_initializer='normal') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 40, kernel_initializer='normal') 

model.PLS %>% compile(loss = "mse", optimizer = "RMSprop")
model.PLS %>% fit( x = x_validation, y = U_validation, verbose = 0, 
                   epochs=150,
                   callbacks = list(callback_early_stopping(monitor = "loss", 
                                                            patience = 30, 
                                                            min_delta = 1e-6, restore_best_weights = TRUE)))


U_hat_validation = model.PLS %>% predict( x_validation )
U_hat_oos = model.PLS %>% predict( x_test )


MSE_test <- rep(0,40)
MSE_test[1] <- mean((U_hat_oos[,1] - y_test)^2)
for (i in 2:40){
  MSE_test[i] <- mean((rowMeans(U_hat_oos[,1:i]) - y_test)^2)
}
plot(MSE_test, xlab = "K")





# Second approach

validation = Xs_train[[2]]
train = Xs_train[[3]]
test = Xs_test[[3]]

fit.train <- plsr(formula = xs_return~., ncomp = 40, data=train, rescale = F)

x_train <- as.matrix(train[,1:49]) %*% drop(fit.train$loading.weights)
y_train <- train$xs_return

x_validation <- as.matrix(validation[,1:49]) %*% drop(fit.train$loading.weights)
y_validation <- validation$xs_return

x_test <- as.matrix(test[,1:49]) %*% drop(fit.train$loading.weights)
y_test <- test$xs_return

U_train <- as.matrix(train[,1:49]) %*% drop(fit.train$coefficients)

model.PLS %>% fit( x = x_train, y = U_train, verbose = 0, 
                   epochs=150,
                   callbacks = list(callback_early_stopping(monitor = "loss", 
                                                            patience = 30, 
                                                            min_delta = 1e-6, restore_best_weights = TRUE)))

U_hat_is = model.PLS %>% predict( x_train )
U_hat_validation = model.PLS %>% predict( x_validation )
U_hat_oos = model.PLS %>% predict( x_test )

MSEs <- rep(0,40)
MSEs[1] <- mean((U_hat_validation[,1] - y_validation)^2)
for (i in 2:40){
  MSEs[i] <- mean((rowMeans(U_hat_validation[,1:i]) - y_validation)^2)
}
plot(MSEs, xlab = "K")
K_star <- which.min(MSEs)


MSE_test <- rep(0,40)
MSE_test[1] <- mean((U_hat_oos[,1] - y_test)^2)
for (i in 2:40){
  MSE_test[i] <- mean((rowMeans(U_hat_oos[,1:i]) - y_test)^2)
}
plot(MSE_test, xlab = "K")
