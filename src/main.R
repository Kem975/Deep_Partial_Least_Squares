list.of.packages <- c("keras", "Rcpp", "devtools", "tensorflow", "pls", "xts", "corrplot", "latex2exp", "ggplot2", "glmnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(keras)
require(tensorflow)
require(pls)
require(corrplot)
require(ggplot2)
require(latex2exp)
require(glmnet)
require(devtools)
devtools::install_github("dppalomar/covFactorModel")
require("covFactorModel")
require("xts")

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

set_random_seed(10)
# download this data from
# https://www.dropbox.com/s/5l6ufcxosc7pv6l/Data.csv?dl=0
#df.data <- read.csv("../data/Data.csv")

#dates <- unique(as.Date(df.data$date))
#dates <- na.omit(dates)
df.data <- c()
df.data$X <- c()

# This next line takes approximately ten minutes to execute
# list.data <- get_all_data(df.data, dates) # Rescaling of the data
list.data<-readRDS("../data/ScaledData.RData")

Xs_train <- list.data[[1]]
Ys_train <- list.data[[2]]
Xs_test  <- list.data[[3]]
Ys_test  <- list.data[[4]]
dates    <- list.data[[5]]
# This next line takes approximately ten minutes to execute
# list.data <- get_all_data(df.data, dates) # Rescaling of the data



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


### --------------- PLOT R? --------------- ###

r2_is(t_0, T, Y_hat_is_list, Yhat_LASSO_is_list, Y_hat_is_list_NN, Y_hat_PLS_is_list, Ys_train, dates)
r2_oos(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test, dates)


### --------------- Information Ratio  --------------- ###

IR(t_0, T, Y_hat_oos_list, Yhat_LASSO_oos_list, Y_hat_oos_list_NN, Y_hat_PLS_oos_list, Ys_test)


### --------------- Table  --------------- ###
# Calculate the Table 1 of the paper

Ks=c(1,2,3,4,5,6,7,8,9,10)


table.DPLS(Ks, T, t_0, port_size=40, Uhat_is_list, Uhat_oos_list, Ys_train, Ys_test, comp.num_list)
table.PLS(Ks, T, t_0, port_size=40, Xs_train, Xs_test,  comp_num_list_PLS, bTest=FALSE, bPortfolio=FALSE)
table.PLS(Ks, 330,  t_0, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=FALSE, bPortfolio=TRUE)
table.PLS(Ks, 330, 1, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=TRUE, bPortfolio=FALSE)
table.PLS(Ks, 330, 1, port_size=40, Xs_train, Xs_test, comp_num_list_PLS, bTest=TRUE, bPortfolio=TRUE)

df.table <- table(Ks, 330, t_0=1, port_size=40, df.data, 
                  Uhat_is_list, Uhat_oos_list, Xs_train, Xs_test, Ys_train, Ys_test, comp.num_list)


table.PCA(Ks, 330, 1, port_size=40, df.data, dates, bPortfolio=FALSE)
table.PCA(Ks, 330, 1, port_size=40, df.data, dates, bPortfolio=TRUE)



