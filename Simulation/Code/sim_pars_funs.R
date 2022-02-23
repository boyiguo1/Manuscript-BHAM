
# Fixed Parameters --------------------------------------------------------
# * Basic Parameters --------------------------------------------------------

K <- 10               # Smoothing function degrees
n_train <- 500        # Training set sample size
n_test <- 10000       # Testing set sample size

bamlasso_seq <- bgam_seq <- seq(0.001, 0.05, length.out = 20)


# 
# if(fam_fun$family == "binomial"){
#   bamlasso_seq <- case_when(
#     p == 4 ~ seq(from = 0.01, to = 0.8, length.out = 20),
#     p == 10 ~ seq(0.01, 0.3, length.out = 20),
#     p == 50 ~  seq(0.005, 0.1, length.out = 20),
#     p == 100 ~ seq(0.005, 0.10, by = 0.005),
#     p == 200 ~ seq(0.001, 0.03, length.out = 20)
#   )
#   
#   bgam_seq <- case_when(
#     p == 4 ~ seq(from = 0.005, to = 0.1, length.out = 20),
#     p == 10 ~ seq(from = 0.005, to = 0.1, length.out = 20),
#     p == 50 ~ seq(0.005, 0.05, length.out = 20),
#     p == 100 ~ seq(0.005, 0.05, length.out = 20),
#     p == 200 ~ seq(0.005, 0.02, length.out = 20)
#   )
#   
#   
#   
# } else if(fam_fun$family == "gaussian"){
#   bamlasso_seq <- case_when(
#     p == 4 ~ seq(from = 0.08, to = 0.25, length.out = 20),
#     p == 10 ~ seq(0.005, 0.03, length.out = 20),
#     p == 50 ~ seq(0.0005, 0.01, length.out = 20),
#     p == 100 ~ seq(0.0005, 0.0075, length.out = 20),
#     p == 200 ~ seq(0.00025, to=0.0025, length.out = 20)
#   )
#   
#   bgam_seq <- case_when(
#     p == 4 ~ seq(from = 0.05, to = 0.1, length.out = 20),
#     p == 10 ~ seq(from = 0.005, to = 0.1, length.out = 20),
#     p == 50 ~ seq(0.01, 0.06, length.out = 20),
#     p == 100 ~ seq(0.01, 0.075, length.out = 20),
#     p == 200 ~ seq(0.015, 0.05, length.out = 20)
#   )
#   
# } else {
#   stop("seq is not specified yet")
# }


# Functions ---------------------------------------------------


