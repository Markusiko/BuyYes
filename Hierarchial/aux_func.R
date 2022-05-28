
estimations_metrics <- function (df, posterior, k_1, k_2) {
  MSE_beta1 = c()
  MSE_beta2 = c()
  MAPE_beta1 = c()
  MAPE_beta2 = c()
  
  beta = c(colMeans(posterior$beta1), colMeans(posterior$beta2))
  rho = mean(posterior$rho)
  
  for (i in 1:k_1) {
    MSE_beta1 = c(MSE_beta1, rmse(beta_1[i], posterior$beta1[, i]))
    MAPE_beta1 = c(MAPE_beta1, mape(beta_1[i], posterior$beta1[, i]))
  }
  
  for (i in 1:k_2) {
    MSE_beta2 = c(MSE_beta2, rmse(beta_2[i], posterior$beta2[, i]))
    MAPE_beta2 = c(MAPE_beta2, mape(beta_2[i], posterior$beta2[, i]))
  }
  
  MSE_rho = rmse(sigma[1,2] / sqrt((sigma[1,1] * sigma[2,2])), rho)
  MAPE_rho = mape(sigma[1,2] / sqrt((sigma[1,1] * sigma[2,2])), rho)
  
  df <- rbind(df, c(beta, rho, 
                    MSE_beta1, MSE_beta2, MSE_rho, 
                    MAPE_beta1, MAPE_beta2, MAPE_rho))
  return(df)
}


generate_sample <- function (N){
  x_11 <- runif(n=N, min=0, max=2)
  
  X_1 <- cbind(1, x_11)
  beta_1 <- matrix(c(2, -2), nrow=2)
  
  mu <- c(0, 0)             
  sigma <- matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2)
  epsilon <- mvrnorm(n = N, mu = mu, Sigma = sigma)
  
  epsilon_1 <- epsilon[, 1]
  epsilon_2 <- epsilon[, 2]
  
  y_star_1 <- as.vector(X_1 %*% beta_1 + epsilon_1) > 0 
  
  X_2 <- cbind(1, x_11, y_star_1)
  beta_2 <- matrix(c(-1, 2, -2), nrow=3)  
  y_star_2 <- as.vector(X_2 %*% beta_2 + epsilon_2) > 0
  
  y <- cbind(y_star_1, y_star_2)
  
  return(list(X_1=X_1, X_2=X_2, y_star=y))
}