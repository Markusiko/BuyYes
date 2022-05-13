
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
  
  x_11 <- runif(n=N, min=0, max=2)    # Генерируем регрессоры из равномерного распределения с одинаковой дисперсией 
  x_12 <- runif(n=N, min=1, max=3)  
  
  x_21 <- runif(n = N, min = 2, max = 4) 
  x_22 <- runif(n = N, min = -1, max = 1) 
  x_23 <- runif(n = N, min = 1, max = 3) 
  
  X_1 <- cbind(1, x_11, x_12)               # Агрегируем все регрессоры => получаем реализацию выборки
  X_2 <- cbind(1, x_21, x_22, x_23)
  
  beta_1 <- matrix(c(3, 2, -2.5), nrow=3)      # Задаем реальные значения b_0, b_1, b_2 для каждого уравнения системы
  
  beta_2 <- matrix(c(2, 1, -2, -2.5), nrow=4)  # Cледим, чтобы классы были сбалансированы
  
  mu <- c(0, 0)                             # Генерируем ошибки из двумерного нормального распределения и задаем коэфф. корреляции                
  sigma <- matrix(c(1, 0.5, 0.5, 1), 
                  nrow=2, ncol=2)
  epsilon <- mvrnorm(n = N, 
                     mu = mu, Sigma = sigma)
  
  epsilon_1 <- epsilon[, 1]
  epsilon_2 <- epsilon[, 2]
  
  y_star_1 <- as.vector(X_1 %*% beta_1 + epsilon_1) > 0        # Находим вектор у
  y_star_2 <- as.vector(X_2 %*% beta_2 + epsilon_2) > 0
  
  y <- cbind(y_star_1, y_star_2)
  
  return(list(X_1=X_1, X_2=X_2, y_star=y))
}