
library("rstan")
library('Metrics')
library('MASS')
source("/Users/User/Desktop/hier/aux_func.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(999)

# --------------------------
# Иерархическая модель:
# y_1 = [b_10 + b_11 * x_11  > 0]
# y_2 = [b_20 + b_21 * x_11 + b_22 * y1 > 0]
# --------------------------

N <- 1000                                 # число наблюдений

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

y_star <- cbind(y_star_1, y_star_2)
colMeans(y_star) # следим, чтобы было по 0.5
# Процесс генерации записали в функцию generate_sample
# --------------------------

# Stan модель
# --------------------------

rnames = c("b_10","b_11", "b_20", "b_21", "b_22", "rho",
           "MSE_b_10", "MSE_b_11", "MSE_b_20", "MSE_b_21", "MSE_b_22", "MSE_rho",
           "MAPE_b_10", "MAPE_b_11", "MAPE_b_20", "MAPE_b_21", "MAPE_b_22", "MAPE_rho")

df <- data.frame(t(rep(0, length(rnames))))
names(df) <- rnames
df = df[FALSE,]

N <- 100
n_iters <- 100 # число итераций

# Начнем прогоны!
for (i in 1:n_iters){
  
  sample <- generate_sample(N)
  X_1 <- sample$X_1
  X_2 <- sample$X_2
  y_star <- sample$y_star
  
  data <- list(N=N,
               K1 = dim(X_1)[2],
               K2 = dim(X_2)[2],
               Y=y_star,
               X1 = X_1,
               X2=X_2
  )
  
  model <- stan(file = "/Users/User/Desktop/hier/hierarchial.stan",
                data = data,               
                chains = 1,                
                iter = 2000)    
  posterior <- extract(model)
  
  
  df <- estimations_metrics(df=df, posterior=posterior, k_1=dim(X_1)[2], k_2=dim(X_2)[2])
  
}  
names(df) <- rnames

# --------------------------

# Смотрим на результаты
# --------------------------
df
write.csv(df, file = "/Users/User/Desktop/hier/estimates_hier_bayes_1000.csv")
