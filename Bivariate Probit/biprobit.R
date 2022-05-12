# основу взял тут:
# https://github.com/elineii/Bayes
# спасибо Алине (❤)

library("rstan")
library('Metrics')
library('MASS')
source("/Users/markymark/Desktop/Статистика/bivariate/au_functions.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(999)

# --------------------------
# ШАГ 1. Генерируем данные 
# Наша модель устроена так:
# y_1 = [b_10 + b_11 * x_11 + b_12 * x_12 > 0]
# y_2 = [b_20 + b_21 * x_21 + b_22 * x_22 + b_23 * x_23 > 0]
# --------------------------

N <- 1000                                 # Число наблюдений
n_iters <- 100                            # Число симуляций

x_11 <- runif(n=N, min=0, max=2)    # Генерируем регрессоры из равномерного распределения 
x_12 <- runif(n=N, min=1, max=3)  

x_21 <- runif(n = N, min = 2, max = 4) 
x_22 <- runif(n = N, min = -1, max = 1) 
x_23 <- runif(n = N, min = 1, max = 3) 

X_1 <- cbind(1, x_11, x_12)               # Аггрегируем все регрессоры => получаем реализацию выборки
X_2 <- cbind(1, x_21, x_22, x_23)

beta_1 <- matrix(c(3, 2, -2.5), nrow=3)      # Задаем реальные значения b_0, b_1, b_2 для каждого уравнения системы

beta_2 <- matrix(c(2, 1, -2, -2.5), nrow=4)  # следим, чтобы классы были сбалансированы

mu <- c(0, 0)             # Генерируем ошибки из двумерного нормального распределения и задаем коэфф. корреляции                
sigma <- matrix(c(1, 0.5, 0.5, 1), 
                nrow=2, ncol=2)
epsilon <- mvrnorm(n = N, 
                   mu = mu, Sigma = sigma)

epsilon_1 <- epsilon[, 1]
epsilon_2 <- epsilon[, 2]

y_star_1 <- as.vector(X_1 %*% beta_1 + epsilon_1) > 0        # Находим вектор у
y_star_2 <- as.vector(X_2 %*% beta_2 + epsilon_2) > 0

y_star <- cbind(y_star_1, y_star_2)
colMeans(y_star) # 0.507 и 0.505  - доли единиц
# эти наши действия запишем в функцию generate_sample
# --------------------------

# ШАГ 2. Оцениваем параметры модели
# --------------------------
# Формируем данные для модели

rnames = c("b_10","b_11", "b_12", "b_20", "b_21", "b_22", "b_23", "rho",
           "MSE_b_10", "MSE_b_11", "MSE_b_12", "MSE_b_20", "MSE_b_21", "MSE_b_22", "MSE_b_23", "MSE_rho",
           "MAPE_b_10", "MAPE_b_11", "MAPE_b_12", "MAPE_b_20", "MAPE_b_21", "MAPE_b_22", "MAPE_b_23", "MAPE_rho")
# Создаем пустые датафреймы, в которых будет содержаться информация об оценках беты
# и метрики для каждого априорного распределения

df <- data.frame(t(rep(0, length(rnames))))
names(df) <- rnames
df = df[FALSE,]
df
# пробная попытка
data <- list(N=N,
             K1 = dim(X_1)[2],
             K2 = dim(X_2)[2],
             Y=y_star,
             X1 = X_1,
             X2=X_2
             )

model <- stan(file = "/Users/markymark/Desktop/Статистика/bivariate/biprobit.stan",
                     data = data,                # входные данные
                     chains = 1,                 # количество выборок из апостериорного распределения
                     iter = 2000)                # удвоенный объем выборки из апостериорного распределения
posterior <- extract(model)


sample <- generate_sample(1000)
X_1 <- sample$X_1
X_2 <- sample$X_2
y_star <- sample$y_star

# на одну итерацию уходит около 3 минут, прогоним 100 итераций:
for (i in 1:n_iters){
  
  sample <- generate_sample(1000)
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
  
  model <- stan(file = "/Users/markymark/Desktop/Статистика/bivariate/biprobit.stan",
                       data = data,                # входные данные
                       chains = 1,                 # количество выборок из апостериорного распределения
                       iter = 2000)                # удвоенный объем выборки из апостериорного распределения
  posterior <- extract(model)
  
  print(colMeans(posterior$beta1))
  
  # Подсчет метрик
  #------------------------------------------
  
  df <- estimations_metrics(df=df, posterior=posterior, k_1=dim(X_1)[2], k_2=dim(X_2)[2])
  
  #------------------------------------------
}  
names(df) <- rnames

true = c(beta_1, beta_2, sigma[1,2] / (sqrt(sigma[1,1] * sigma[2,2])), rep(0, length(rnames)-length(beta_1)-length(beta_2)-1))

table = data.frame(true=true,
                   biprobit=colMeans(df))
# --------------------------

# Итоговые таблицы! 
# --------------------------
df

table

# Write data to csv files:  
# decimal point = "." and value separators = comma (",")
write.csv(table, file = "/Users/markymark/Desktop/Статистика/bivariate/results_biprobit.csv")
write.csv(df, file = "/Users/markymark/Desktop/Статистика/bivariate/estimates.csv")
