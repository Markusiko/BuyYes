library(GJRM)
source("/Users/markymark/Desktop/Статистика/bivariate/au_functions.R")

options(mc.cores = parallel::detectCores())
set.seed(999)

n_iters <- 101
beta_1 <- matrix(c(3, 2, -2.5), nrow=3)      # Задаем реальные значения b_0, b_1, b_2 для каждого уравнения системы
beta_2 <- matrix(c(2, 1, -2, -2.5), nrow=4) 

rnames = c("b_10","b_11", "b_12", "b_20", "b_21", "b_22", "b_23", "rho")
df <- data.frame(t(rep(0, length(rnames))))
names(df) <- rnames
df = df[FALSE,]

for (i in 2:n_iters){
  sample <- generate_sample(1000)

  data <- data.frame(list(x_11=sample$X_1[, 2], x_12=sample$X_1[, 3],
                         x_21=sample$X_2[, 2], x_22=sample$X_2[, 3], x_23=sample$X_2[, 4],
                         y_1=sample$y[, 1], y_2=sample$y[, 2]))

  first.eq <- y_1 ~  x_11 + x_12
  second.eq <- y_2 ~ x_21 + x_22 + x_23
  f.list <- list(first.eq, second.eq)
  mr <- c("probit", "probit")

  bvp <- gjrm(f.list, data=data, Model="B", margins=mr)
  results <- summary(bvp)
  estimates <- c(results$tableP1[, 1], results$tableP2[, 1], results$theta)
  
  df <- rbind(df, estimates)
}

names(df) <- rnames
df
write.csv(df, file = "/Users/markymark/Desktop/Статистика/bivariate/estimates_ml.csv")
