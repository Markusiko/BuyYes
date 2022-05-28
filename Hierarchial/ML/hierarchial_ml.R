
library(GJRM)
library('MASS')
source("/Users/User/Desktop/hier/aux_func.R")
options(mc.cores = parallel::detectCores())
set.seed(999)

rnames = c("b_10","b_11", "b_20", "b_21", "b_22", "rho")
df <- data.frame(t(rep(0, length(rnames))))
names(df) <- rnames
df = df[FALSE,]

n_iters <- 100
N <- 1000

for (i in 1:n_iters){
  sample <- generate_sample(N)
  
  data <- data.frame(list(x_11=sample$X_1[, 2],
                          x_21=sample$X_2[, 2], x_22=sample$X_2[, 3],
                          y_1=sample$y[, 1], y_2=sample$y[, 2]))
  
  first.eq <- y_1 ~ x_11 
  second.eq <- y_2 ~ x_21 + x_22
  f.list <- list(first.eq, second.eq)
  mr <- c("probit", "probit")
  
  bvp <- gjrm(f.list, data=data, Model="B", margins=mr)
  results <- summary(bvp)
  estimates <- c(results$tableP1[, 1], results$tableP2[, 1], results$theta)
  
  df <- rbind(df, estimates)
}

names(df) <- rnames
write.csv(df, file = "/Users/User/Desktop/hier/estimates_hier_ml_1000.csv")
