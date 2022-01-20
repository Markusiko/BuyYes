data {
    int<lower=0> n;
    int y[n];
    real x_1[n];
    real x_2[n];
}
transformed data {}
parameters {
    real beta0;
    real beta1;
    real beta2;
}
transformed parameters {}
model {
    beta0 ~ uniform(-10, 10);
    beta1 ~ uniform(-10, 10);
    beta2 ~ uniform(-10, 10);
    for(i in 1:n) {
        y[i] ~ bernoulli(Phi(beta0 + beta1 * x_1[i] + beta2 * x_2[i]));
  }
}
generated quantities {}