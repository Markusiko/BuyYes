data {
    int<lower=0> n;
    int<lower=0,upper=1> y[n];
    vector[n] x_1;
    vector[n] x_2;
}

parameters {
    real beta0;
    real beta1;
    real beta2;
}

model {
    beta0 ~ uniform(-10, 10);
    beta1 ~ uniform(-10, 10);
    beta2 ~ uniform(-10, 10);
    y ~ bernoulli(Phi(beta0 + beta1 * x_1 + beta2 * x_2));
  }
