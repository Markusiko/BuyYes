// большую часть (особенно связанную с функцией плотности) взял отсюда:
// https://gist.github.com/khakieconomics/473bbcddf07e463b6b7cae8fa50bd53d

functions {
    real binormal_cdf(real z1, real z2, real rho) {
    if (z1 != 0 || z2 != 0) {
      real denom = fabs(rho) < 1.0 ? sqrt((1 + rho) * (1 - rho)) : not_a_number();
      real a1 = (z2 / z1 - rho) / denom;
      real a2 = (z1 / z2 - rho) / denom;
      real product = z1 * z2;
      real delta = product < 0 || (product == 0 && (z1 + z2) < 0);
      return 0.5 * (Phi(z1) + Phi(z2) - delta) - owens_t(z1, a1) - owens_t(z2, a2);
    }
    return 0.25 + asin(rho) / (2 * pi());
  }
  real biprobit_lpdf(row_vector Y, real mu1, real mu2, real rho) {
    real q1;
    real q2;
    real w1;
    real w2;
    real rho1;
    
    q1 = 2*Y[1] - 1.0;
    q2 = 2*Y[2] - 1.0;
    
    w1 = q1*mu1;
    w2 = q2*mu2;
    
    rho1 = q1*q2*rho;
    return log(binormal_cdf(w1, w2, rho1));
  }
}
data {
  int<lower=0> N;  // число наблюдений
  int<lower=1> K1; // кол-во переменных в первом уравнении
  int<lower=1> K2; // кол-во переменных во втором уравнении
  matrix[N, 2] Y;
  matrix[N, K1] X1; // X в первом уравнении
  matrix[N, K2] X2; // X во втором уравнении
  
}

parameters {
  vector[K1] beta1;
  vector[K2] beta2;
  real<lower = -1, upper = 1> rho;
}
model {
  // приорные распределения
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  
  // правдоподобие
  for(i in 1:N) {
    Y[i] ~ biprobit(X1[i] * beta1, X2[i] * beta2, rho);}
}
