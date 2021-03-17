data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> J;
  int<lower=0,upper=1> y[N];
  int<lower=1,upper=I> i[N];
  int<lower=1,upper=J> j[N];
}

parameters {
  vector[I] alpha;
  vector[I] beta;
  vector[J] theta;
}

transformed parameters {
  real p[N];
  
  for (n in 1:N) {
    p[n] = theta[j[n]] * beta[i[n]] + alpha[i[n]];
  }
}


model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 5);
  theta ~ normal(0, 1);
  for (n in 1:N) {
    y ~ bernoulli_logit(p);
  }
  
}

