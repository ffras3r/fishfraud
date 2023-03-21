data{
  int<lower=1> N;
  int<lower=0, upper=1> isMislabelled[N];
  int<lower=0, upper=1> isCut[N];
  int<lower=0, upper=1> isRaw[N];
  int<lower=0, upper=1> isMixed[N];
  real<lower=10, upper=75> thePrice[N];
}

parameters{
  real alpha;
  
  real beta_price;
  real beta_cut;
  real beta_mixed;
  real beta_raw;
}

model {
  vector[N] eta;
  
  alpha ~ normal(-19.45, 1245);
  beta_price ~ normal(0.0478, 0.02035);
  beta_cut ~ normal(.27112, .42883);
  beta_mixed ~ normal(-.1204, .38);
  beta_raw ~ normal(16.83, 1245);
  
  for(i in 1:N){
    eta[i] = alpha + 
          beta_price * thePrice[i] +
          beta_cut * isCut[i] + 
          beta_mixed * isMixed[i] + 
          beta_raw + isRaw[i];
  }
  
  isMislabelled ~ bernoulli_logit(eta);
}