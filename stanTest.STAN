data{
  int<lower=1> N;
  //int<lower=1> num_expected;
  
  int<lower=0, upper=1> isMislabelled[N];
  
  // int<lower=0, upper=1> isCut[N];
  // int<lower=0, upper=1> isRaw[N];
  // int<lower=0, upper=1> isMixed[N];
  int<lower=0, upper=1> rest[N];
  // 
  // int<lower=0, upper=1> EN[N];
  // int<lower=0, upper=1> LC[N];
  // int<lower=0, upper=1> VU[N];
  // int<lower=0, upper=1> NR[N];
  
  // int<lower=0, upper=1> butter[N];
  // int<lower=0, upper=1> clam[N];
  // int<lower=0, upper=1> fish[N];
  // int<lower=0, upper=1> mackerel[N];
  // int<lower=0, upper=1> oct[N];
  // int<lower=0, upper=1> salmon[N];
  // int<lower=0, upper=1> shrimp[N];
  // int<lower=0, upper=1> snapper[N];
  // int<lower=0, upper=1> tilapia[N];
  // int<lower=0, upper=1> tuna[N];
  // int<lower=0, upper=1> unknown[N];
  
  
  
  int<lower=0, upper=1> summer[N];
  int<lower=0, upper=1> winter[N];
  int<lower=0, upper=1> spring[N];
  
  //int<lower=1, upper=num_expected> expected[N];
  
  real<lower=10, upper=75> thePrice[N];
  
}

parameters{
  real alpha;
  
  real beta_price;
  real beta_rest;
  
  // real beta_cut;
  // real beta_mixed;
  // real beta_raw;
  // real beta_EN;
  // real beta_LC;
  // real beta_VU;
  // real beta_NR;
  
  // real beta_butter;
  // real beta_clam;
  // real beta_fish;
  // real beta_mackerel;
  // real beta_oct;
  // real beta_salmon;
  // real beta_shrimp;
  // real beta_snapper;
  // real beta_tilapia;
  // real beta_tuna;
  // real beta_unknown;
  
  real beta_summer;
  real beta_winter;
  real beta_spring;

  
  //real beta_expected;
}

model {
  vector[N] eta;
  
  //first model
  // alpha ~ normal(-3.04, .65);
  // beta_price ~ normal(0.058, 0.0205);
  // beta_cut ~ normal(.29349, .43415);
  // beta_mixed ~ normal(-.1905, .378);
  
  // for(i in 1:N){
  //   eta[i] = alpha + 
  //         beta_price * thePrice[i] +
  //         beta_cut * isCut[i] + 
  //         beta_mixed * isMixed[i];
  //         //beta_raw * isRaw[i];
  // }
  
  //second
  // alpha ~ normal(-.087474, 12.56);
  // beta_butter ~ normal(.919687, 13.88);
  // beta_clam ~ normal(-.04789, 14.82);
  // beta_fish ~ normal(.002781, 24.56);
  // beta_mackerel ~ normal(-.059463, 18.545);
  // beta_oct ~ normal(-.019966, 18.4367);
  // beta_salmon ~ normal(-.023989, 13.41);
  // beta_shrimp ~ normal(-.016886, 12.599);
  // beta_snapper ~ normal(.95059, 13.09);
  // beta_tilapia ~ normal(-.13438, 25.2358);
  // beta_tuna ~ normal(.1112, 12.4989);
  // beta_unknown ~ normal(-.017562, 12.9759);
  // beta_price ~ normal(.004558, .00075295);
  // 
  // for(i in 1:N){
  //   eta[i] = alpha + 
  //           beta_price * thePrice[i] + 
  //           beta_butter * butter[i] + 
  //           beta_clam * clam[i] + 
  //           beta_fish * fish[i] +
  //           beta_mackerel * mackerel[i] +
  //           beta_oct * oct[i] + 
  //           beta_salmon * salmon[i] + 
  //           beta_shrimp * shrimp[i] + 
  //           beta_snapper * snapper[i] + 
  //           beta_tilapia * tilapia[i] + 
  //           beta_tuna * tuna[i] + 
  //           beta_unknown * unknown[i];
  // }
  alpha ~ normal(-5.125, 263.737);
  beta_price ~ normal(.04684, .08029);
  beta_spring ~ normal(.01519, 41.32);
  beta_summer ~ normal(-.6789, 139.078);
  beta_winter ~ normal(-.99274, 69.108);
  beta_rest ~ normal(.305606, 205.8);
  for(i in 1:N){
    eta[i] = alpha + 
              beta_price * thePrice[i] + 
              beta_spring * spring[i] + 
              beta_summer * summer[i] + 
              beta_winter * winter[i] + 
              beta_rest * rest[i];
  }
  

  
  isMislabelled ~ bernoulli_logit(eta);
}
