data {
    int<lower=0> N;
    int<lower=0> P;
    matrix[N, P] X;

    int<lower=0, upper=1> isMislabelled[N];
  }
  parameters{
    vector[P] beta;
  }
  model{
    beta ~ normal(0,1);
    isMislabelled ~ bernoulli_logit(X * beta);
  }
  /*generated quantities{
    vector<lower=0, upper=1>[N] prob;
    vector[N] log_lik;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }*/
