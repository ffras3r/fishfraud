library(rstan)
#library(shinystan)
#launch_shinystan(fit)

options(mc.cores = parallel::detectCores()) #for efficiency
rstan_options(auto_write = TRUE)

setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self\\forGit") #set working directory
DataFull<-read.csv('CollatedDataFinal.csv') #read in datafile
Data <- subset(DataFull, DataFull$Sample.source != "Missing form") #remove observations without process data
attach(Data)

#OPTIMAL MODEL
model <- glm(isMislabelled ~ thePrice + isRest, data=Data, family=binomial) #model
X <- model.matrix(~ thePrice + isRest, data=Data) #matrix of parameters

betaMu <- coef(model) #mean estimates
betaSig <- summary(model)$coef[, "Std. Error"] * sqrt(nrow(Data)) #estimate standard deviations

#FULL MODEL
modelFull <- glm(isMislabelled ~ thePrice + isRest + theSeason, data=Data, family=binomial)
XFull <- model.matrix(~ thePrice + isRest + theSeason, data=Data)

betaMuFull <- coef(modelFull)
betaSigFull <- summary(modelFull)$coef[, "Std. Error"] * sqrt(nrow(Data))

#OPTIMAL W INTERACTION
modelInteraction <- glm(isMislabelled ~ thePrice + isRest + thePrice:isRest, data=Data, family=binomial)
XInteraction <- model.matrix(~ thePrice*isRest, data=Data)

betaMuInteraction <- coef(modelInteraction)
betaSigInteraction <- summary(modelInteraction)$coef[, "Std. Error"] * sqrt(nrow(Data))

#FULL MODEL W INTERACTION
modelFullInteraction <- glm(isMislabelled ~ thePrice + isRest + thePrice:isRest + theSeason, data=Data, family=binomial)
XFullInteraction <- model.matrix(~ thePrice*isRest + theSeason, data=Data)

betaMuFullInteraction <- coef(modelFullInteraction)
betaSigFullInteraction <- summary(modelFullInteraction)$coef[, "Std. Error"] * sqrt(nrow(Data))

#read stan code
informed <- stanc(file = "StanModels\\Informed.stan", model_name = 'Informed')
informedFull <- stanc(file = "StanModels\\InformedFull.stan", model_name = 'InformedFull')
informedInteraction <- stanc(file = "StanModels\\InformedInteraction.stan", model_name = 'InformedInteraction')
informedFullInteraction <- stanc(file = "StanModels\\InformedFullInteraction.stan", model_name = 'InformedFullInteraction')
uninformed <- stanc(file = "StanModels\\Uninformed.stan", model_name = 'Uninformed')

Informed <- stan_model(stanc_ret = informed)
InformedFull <- stan_model(stanc_ret = informedFull)
InformedInteraction <- stan_model(stanc_ret = informedInteraction)
InformedFullInteraction <- stan_model(stanc_ret = informedFullInteraction)

Uninformed <- stan_model(stanc_ret = uninformed)

#define each model conversion
fitInformed <- sampling(
  Informed,
  data = list(
    N=nrow(X),
    P=ncol(X),
    X=X,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMu,
    betaSig = betaSig),
    chains=4,
    iter=2000,
    warmup=1000,
    cores=4
)

fitInformedFull <- sampling(
  InformedFull,
  data = list(
    N=nrow(XFull),
    P=ncol(XFull),
    X=XFull,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMuFull,
    betaSig = betaSigFull),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitInformedInteraction <- sampling(
  InformedInteraction,
  data = list(
    N=nrow(XInteraction),
    P=ncol(XInteraction),
    X=XInteraction,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMuInteraction,
    betaSig = betaSigInteraction),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitInformedFullInteraction <- sampling(
  InformedFullInteraction,
  data = list(
    N=nrow(XFullInteraction),
    P=ncol(XFullInteraction),
    X=XFullInteraction,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMuFullInteraction,
    betaSig = betaSigFullInteraction),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitInformedFullInteraction <- sampling(
  InformedFullInteraction,
  data = list(
    N=nrow(XFullInteraction),
    P=ncol(XFullInteraction),
    X=XFullInteraction,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMuFullInteraction,
    betaSig = betaSigFullInteraction),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitInformedFullInteraction <- sampling(
  InformedFullInteraction,
  data = list(
    N=nrow(XFullInteraction),
    P=ncol(XFullInteraction),
    X=XFullInteraction,
    isMislabelled = Data$isMislabelled,
    betaMu = betaMuFullInteraction,
    betaSig = betaSigFullInteraction),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)


fitUninformed <- sampling(
  Uninformed,
  data = list(
    N=nrow(X),
    P=ncol(X),
    X=X,
    isMislabelled = Data$isMislabelled),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitUninformedInteraction <- sampling(
  Uninformed,
  data = list(
    N=nrow(XInteraction),
    P=ncol(XInteraction),
    X=XInteraction,
    isMislabelled = Data$isMislabelled),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitUninformedFull <- sampling(
  Uninformed,
  data = list(
    N=nrow(XFull),
    P=ncol(XFull),
    X=XFull,
    isMislabelled = Data$isMislabelled),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitUninformedFullInteraction <- sampling(
  Uninformed,
  data = list(
    N=nrow(XFullInteraction),
    P=ncol(XFullInteraction),
    X=XFullInteraction,
    isMislabelled = Data$isMislabelled),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

#print models
fitInformed
fitInformedFull
fitInformedInteraction
fitInformedFullInteraction

fitUninformed
fitUninformedFull
fitUninformedInteraction
fitUninformedFullInteraction

#log_lik_in <- extract_log_lik(fitInformed, merge_chains = FALSE)

informed <- "
  data {
    int<lower=0> N;
    int<lower=0> P;
    matrix[N, P] X;
    vector[P] betaMu;
    vector[P] betaSig;

    int<lower=0, upper=1> isMislabelled[N];
  }
  parameters{
    vector[P] beta;
  }
  model{
    for(i in 1:P){
      beta[i] ~ normal(betaMu[i], betaSig[i]);
    }
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
"

informedFull <- "
  data {
    int<lower=0> N;
    int<lower=0> P;
    matrix[N, P] X;
    vector[P] betaMu;
    vector[P] betaSig;

    int<lower=0, upper=1> isMislabelled[N];
  }
  parameters{
    vector[P] beta;
  }
  model{
    for(i in 1:P){
      beta[i] ~ normal(betaMu[i], betaSig[i]);
    }
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
"

informedInteraction <- "
  data {
    int<lower=0> N;
    int<lower=0> P;
    matrix[N, P] X;
    vector[P] betaMu;
    vector[P] betaSig;

    int<lower=0, upper=1> isMislabelled[N];
  }
  parameters{
    vector[P] beta;
  }
  model{
    for(i in 1:P){
      beta[i] ~ normal(betaMu[i], betaSig[i]);
    }
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
"

informedFullInteraction <- "
  data {
    int<lower=0> N;
    int<lower=0> P;
    matrix[N, P] X;
    vector[P] betaMu;
    vector[P] betaSig;

    int<lower=0, upper=1> isMislabelled[N];
  }
  parameters{
    vector[P] beta;
  }
  model{
    for(i in 1:P){
      beta[i] ~ normal(betaMu[i], betaSig[i]);
    }
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
"

uninformed <- "
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
"




