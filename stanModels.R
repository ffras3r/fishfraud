library(rstan)
#library(shinystan)
#launch_shinystan(fit)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")
DataFull<-read.csv('CollatedDataFinal.csv')
Data <- subset(DataFull, DataFull$Sample.source != "Missing form")
Tuna <- subset(Data, Data$expectedGen == "Tuna")

attach(Tuna)

modelTuna <- glm(isMislabelled ~ 
                    thePrice + IUCN + theMonth + isCut, 
                    data=Tuna, family=binomial)

XTuna <- model.matrix(~ thePrice + IUCN + theMonth + isCut,  
                      data=Tuna)

betaMuTuna <- coef(modelTuna)
betaSigTuna <- summary(modelTuna)$coef[, "Std. Error"] * sqrt(nrow(Tuna))


informedTuna <- "
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
  generated quantities{
    vector[N] log_lik;
    vector<lower=0, upper=1>[N] prob;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
"

InformedTuna <- stan_model(model_code = informedTuna)

fitInformedTuna <- sampling(
  InformedTuna,
  data = list(
    N=nrow(XTuna),
    P=ncol(XTuna),
    X=XTuna,
    isMislabelled = Tuna$isMislabelled,
    betaMu = betaMuTuna,
    betaSig = betaSigTuna),
  chains=4,
  iter=2000,
  warmup=1000,
  cores=4
)

fitInformedTuna

detach(Tuna)
attach(Data)

model <- glm(isMislabelled ~ thePrice + isRest, data=Data, family=binomial)
X <- model.matrix(~ thePrice + isRest, data=Data)

betaMu <- coef(model)
betaSig <- summary(model)$coef[, "Std. Error"] * sqrt(nrow(Data))

modelFull <- glm(isMislabelled ~ thePrice + isRest + theSeason, data=Data, family=binomial)
XFull <- model.matrix(~ thePrice + isRest + theSeason, data=Data)

betaMuFull <- coef(modelFull)
betaSigFull <- summary(modelFull)$coef[, "Std. Error"] * sqrt(nrow(Data))

modelInteraction <- glm(isMislabelled ~ thePrice + isRest + thePrice:isRest, data=Data, family=binomial)
XInteraction <- model.matrix(~ thePrice*isRest, data=Data)

betaMuInteraction <- coef(modelInteraction)
betaSigInteraction <- summary(modelInteraction)$coef[, "Std. Error"] * sqrt(nrow(Data))

modelFullInteraction <- glm(isMislabelled ~ thePrice + isRest + thePrice:isRest + theSeason, data=Data, family=binomial)
XFullInteraction <- model.matrix(~ thePrice*isRest + theSeason, data=Data)

betaMuFullInteraction <- coef(modelFullInteraction)
betaSigFullInteraction <- summary(modelFullInteraction)$coef[, "Std. Error"] * sqrt(nrow(Data))

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
  generated quantities{
    vector[N] log_lik;
    vector<lower=0, upper=1>[N] prob;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
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
  generated quantities{
    vector[N] log_lik;
    vector<lower=0, upper=1>[N] prob;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
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
  generated quantities{
    vector[N] log_lik;
    vector<lower=0, upper=1>[N] prob;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
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
  generated quantities{
    vector[N] log_lik;
    vector<lower=0, upper=1>[N] prob;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
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
  generated quantities{
    vector<lower=0, upper=1>[N] prob;
    vector[N] log_lik;
    for(i in 1:N){
      log_lik[i] = bernoulli_logit_lpmf(isMislabelled[i] | X[i] * beta);
      prob[i] = inv_logit(log_lik[i]);
    }
  }
"

Informed <- stan_model(model_code = informed)
InformedFull <- stan_model(model_code = informedFull)
InformedInteraction <- stan_model(model_code = informedInteraction)
InformedFullInteraction <- stan_model(model_code = informedFullInteraction)

Uninformed <- stan_model(model_code = uninformed)


# fitInformed <- sampling(
#   Informed,
#   data = list(
#     N=nrow(X),
#     P=ncol(X),
#     X=X,
#     isMislabelled = Data$isMislabelled,
#     betaMu = betaMu,
#     betaSig = betaSig),
#     chains=4,
#     iter=2000,
#     warmup=1000,
#     cores=4
# )
# 
# fitInformedFull <- sampling(
#   InformedFull,
#   data = list(
#     N=nrow(XFull),
#     P=ncol(XFull),
#     X=XFull,
#     isMislabelled = Data$isMislabelled,
#     betaMu = betaMuFull,
#     betaSig = betaSigFull),
#   chains=4,
#   iter=2000,
#   warmup=1000,
#   cores=4
# )
# 
# fitInformedInteraction <- sampling(
#   InformedInteraction,
#   data = list(
#     N=nrow(XInteraction),
#     P=ncol(XInteraction),
#     X=XInteraction,
#     isMislabelled = Data$isMislabelled,
#     betaMu = betaMuInteraction,
#     betaSig = betaSigInteraction),
#   chains=4,
#   iter=2000,
#   warmup=1000,
#   cores=4
# )
# 
# fitInformedFullInteraction <- sampling(
#   InformedFullInteraction,
#   data = list(
#     N=nrow(XFullInteraction),
#     P=ncol(XFullInteraction),
#     X=XFullInteraction,
#     isMislabelled = Data$isMislabelled,
#     betaMu = betaMuFullInteraction,
#     betaSig = betaSigFullInteraction),
#   chains=4,
#   iter=2000,
#   warmup=1000,
#   cores=4
# )
# 
# fitUninformed <- sampling(
#   Uninformed,
#   data = list(
#     N=nrow(X),
#     P=ncol(X),
#     X=X,
#     isMislabelled = Data$isMislabelled),
#   chains=4,
#   iter=2000,
#   warmup=1000,
#   cores=4
# )

# fitInformed
# fitInformedFull
# fitInformedInteraction
# fitInformedFullInteraction
# fitUninformed

#log_lik_in <- extract_log_lik(fitInformed, merge_chains = FALSE)
#traceplot(fit)



