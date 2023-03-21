library(rstan)
setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")

rt <- stanc(file = "stanTest.stan", model_name = 'fish')

#sm <- stan_model(stanc_ret = rt, verbose = TRUE)
sm <- stan_model(stanc_ret = rt)

Data<-read.csv('CollatedDataFinal.csv')
N=196
model <- list(N=N, isMislabelled=Data$isMislabelled, isCut=Data$isCut, isMixed=Data$isMixed , isRaw=Data$isRaw, thePrice=Data$thePrice)


fit <- sampling(sm, data=model, chains=4)
fit
