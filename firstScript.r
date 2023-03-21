library(rstan)
setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")

rt <- stanc(file = "stanTest.stan", model_name = 'fish')

#sm <- stan_model(stanc_ret = rt, verbose = TRUE)
sm <- stan_model(stanc_ret = rt)

Data<-read.csv('CollatedDataFinal.csv')
model <- list(N=196, subset(Data, select = c("isMislabelled", "thePrice", "isMixed", "isCut", "isRaw")))

fit <- sampling(sm, data=model, chains=4)
fit