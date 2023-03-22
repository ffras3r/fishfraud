library(rstan)
setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")

rt <- stanc(file = "stanTest.stan", model_name = 'fish')

#sm <- stan_model(stanc_ret = rt, verbose = TRUE)
sm <- stan_model(stanc_ret = rt)

DataFull<-read.csv('CollatedDataFinal.csv')
Data <- subset(Data, Data$Sample.source != "Missing form")
length(Data$isMislabelled)

#N=196
N=length(Data$isMislabelled)

LC <- ifelse(Data$IUCN == "LC", 1, 0)
VU <- ifelse(Data$IUCN == "VU", 1, 0)
NR <- ifelse(Data$IUCN == "NR", 1, 0)
EN <- ifelse(Data$IUCN == "EN", 1, 0)

summer <- ifelse(Data$theSeason == "Summer", 1, 0)
winter <- ifelse(Data$theSeason == "Winter", 1, 0)
spring <- ifelse(Data$theSeason == "Spring", 1, 0)

butter <- ifelse(Data$expectedGen=="Butterfish", 1, 0)
clam <- ifelse(Data$expectedGen=="Clam", 1, 0)
fish <- ifelse(Data$expectedGen=="Fish", 1, 0)
mackerel <- ifelse(Data$expectedGen=="Mackerel", 1, 0)
oct <- ifelse(Data$expectedGen=="Octopus", 1, 0)
salmon <- ifelse(Data$expectedGen=="Salmon", 1, 0)
shrimp <- ifelse(Data$expectedGen=="Shrimp", 1, 0)
snapper <- ifelse(Data$expectedGen=="Snapper", 1, 0)
tilapia <- ifelse(Data$expectedGen=="Tilapia", 1, 0)
tuna <- ifelse(Data$expectedGen=="Tuna", 1, 0)
unknown <- ifelse(Data$expectedGen=="Unknown", 1, 0)

jan <- ifelse(Data$theMonth=="Jan",1,0)
feb <- ifelse(Data$theMonth=="Feb",1,0)
mar <- ifelse(Data$theMonth=="Mar",1,0)
may <- ifelse(Data$theMonth=="May",1,0)
apr <- ifelse(Data$theMonth=="Apr",1,0)
jun <- ifelse(Data$theMonth=="Jun",1,0)
jul <- ifelse(Data$theMonth=="Jul",1,0)
aug <- ifelse(Data$theMonth=="Aug",1,0)
sep <- ifelse(Data$theMonth=="Sep",1,0)
oct <- ifelse(Data$theMonth=="Oct",1,0)
nov <- ifelse(Data$theMonth=="Nov",1,0)



# model1 <- list(N=N, isMislabelled=Data$isMislabelled, isCut=Data$isCut, isMixed=Data$isMixed, thePrice=Data$thePrice)
# model2 <- list(N=N, isMislabelled=Data$isMislabelled, LC=LC, VU=VU, NR=NR, EN=EN, summer=summer, winter=winter, spring=spring, thePrice=Data$thePrice)
# model3 <- list(N=N, isMislabelled=Data$isMislabelled, thePrice = Data$thePrice, butter=butter, clam=clam, fish=fish, mackerel=mackerel, oct=oct, salmon=salmon, shrimp=shrimp, snapper=snapper, tilapia=tilapia, tuna=tuna, unknown=unknown)
model4 <- list(N=N, isMislabelled=Data$isMislabelled, thePrice = Data$thePrice, rest=Data$isRest, summer=summer, winter=winter, spring=spring)
fit1 <- sampling(sm, data=model4, chains=4)
fit1
#fit2 <- sampling(sm, data=model1, chains=4)
