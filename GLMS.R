#Read data, from Bob's lab or Oceana
setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")
Data<-read.csv('CollatedDataFinal.csv')
getwd()


#Remove data with low counts, 
Data2 <- Data[Data$City %in% names(which(table(Data$City)>5)),]
Data3 <- Data2[Data2$IUCN %in% names(which(table(Data2$IUCN)>5)),]
Data4 <- Data3[Data3$expectedGen %in% names(which(table(Data3$expectedGen)>5)),]
redData <- Data4[Data4$realGen %in% names(which(table(Data4$realGen)>5)),]

noMissing <- subset(Data, Data$Locality != "Missing form")
forIucn <- subset(Data, Data$IUCN != "NR" & Data$IUCN != "NA")

#MISLABELLING PROPORTIONS 
#Change this var \/ depending on what you want to check
Table <- table(City, isMislabelled)
prop_successes <- Table[, 2] / (Table[, 1] + Table[, 2])
prop_samples <- (Table[, 2] + Table[, 1]) / sum(Table)
mosaic <- data.frame(Labeled = Table[, 1], Mislabelled = Table[, 2])
results <- data.frame(Labeled = Table[, 1], Mislabelled = Table[, 2], MislabelledProp = prop_successes, SampleProp = prop_samples)
# results

#TRAIN THE FOLLOWING GLMS THEN TEST THEM
set.seed(123)
traini <- sample(nrow(Data), nrow(Data)*.7)
train <- Data[traini, ]
test <- Data[-traini, ]
#test <- subset(test, IUCN !="EN")

#UNIVARIATE GLMS
#for accuracy / sensitivity / specificity
# cityR <- glm(isMislabelled ~ City, family = 'binomial', data=train)
# source <- glm(isMislabelled ~ Sample.source, family = 'binomial', data=train)
# local <- glm(isMislabelled ~ Locality, family = 'binomial', data=train)
# expected <- glm(isMislabelled ~ expectedGen, family = 'binomial', data=train)
# iucn <- glm(isMislabelled ~ IUCN, family = 'binomial', data=train)
# real <- glm(isMislabelled ~ realGen, family = 'binomial', data=train)
# price <- glm(isMislabelled ~ thePrice, family = 'binomial', data=train)
# month <- glm(isMislabelled ~ theMonth, family = 'binomial', data=train)
# season <- glm(isMislabelled ~ theSeason, family = 'binomial', data=train)
# cut <- glm(isMislabelled ~ isCut, family = 'binomial', data=train)
# mixed <- glm(isMislabelled ~ isMixed, family = 'binomial', data=train)
# raw <- glm(isMislabelled ~ isRaw, family = 'binomial', data=train)

#for aic bic p-values
cityR <- glm(isMislabelled ~ City, family = 'binomial', data=Data)
source <- glm(isMislabelled ~ Sample.source, family = 'binomial', data=Data)
local <- glm(isMislabelled ~ Locality, family = 'binomial', data=Data)
expected <- glm(isMislabelled ~ expectedGen, family = 'binomial', data=Data)
iucn <- glm(isMislabelled ~ IUCN, family = 'binomial', data=Data)
real <- glm(isMislabelled ~ realGen, family = 'binomial', data=Data)
price <- glm(isMislabelled ~ thePrice, family = 'binomial', data=Data)
month <- glm(isMislabelled ~ theMonth, family = 'binomial', data=Data)
season <- glm(isMislabelled ~ theSeason, family = 'binomial', data=Data)
cut <- glm(isMislabelled ~ isCut, family = 'binomial', data=Data)
mixed <- glm(isMislabelled ~ isMixed, family = 'binomial', data=Data)
raw <- glm(isMislabelled ~ isRaw, family = 'binomial', data=Data)

#Reduced Data
cityRR <- glm(isMislabelled ~ City, family = 'binomial', data=redData)
sourceR <- glm(isMislabelled ~ Sample.source, family = 'binomial', data=redData)
localR <- glm(isMislabelled ~ Locality, family = 'binomial', data=redData)
expectedR <- glm(isMislabelled ~ expectedGen, family = 'binomial', data=redData)
iucnR <- glm(isMislabelled ~ IUCN, family = 'binomial', data=redData)
realR <- glm(isMislabelled ~ realGen, family = 'binomial', data=redData)
priceR <- glm(isMislabelled ~ thePrice, family = 'binomial', data=redData)
monthR <- glm(isMislabelled ~ theMonth, family = 'binomial', data=redData)
seasonR <- glm(isMislabelled ~ theSeason, family = 'binomial', data=redData)
cutR <- glm(isMislabelled ~ isCut, family = 'binomial', data=redData)
mixedR <- glm(isMislabelled ~ isMixed, family = 'binomial', data=redData)
rawR <- glm(isMislabelled ~ isRaw, family = 'binomial', data=redData)

#AIC AND BIC OF EACH UNIVARIATE GLM
# AIC(cityR)
# BIC(cityR)
# 
# AIC(source)
# BIC(source)
# 
# AIC(local)
# BIC(local)
# 
# AIC(expected)
# BIC(expected)
# 
# AIC(iucn)
# BIC(iucn)
# 
# AIC(real)
# BIC(real)
# 
# AIC(price)
# BIC(price)
# 
# AIC(month)
# BIC(month)
# 
# AIC(season)
# BIC(season)
# 
# AIC(cut)
# BIC(cut)
# 
# AIC(mixed)
# BIC(mixed)
# 
# AIC(raw)
# BIC(raw)

#PREDICTION RESULTS
#change this variable         \/ to one of the glms above to replicate
predictions <- predict(cityR, newdata = test, type = 'response')
predictions <- ifelse(predictions > 0.5, 1, 0)
true_pos <- sum(predictions ==1 & isMislabelled == 1)
false_pos <- sum(predictions == 1 & isMislabelled == 0)
true_neg <- sum(predictions == 0 & isMislabelled == 0)
false_neg <- sum(predictions == 0 & isMislabelled == 1)

accuracy <- (true_pos + true_neg) / (true_neg + true_pos + false_neg + false_pos)
sensitivity <- true_pos / (true_pos + false_neg)
specificity <- true_neg / (true_neg + false_pos)

# accuracy
# sensitivity
# specificity

library(nnet)
library(MASS)
priceCity <- multinom(City ~ thePrice, data=Data)
priceMonth <- multinom(theMonth ~ thePrice, data=Data)
priceLoc <- multinom(Locality ~ thePrice, data=noMissing)
priceReal <- multinom(realGen ~ thePrice, data=Data)
priceExp <- multinom(expectedGen ~ thePrice, data=Data)
priceRaw <- glm(isRest ~ thePrice, data=noMissing)
priceCut <- glm(isCut ~ thePrice, data=Data)
priceMixed <- glm(isMixed ~ thePrice, data=Data)
#priceIUCN <- polr(IUCN ~ thePrice, data=forIucn, method='logistic')

# summary(priceCity)
# summary(priceMonth)
# summary(priceLoc)
# summary(priceReal)
# summary(priceExp)
# (1 - pnorm(abs(summary(priceCity)$coefficients/summary(priceCity)$standard.errors), 0, 1))*2
# (1 - pnorm(abs(summary(priceMonth)$coefficients/summary(priceMonth)$standard.errors), 0, 1))*2
# (1 - pnorm(abs(summary(priceLoc)$coefficients/summary(priceLoc)$standard.errors), 0, 1))*2
# (1 - pnorm(abs(summary(priceReal)$coefficients/summary(priceReal)$standard.errors), 0, 1))*2
# (1 - pnorm(abs(summary(priceExp)$coefficients/summary(priceExp)$standard.errors), 0, 1))*2
# 
# summary(priceRaw)
# summary(priceCut)
# summary(priceMixed)
# summary(priceIUCN)


#SUMMARIES
# summary(cityR)
# summary(source)
# summary(local)
# summary(expected)
# summary(real)
# summary(price)
# summary(iucn)
# summary(month)
# summary(season)
# summary(cut)
# summary(mixed)
# summary(raw)
