#Read data, from Bob's lab
setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self\\previous")
DataFull<-read.csv('forGit\\CollatedDataFinal.csv')

DataFull$Sample1 <- trimws(DataFull$Sample1)
DataFull$Sample2 <- trimws(DataFull$Sample2)
DataFull$Sample3 <- trimws(DataFull$Sample3)
DataFull$Sample4 <- trimws(DataFull$Sample4)
DataFull$Sample.source <- trimws(DataFull$Sample.source)
DataFull$Locality <- trimws(DataFull$Locality)
DataFull$City <- trimws(DataFull$City)
DataFull$expectedGen <- trimws(DataFull$expectedGen)
DataFull$theYear <- trimws(DataFull$theYear)
DataFull$theMonth <- trimws(DataFull$theMonth)
DataFull$theSeason <- trimws(DataFull$theSeason)
DataFull$IUCN <- trimws(DataFull$IUCN)

# Convert to factor

DataFull$Sample1 <- as.integer(as.factor(DataFull$Sample1))
DataFull$Sample2 <- as.integer(as.factor(DataFull$Sample2)) 
DataFull$Sample3 <- as.integer(as.factor(DataFull$Sample3)) 
DataFull$Sample4 <- as.integer(as.factor(DataFull$Sample4)) 
DataFull$Sample.source <- as.integer(as.factor(DataFull$Sample.source)) 
DataFull$Locality <- as.integer(as.factor(DataFull$Locality)) 
DataFull$City <- as.integer(as.factor(DataFull$City)) 
DataFull$expectedGen <- as.integer(as.factor(DataFull$expectedGen)) 
DataFull$theYear <- as.integer(as.factor(DataFull$theYear)) 
DataFull$theMonth <- as.integer(as.factor(DataFull$theMonth)) 
DataFull$theSeason <- as.integer(as.factor(DataFull$theSeason)) 
DataFull$IUCN <- as.integer(as.factor(DataFull$IUCN)) 

#ESTIMATE PRICE
#using linear model
# library(MASS)
# dataPrice <- data.frame(DataFull$Sample.source, DataFull$Locality, DataFull$expectedGen, DataFull$IUCN, DataFull$thePrice, DataFull$theMonth, DataFull$Sample1, DataFull$Sample2, DataFull$Sample3, DataFull$Sample4)
# dataPrice <- subset(dataPrice, dataPrice$DataFull.Sample.source != "Missing form" & dataPrice$DataFull.Sample2 != "Raw/cooked" & dataPrice$DataFull.Sample3 != "rolls/cuts")
# full <- lm(DataFull.thePrice ~ ., data=dataPrice)R
# backward <- stepAIC(full, direction="backward", scope = list(upper = ~., lower = ~1), trace=TRUE)

#using mean imputation
library(mice)
impute <- mice(DataFull, method='pmm')
impute$imp$thePrice #get average of these values?


Data <- subset(DataFull, DataFull$Sample.source != 2 & DataFull$Sample2 != 4 & DataFull$Sample3 != 4)

# THIS R CODE GOES THROUGH THE FINAL WORKTHOUGH OF BUILDING FREQUENTIST MODELS
# EACH SECTION OF CODE IS COMMENTED OUT SO PARTS CAN BE UNCOMMENTED FOR OUTPUT READIBILITY

#Remove data with low counts, useful?
# 
# redData <- Data %>%
#   filter(City %in% names(which(table(City) > 5)),
#          IUCN %in% names(which(table(IUCN) > 5)),
#          expectedGen %in% names(which(table(expectedGen) > 5)),
#          realGen %in% names(which(table(realGen) > 5)))
# attach(Data)

# #MISLABELLING PROPORTIONS 
# Change this var \/ depending on what you want to check
# Table <- table(theMonth, isMislabelled)
# prop_successes <- Table[, 2] / (Table[, 1] + Table[, 2])
# prop_samples <- (Table[, 2] + Table[, 1]) / sum(Table)
# prop_SE <- sqrt((prop_successes * (1-prop_successes)) / (Table[, 1] + Table[, 2])) 
# mosaic <- data.frame(Labeled = Table[, 1], Mislabelled = Table[, 2]) #for mosaic plots
# results <- data.frame(Labeled = Table[, 1], Mislabelled = Table[, 2], MislabelledProp = prop_successes, SE = prop_SE, CIlb = (prop_successes-(1.96*prop_SE)), CIub = (prop_successes+(1.96*prop_SE)), SampleProp = prop_samples)
# TotalMislableProp = sum(Table[, 2]) / (sum(Table[, 1]) + sum(Table[, 2]))
# results
# TotalMislableProp
# 
# chitest <- data.frame(Labeled = Table[, 1], Mislabelled = Table[, 2])
# fisher.test(chitest[,1:2], simulate.p.value=TRUE)  #fishers test for a category

#TEST OF TWO PROPORTIONS FOR EACH VARIABLE CATEGORY
# tests <- matrix(nrow=nrow(results), ncol=nrow(results));
# for(i in 1:nrow(results)){
#   for(j in 1:nrow(results)){
#     if(i < j){
#       p <- (Table[i, 2] + Table[j, 2]) / (Table[i, 1] + Table[j, 1] + Table[i, 2] + Table[j, 2])
#       tests[i, j] <- pnorm((prop_successes[i] - prop_successes[j]) / (sqrt((p*(1-p)) * (1/(Table[i,1]+Table[i,2]) + 1/(Table[j,1]+Table[j,2])))))
#     } else{
#       tests[i,j] <- 0
#     }
#   }
# }
# tests

#UNIVARIATE GLMS
# cityR <- glm(isMislabelled ~ City, family = 'binomial', data=Data)
# source <- glm(isMislabelled ~ Sample.source, family = 'binomial', data=Data)
# local <- glm(isMislabelled ~ Locality, family = 'binomial', data=Data)
# expected <- glm(isMislabelled ~ expectedGen, family = 'binomial', data=Data)
# iucn <- glm(isMislabelled ~ IUCN, family = 'binomial', data=Data)
# price <- glm(isMislabelled ~ thePrice, family = 'binomial', data=Data)
# month <- glm(isMislabelled ~ theMonth, family = 'binomial', data=Data)
# season <- glm(isMislabelled ~ theSeason, family = 'binomial', data=Data)
# cut <- glm(isMislabelled ~ isCut, family = 'binomial', data=Data)
# mixed <- glm(isMislabelled ~ isMixed, family = 'binomial', data=Data)
# raw <- glm(isMislabelled ~ isRaw, family = 'binomial', data=Data)
# sushiType <- glm(isMislabelled ~ Sample4, family = 'binomial', data=Data)
# 
# null <- glm(isMislabelled ~ 1, family='binomial', data=Data)

#FIRST DROP IN DEVIANCE TESTS
# anova(cityR, null, test='Chisq')
# anova(source, null, test='Chisq')
# anova(local, null, test='Chisq')
# anova(expected, null, test='Chisq')
# anova(price, null, test='Chisq')
# anova(iucn, null, test='Chisq')
# anova(month, null, test='Chisq')
# anova(season, null, test='Chisq')
# anova(cut, null, test='Chisq')
# anova(mixed, null, test='Chisq')
# anova(raw, null, test='Chisq')
# anova(sushiType, null, test='Chisq')

# #AIC OF EACH UNIVARIATE GLM
# AIC(cityR)
# AIC(source)
# AIC(local)
# AIC(expected)
# AIC(iucn)
# AIC(real)
# AIC(price)
# AIC(month)
# AIC(season)
# AIC(cut)
# AIC(mixed)
# AIC(raw)
# AIC(sushiType)

# summary(cityR)
# summary(source)
# summary(local)
# summary(expected)
# #summary(real)
# summary(price)
# summary(iucn)
# summary(month)
# summary(season)
# summary(cut)
# summary(mixed)
# summary(raw)

#TWO VARIABLE MODELS
# priceCity <- glm(isMislabelled ~ isRest + City, family='binomial', data=Data)
# priceSource <- glm(isMislabelled ~ isRest + thePrice, family='binomial', data=Data)
# pricelocal <- glm(isMislabelled ~ isRest + Locality, family='binomial', data=Data)
# priceiucn <- glm(isMislabelled ~ isRest + IUCN, family='binomial', data=Data)
# priceMonth <- glm(isMislabelled ~ isRest + theMonth, family='binomial', data=Data)
# priceSeason <- glm(isMislabelled ~ isRest + theSeason, family='binomial', data=Data)
# priceCut <- glm(isMislabelled ~ isRest + isCut, family='binomial', data=Data)
# priceMixed <- glm(isMislabelled ~ isRest + isMixed, family='binomial', data=Data)
# priceRaw <- glm(isMislabelled ~ isRest + isRaw, family='binomial', data=Data)
# 
# AIC(priceCity)
# AIC(priceSource)
# AIC(pricelocal)
# AIC(priceiucn)
# AIC(priceMonth)
# AIC(priceSeason)
# AIC(priceCut)
# AIC(priceMixed)
# AIC(priceRaw)


# THREE VAIRABLE MODELS
# priceSourceCity <- glm(isMislabelled ~ thePrice + isRest + City, family='binomial', data=Data)
# priceSourceLocal <- glm(isMislabelled ~ thePrice + isRest + Locality, family='binomial', data=Data)
# priceSourceIUCN <- glm(isMislabelled ~ thePrice + isRest + IUCN, family='binomial', data=Data)
# priceSourceMonth <- glm(isMislabelled ~ thePrice + isRest + theMonth, family='binomial', data=Data)
# priceSourceSeason <- glm(isMislabelled ~ thePrice + isRest + theSeason, family='binomial', data=Data)
# priceSourceCut <- glm(isMislabelled ~ thePrice + isRest + isCut, family='binomial', data=Data)
# priceSourceMixed <- glm(isMislabelled ~ thePrice + isRest + isMixed, family='binomial', data=Data)
# priceSourceRaw <- glm(isMislabelled ~ thePrice + isRest + isRaw, family='binomial', data=Data)
# 
# AIC(priceSourceCity)
# AIC(priceSourceLocal)
# AIC(priceSourceIUCN)
# AIC(priceSourceMonth)
# AIC(priceSourceSeason)
# AIC(priceSourceCut)
# AIC(priceSourceMixed)
# AIC(priceSourceRaw)


#PREDICTION RESULTS, preliminary, didn't get to analysing this kinda cool though
# 
# #change this variable         \/ to one of the glms above to replicate
# final <- glm(isMislabelled ~ expectedGen, data = train, family=binomial)
# predictions <- predict(final, newdata = test, type = 'response')
# predictions <- ifelse(predictions > 0.5, 1, 0)
# true_pos <- sum(predictions ==1 & isMislabelled == 1)
# false_pos <- sum(predictions == 1 & isMislabelled == 0)
# true_neg <- sum(predictions == 0 & isMislabelled == 0)
# false_neg <- sum(predictions == 0 & isMislabelled == 1)
# 
# accuracy <- (true_pos + true_neg) / (true_neg + true_pos + false_neg + false_pos)
# sensitivity <- true_pos / (true_pos + false_neg)
# specificity <- true_neg / (true_neg + false_pos)
# 
# accuracy
# sensitivity
# specificity
 
full <- glm(isMislabelled ~ thePrice + theSeason + isRest, family=binomial, data=Data)
red <- glm(isMislabelled ~ thePrice + isRest, family=binomial, data=Data)

summary(full)
summary(red)
anova(full, red)

#CHECK FOR COLINEARITY
library(car)
vif(full)
vif(red)

#INTERACTION TERMS
#FULL MODEL
# model1 <- glm(isMislabelled ~ thePrice + theSeason + isRest, family=binomial, data=Data)
# int1 <- glm(isMislabelled ~ thePrice + theSeason + isRest + theSeason:isRest, family=binomial, data=Data)
# int2 <- glm(isMislabelled ~ thePrice + theSeason + isRest + thePrice:isRest, family=binomial, data=Data)
# int3 <- glm(isMislabelled ~ thePrice + theSeason + isRest + thePrice:theSeason, family=binomial, data=Data)
# int4 <- glm(isMislabelled ~ thePrice + theSeason + isRest + thePrice:theSeason:isRest, family=binomial, data=Data)
# anova(int1, model1, test='Chisq')
# anova(int2, model1, test='Chisq')
# anova(int3, model1, test='Chisq')
# anova(int4, model1, test='Chisq')
# 
# #OPTIMAL MODEL
# model2 <- glm(isMislabelled ~ thePrice + isRest, family=binomial, data=Data)
# int <- glm(isMislabelled ~ thePrice + isRest + thePrice:isRest, family=binomial, data=Data)
# anova(int, model2, test='Chisq')


