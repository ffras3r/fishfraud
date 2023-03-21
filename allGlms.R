# load the MASS package
library(MASS)

setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")
theData<-read.csv('CollatedDataFinal.csv')
getwd()
attach(theData);

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

# create a vector of predictor names
predictors <- c("thePrice", "Sample.source", "Locality", "IUCN", "expectedGen", "theMonth", "theSeason", "isCut", "isMixed", "isRaw")

# create an empty data frame to store the results
results <- data.frame()

# loop through all possible combinations of predictor variables
for (i in 1:length(predictors)) {
  comb <- combn(predictors, i)
  
  # loop through each combination of predictors
  for (j in 1:ncol(comb)) {
    formula <- as.formula(paste("isMislabelled ~", paste(comb[,j], collapse="+")))
    model <- glm(formula, data=theData, family=binomial(link='logit'))
    
    # calculate the AIC and p-value for each model
    aic <- AIC(model)
    null_model <- glm(isMislabelled ~ 1, family=binomial)
    p_value <- anova(null_model, model, test="Chi")[2,"Pr(>Chi)"]
    
    # add the results to the data frame
    results <- rbind(results, c(paste(comb[,j], collapse="+"), aic, p_value))
  }
}

# sort the data frame by AIC and p-value
results_aic <- results[order(results[,2]),]
results_pvalue <- results[order(results[,3]),]

# select the model with the lowest AIC using stepAIC function
best_model <- stepAIC(model, direction="both", trace=FALSE)

cat("Top 10 models by AIC:\n")
for (i in 1:10) {
  formula <- as.formula(paste("isMislabelled ~", results_aic[i,1]))
  model <- glm(formula, data=theData, family=binomial(link='logit'))
  print(formula)
  print(AIC(model))
  #print(summary(model))
}

# output the top 10 models by p-value
cat("Top 10 models by p-value:\n")
for (i in 1:10) {
  formula <- as.formula(paste("isMislabelled ~", results_pvalue[i,1]))
  print(formula)
  model <- glm(formula, data=theData, family=binomial(link='logit'))
  print(anova(model, null, test="Chisq"))
}

