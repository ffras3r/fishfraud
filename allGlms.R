library(MASS)

# THIS PROGRAM CALCULATES THE AIC FOR ALL POSSIBLE MODELS AND ORDERS THEM
# THE REASON FOR THIS WAS TO FIND SIMILAR SIGNIFIANCE-LEVEL MODELS TO COMPARE IN RSTAN

setwd("C:\\Users\\ffras\\OneDrive\\Desktop\\School\\self")
DataFull<-read.csv('forGit\\CollatedDataFinal.csv')
getwd()
theData <- subset(DataFull, DataFull$Sample.source != "Missing form" & DataFull$Sample2 != "Raw/cooked" & DataFull$Sample3 != "rolls/cuts")
attach(theData)

# create a vector of predictor names
predictors <- c("Sample4", "thePrice", "Locality", "theMonth", "theSeason", "isCut", "isMixed", "isRaw", "isRest")

# create an empty data frame to store the results
results <- data.frame()

# loop through all possible combinations of predictor variables
for (i in 1:length(predictors)) {
  comb <- combn(predictors, i)
  
  # loop through each combination of predictors
  for (j in 1:ncol(comb)) {
    formula <- as.formula(paste("isMislabelled ~", paste(comb[,j], collapse="+")))
    model <- glm(formula, data=theData, family=binomial(link='logit'))
    
    # add the results to the data frame
    aic <- AIC(model)
    results <- rbind(results, c(paste(comb[,j], collapse="+"), aic))
  }
}

# sort the data frame by AIC
resultsAIC <- results[order(results[,2]),]

best <- stepAIC(model, direction="both", trace=FALSE)

cat("Top 10 models by AIC:\n")
for (i in 1:10) {
  formula <- as.formula(paste("isMislabelled ~", resultsAIC[i,1]))
  model <- glm(formula, data=theData, family=binomial(link='logit'))
  print(formula)
  print(AIC(model))
  #print(summary(model))
}

