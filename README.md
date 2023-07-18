Contained in this repository:

Stan files, these are very basic and general stan files for each of the models tested. The models tested were either the optimal model (price and vendor type) or the full model (price, vendor type, and season). Models were either informed by the maximum likelihood estimators from a logistic model fitted in R, or uninformed (~N(0,1)). Finally, models were tested with or without an interaction term of price:vendor type. These files work in tandem with the R file titled stanModels.r. to run, I simply used the run script function in R studio.

R files. There are a few R files. GLMS.r goes through the succinct analysis of the project. StanModels.r runs all the Bayesian models using Rstan. AllGLMS.r is a program that lists the top models ranked by AIC. All R models were run simply with the run script command in r studio.

Csv files. Two csv files include the data used in the analysis. Ocean.csv is the data from the oceana 2018 and 2019 reports on Canadian seafood mislabepling. CollatedDataFinal.csv is data collected from a lab in the University of Guelph and was the main focus of the project.

The pdf file is my final report of the project, and is a good place to start to get a sense of the work done in this repository.
