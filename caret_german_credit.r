
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("BradleyTerry2")

library(caret)
data(GermanCredit)
summary(GermanCredit)
str(GermanCredit)

# este dataset es básicamente dummies de categorías como 
# Purpose.NewCar
# CreditHistory.Delay

#doMC no funciona en windows
#library(doMC)
#registerDoMC(8)

nearZeroVar(GermanCredit)
# indices de las variables constantes
names(GermanCredit)[nearZeroVar(GermanCredit)]
# Son (prácticamente todos) trabajadores extranjeros

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]

# y ahora nos quitamos de enmedio unas cuantas variables más
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

set.seed(1056)

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

svmFit = train(Class ~ ., 
               data = GermanCreditTrain,
               method = "svmRadial",
               trControl = ctrl,
               metric = "ROC")

svmFit

plot(svmFit)


# Now apply the trained model to test data

plsClasses <- predict(svmFit, newdata = GermanCreditTest)
str(plsClasses)

# you can specify probabilities as output of prediction
plsProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(plsProbs)

# caret contains a function to compute the confusion matrix and associated statistics for the model fit
confusionMatrix(data = plsClasses, GermanCreditTest$Class)
