
# Trying caret
# from vignette
# https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

library(caret)
library(mlbench)
data(Sonar)
summary(Sonar)
str(Sonar$Class) #class is a factor


set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,
                                 ## the outcome data are needed
                                 p = .75,
                                 ## The percentage of data in the
                                 ## training set
                                 list = FALSE) ## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)


# By default,createDataPartitiondoes a stratied random split of the data. To partition the data:

training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
nrow(Sonar)
nrow(training)
nrow(testing)

# To tune a model using Algorithm??, the train function can be used. More details on this function
# can be found at:
#  http://caret.r-forge.r-project.org/training.html
  
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                ## Center and scale the predictors for the training
                ## set and all future samples.
                preProc = c("center", "scale"))

str(plsFit)


# The train function can generate a candidate set of parameter values and
# the tuneLength argument controls how many are evaluated. 
# In the case of PLS, the function uses a sequence of integers from 1 to tuneLength.

# The tuneGrid argument is used when specific values are desired. 
# A data frame is used where each row is a tuning parameter setting and each column is a tuning parameter.

plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                preProc = c("center", "scale"))

plsFit

#adding a trainControl object you can specify cross-valid parameters, + summary function

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# and you can specify an appropriate metric by using metric in train

plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))

plsFit

plot(plsFit)

# Now apply the trained model to test data

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

# you can specify probabilities as output of prediction
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

# caret contains a function to compute the confusion matrix and associated statistics for the model fit
confusionMatrix(data = plsClasses, testing$Class)


# to fit a regularized discriminant model to these data, the following syntax can be used:
## To illustrate, a custom grid is used

# CAVEAT MAY REQUIRE ADDITIONAL LIBRARIES IE combinat
install.packages("combinat")

rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")

rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# How do these models compare in terms of their resampling results? Theresamplesfunction can be
# used to collect, summarize and contrast the resampling results. Since the random number seeds
# were initialized to the same value prior to callingtrain, the same folds were used for each model.
# To assemble them:

resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

# There are several functions to visualize these results. 
# For example, a Bland{Altman type plot can be created using

xyplot(resamps, what = "BlandAltman")

#Since, for each resample, there are paired results a pairedt{test can be used to assess whether there
# is a difference in the average resampled area under the ROC curve. The diff.resamplesfunction
#can be used to compute this

diffs <- diff(resamps)
summary(diffs)
