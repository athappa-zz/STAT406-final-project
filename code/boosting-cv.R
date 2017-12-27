########################################################################
# Boosting with cross validation of # iterations
# run k-fold cross validation to estimate # iterations
########################################################################

rm(list=ls())
getwd()

library(randomForest)
library(adabag)
library(maboost)


#####################
# read in data
#####################

dat <- read.table("~/STAT406-final-project/clean_data/clean_data.txt", header=T, sep=",")

dat <- dat[,-1]
summary(dat$age_of_death_above_one_year)

factor.vars <- c("state", "rural", "sex", "treatment_source" , "death_symptoms", "factors_contributing_death",
                 "factors_contributing_death_2","religion","marital_status", "highest_qualification","occupation_status",
                 "chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
                 "household_have_electricity")

# factorize variables
dat[factor.vars] <- lapply(dat[factor.vars], factor)
split(names(dat),
      sapply(dat, function(x) paste(class(x), collapse=" ")))

dat$age_of_death_above_one_year <- cut(as.numeric(dat$age_of_death_above_one_year), 
                                       breaks=c(-1,33,66,99))

# make sure age_of_death_above_one_year is a factor
sapply(dat, class)


# randomly shuffle data 
shuffleData <- dat[sample(nrow(dat)), ]

# create k equally size folds
k <- 2
folds <- cut(seq(1,nrow(shuffleData)), breaks=k, labels=FALSE)
iter.kf <- c(100, 200, 300, 400, 500)

# initialize matrix we will populate
pred.boost.error <- matrix(rep(0, k*length(iter.kf)), nrow = k, byrow = TRUE)
pred.boost.error

control.module <- rpart.control(maxdepth=2)
adaboost <- boosting(age_of_death_above_one_year ~ .,
                     data = dat.tr,
                     # data=dat.tr[sample(nrow(dat.tr),5000),],
                     mfinal=500,
                     coeflearn='Breiman',
                     control = control.module)

adaboost.pred <- predict.boosting(adaboost, newdata=dat.te)
adaboost.pred$confusion
adaboost.pred$error
adaboost

plot(errorevol(adaboost, newdata=dat.te))

# cross validate iter.kf parameter

library("ggplot2")
qplot(dat$as, geom = "histogram")
##########################
# cross validation test
##########################

#Create 10 equally size folds
folds <- cut(seq(1,nrow(shuffleData)),
             breaks=10,
             labels=FALSE)

#Perform k fold cross validation
for(i in iter.kf){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- shuffleData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  control.module <- rpart.control(maxdepth=2)
  adaboost <- boosting(age_of_death_above_one_year ~ .,
                       # data = trainData[sample(nrow(trainData),100),],
                       data=dat.tr[sample(nrow(dat.tr),5000),],
                       mfinal=200,
                       coeflearn='Breiman',
                       control = control.module)
  
  adaboost.pred <- predict.boosting(adaboost, newdata=testData)
  paste0("error for", i, "is", adaboost.pred$error)
}







# install.packages("gbm")
# library("gbm")



