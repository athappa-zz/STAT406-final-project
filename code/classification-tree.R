
rm(list=ls())
getwd()


library(randomForest)
library(adabag)
library(varImpPlot)
library(maboost)

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

sapply(dat, class)

##classification tree
##split data into training set and test set
# dat$age_of_death_above_one_year

# dat$age_of_death_above_one_year <- cut(as.numeric(dat$age_of_death_above_one_year), 
#                                        breaks=c(-1,33,66,99))

# dat$age_of_death_above_one_year
test <- dat
dat$age_of_death_above_one_year <- cut(as.numeric(dat$age_of_death_above_one_year), 
                                       breaks=c(-1,33,66,99))
n <- nrow(dat)
ii <- sample(n,floor(n/4))
dat.te <- dat[ii,]
dat.tr <- dat[-ii,]

summary(dat$age_of_death_above_one_year)

##build random forest and predict on test set, plot error against trees

#################
# Random Forest
#################

a.rf <- randomForest(dat.tr$age_of_death_above_one_year~.,
                     data=dat.tr,ntree=500)
p.rf <- predict(a.rf, newdata=dat.te,type="response")
table(p.rf,dat.te$age_of_death_above_one_year)
plot(a.rf,lwd=3,lty=1)
a.rf

######################
# Boosting: 2 classes
######################


onesplit <- rpart.control(cp=-1, maxdepth=1, minsplit=0, xval=0)
boost <- boosting(age_of_death_above_one_year ~ .,
                  data = dat.tr,
                  boos = FALSE, 
                  mfinal = 500,
                  control = onesplit)
p.boost <- predict(boost, newdata=dat.te)
pboost.table <- table(dat.te$age_of_death_above_one_year, p.boost$class)

numerator <- table(dat.te$age_of_death_above_one_year, p.boost$class)[2] + 
  table(dat.te$age_of_death_above_one_year, p.boost$class)[3]

denominator <- table(dat.te$age_of_death_above_one_year, p.boost$class)[1] + 
  table(dat.te$age_of_death_above_one_year, p.boost$class)[2] +
  table(dat.te$age_of_death_above_one_year, p.boost$class)[3] + 
  table(dat.te$age_of_death_above_one_year, p.boost$class)[4]

paste("The boosting classification error rate with boosting is",
      round(numerator/denominator, 4))
p.boost$confusion
p.boost$error
plot(errorevol(boost, newdata=dat.te))

#######################
# Boosting: >2 classes
#######################



# dropvar1 <- names(dat.te) %in% c("age_of_death_above_one_year")
# dat.te.x = as.data.frame(dat.te[!dropvar1])
# dat.te.y = as.data.frame(dat.te[dropvar1])
# 
# dropvar2 <- names(dat.te) %in% c("age_of_death_above_one_year")
# dat.tr.x = as.data.frame(dat.tr[!dropvar2])
# dat.tr.y = as.data.frame(dat.tr[dropvar2])
# mboost <- maboost(x = dat.tr.x,
#                   y = dat.tr.y, 
#                   test.x = dat.te.x,
#                   test.y = dat.te.y,
#                   iter = 100)

mboost <- maboost(age_of_death_above_one_year ~ .,
                  data = dat.tr,
                  iter=100,
                  nu = 1, 
                  breg = "entrop")

p.mboost <- predict(mboost,
                    dat.te,
                    type="class");
varplot.maboost(mboost)

#table(p.mboost$age_of_death_above_one_year, )
mboost$confusion
mboost$confusion[1]


##############################
# Different multiclass boost
# Link: https://www.rdocumentation.org/packages/adabag/versions/4.1/topics/boosting
##############################
#control.module <- rpart.control(cp=-1, maxdepth=3, minsplit=0, xval=0)
control.module <- rpart.control(maxdepth=5)
adaboost <- boosting(age_of_death_above_one_year ~ .,
                     data=dat.tr[sample(nrow(dat.tr),5000),],
                     mfinal=100,
                     coeflearn='Breiman',
                     control = control.module)

adaboost.pred <- predict.boosting(adaboost, newdata=dat.te)
adaboost.pred$confusion
adaboost.pred$error
adaboost

########################################################################
# Boosting with cross validation of params maxdepth, # iterations
# run 5-fold cross validation to estimate maxdepth, and # iterations
########################################################################

# randomly shuffle data 
shuffleData <- dat[sample(nrow(dat)), ]

# create k equally size folds
k <- 5
folds <- cut(seq(1,nrow(shuffleData)), breaks=k, labels=FALSE)

# perform 10 fold cross validation
for (i in 1:k){
  testIndices <- which(folds==i, arr.ind=TRUE)
  testData <- shuffleData[testIndices, ]
  trainData <- shuffleData[-testIndices, ]
}





## pairwise chisq post hoc
library(MASS)
chisq.post.hoc(dat)




varImpPlot(a.rf,n.var=19)

