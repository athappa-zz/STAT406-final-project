# Feature Selection

rm(list=ls(all=TRUE))
setwd

# Load libraries
library(MASS)

# Read in data
# data <- read.table(unz('~/STAT406-final-project/clean_data/clean_data.csv', '~/STAT406-final-project/clean_data/clean_data.csv.zip'), 
#                    header=T, quote="\"", sep=",")

raw.data <- read.csv("~/STAT406-final-project/clean_data/clean_data.csv", nrows = 5000, 
                     header=T, fill = TRUE, sep = " ")

clean.data <- na.omit(raw.data)

summary(clean.data$age_of_death_above_one_year)

full <- lm(age_of_death_above_one_year~., data=clean.data)
null <- lm(age_of_death_above_one_year~1, data=clean.data)

# Ask Matias about scalability of StepAIC 
stAICF <- stepAIC(null,scope=list(lower=null,upper=full),trace=F, data=clean.data)
stAICB <- stepAIC(full,direction = "backward", data=clean.data)

matias <- model.matrix(full)

# use r factors so we don't drop pieces of a factor 


####################################################################

## Feature Selection
## stepAIC to select features

## ten-fold CV
install.packages('glmnet')
library(glmnet)
library(MASS)

k <- 10
n <- nrow(clean.data)
y <- as.vector(clean.data$age_of_death_above_one_year)
keep <-  c("state", "rural", "sex", "treatment_source" , "death_symptoms", "factors_contributing_death",
           "factors_contributing_death_2","religion","marital_status", "highest_qualification","occupation_status",
           "chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
           "household_have_electricity","IsCoveredByHealthScheme","as")
# xm <- clean.data[,which(names(clean.data) %in% keep)]
xm <- clean.data
xm <- as.matrix(xm)
lambdas <- exp( seq(-3, 10, length=50))

ii <- (1:n)%% k +1
N <- 50
set.seed(123)
mspe.la <- mspe.st <- mspe.ri <- mspe.f <- rep(0, N)
for (i in 1:N) {
  ii <- sample(ii)
  pr.la <- pr.f <- pr.ri <- pr.st <- rep(0, n)
  for (j in 1:k) {
    tmp.ri <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
                        nfolds = 5, alpha = 0, family = "gaussian")
    tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
                        nfolds = 5, alpha = 1, family = "gaussian")
    null <- lm( ~ 1, data = x[ii != j, ])
    full <- lm(age_of_death_above_one_year ~ ., data = x[ii != j, ])
    tmp.st <- stepAIC(null, scope = list(lower = null, upper = full), trace = 0)
    pr.ri[ii == j] <- predict(tmp.ri, s = "lambda.min", newx = xm[ii == 
                                                                    j, ])
    pr.la[ii == j] <- predict(tmp.la, s = "lambda.min", newx = xm[ii == 
                                                                    j, ])
    pr.st[ii == j] <- predict(tmp.st, newdata = x[ii == j, ])
    pr.f[ii == j] <- predict(full, newdata = x[ii == j, ])
  }
  mspe.ri[i] <- mean((clean.data$age_of_death_above_one_year - pr.ri)^2)
  mspe.la[i] <- mean((clean.data$age_of_death_above_one_year - pr.la)^2)
  mspe.st[i] <- mean((clean.data$age_of_death_above_one_year - pr.st)^2)
  mspe.f[i] <- mean((clean.data$age_of_death_above_one_year - pr.f)^2)
}
boxplot(mspe.la, mspe.ri, mspe.st, mspe.f, names = c("LASSO", "Ridge", "Stepwise", 
                                                     "Full"), col = c("steelblue", "gray80", "tomato", "springgreen"), cex.axis = 1, 
        cex.lab = 1, cex.main = 2)
mtext(expression(hat(MSPE)), side = 2, line = 2.5)






