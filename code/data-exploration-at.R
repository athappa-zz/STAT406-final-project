# Data Exploration
# Andrew Thappa

rm(list=ls(all=TRUE))
install.packages("dummies")
library(dummies)

files.format <- files <- list.files(path = "~/STAT406-final-project/data", 
                                    pattern = "*.csv")

for (i in 1:length(files.format)){
  print(files.format[i])
  files.format[i] = paste("~/STAT406-final-project/data/", files.format[i], sep="")
}


myfiles <- do.call(rbind, 
                  lapply(files.format, 
                         function(x) read.csv(x, stringsAsFactors = FALSE)))
names(myfiles)

# keep only a subset of variables
keep <- c("id", "state", "rural", "sex", "date_of_death", "month_of_death",
"year_of_death", "age_of_death_below_one_month", "age_of_death_below_eleven_month",
"age_of_death_above_one_year","treatment_source" , "death_symptoms", "factors_contributing_death",
"factors_contributing_death_2", "date_of_birth", "month_of_birth", "year_of_birth",
"age","religion","marital_status", "highest_qualification","occupation_status" ,
"chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
"household_have_electricity","IsCoveredByHealthScheme","year","as")

data.subset <- myfiles[ , which(names(myfiles) %in% keep)]

data.dummies <- cbind(data.subset, dummy(data.subset$state, sep = "_"))

dummy.vars <- c("state", "rural", "sex", "treatment_source" , "death_symptoms", "factors_contributing_death",
"factors_contributing_death_2","religion","marital_status", "highest_qualification","occupation_status",
"chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
"household_have_electricity","IsCoveredByHealthScheme")

test <- data.subset[1:100,]
test <- dummy.data.frame(data.subset, names = dummy.vars, sep = ".")

 
summary(data.subset)
# issues with types: need to convert lots of things to numeric
sapply(data.subset, class)
names(data.subset)



cols.num <- c("a","b")
data.subset[cols.num] <- sapply(data.subset[cols.num],as.numeric)



head(data.subset[ , sapply(data.subset, is.numeric)])

# pairwise correlations among predictors
pairs(data.subset)
cor(data.subset)






