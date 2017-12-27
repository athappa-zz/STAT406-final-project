# Data Exploration

rm(list=ls(all=TRUE))
install.packages("dummies")
library(dummies)

data <- "~/STAT406-final-project/data/"
root <- "~/STAT406-final-project/"
files.format <- files <- list.files(path = data, 
                                    pattern = "*.csv")

for (i in 1:length(files.format)){
  print(files.format[i])
  files.format[i] = paste(data, files.format[i], sep="")
}


myfiles <- do.call(rbind, 
                  lapply(files.format, 
                         function(x) read.csv(x, stringsAsFactors = FALSE)))

# keep only a subset of variables
keep <- c("id", "state", "rural", "sex", "date_of_death", "month_of_death",
"year_of_death", "age_of_death_below_one_month", "age_of_death_below_eleven_month",
"age_of_death_above_one_year","treatment_source" , "death_symptoms", "factors_contributing_death",
"factors_contributing_death_2", "date_of_birth", "month_of_birth", "year_of_birth",
"age","religion","marital_status", "highest_qualification","occupation_status" ,
"chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
"household_have_electricity","IsCoveredByHealthScheme","year","as")

for (i in keep){
  print(i)
}

data.subset <- myfiles[ , which(names(myfiles) %in% keep)]

# drop vals if rural > 2
data.subset <- data.subset[as.numeric(data.subset$rural) == 1 | as.numeric(data.subset$rural) == 2, ]
data.subset <- data.subset[complete.cases(data.subset), ] #remove na
# keep where year == 1
data.subset <- data.subset[as.numeric(data.subset$year) == 1, ] 

dummy.vars <- c("state", "rural", "sex", "treatment_source" , "death_symptoms", "factors_contributing_death",
"factors_contributing_death_2","religion","marital_status", "highest_qualification","occupation_status",
"chew", "smoke", "alcohol", "drinking_water_source", "is_water_filter", "toilet_used",
"household_have_electricity","IsCoveredByHealthScheme", "id")

# coerce columns to factors
factor.vars <- sapply(data.subset[,which(names(data.subset) %in% dummy.vars)], 
               function(x) as.factor(x))   

factor.vars <- as.data.frame(factor.vars)
factor.vars <- merge(factor.vars, myfiles[,c("id", "as")], by="id")


structure(factor.vars)
# we still have a problem with too many NULL values
# factor.vars[apply(factor.vars, 2, function(x) x=="")] = NA
library(data.table)
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#factor.vars <- nullToNA(as.matrix(factor.vars))
test <- factor.vars[!factor.vars$death_symptoms %in% "NULL", ] # 432,000 -> 126,000
test <- test[!test$factors_contributing_death %in% "NULL", ] # 126,000 -> 58,633
test <- test[!test$factors_contributing_death_2 %in% "NULL", ] # 
test <- test[!test$as.x %in% "NULL", ] # 

clean.data <- dummy.data.frame(data.subset, 
                               names = dummy.vars, sep = ".")


# we have lots of missing values...not sure why R didn't pick this up before
sapply(test, function(x) sum(x==" "))

df.clean <- test[!test$death_symptoms %in% " ", ]
df.clean <- df.clean[!df.clean$factors_contributing_death %in% " ", ]
df.clean <- df.clean[!df.clean$death_symptoms %in% " ", ]



merge.age <- merge(x=df.clean,
              y=myfiles[,c("id","age_of_death_above_one_year")], 
              by = "id",
              all.x = TRUE)

merge.age <- merge.age[!merge.age$age_of_death_above_one_year %in% "NULL", ] 
drop <- c("as.y")
merge.age <- merge.age[ , !(names(merge.age) %in% drop)]
names(merge.age)[names(merge.age) == 'as.x'] <- 'as'
df.clean <- merge.age

# Random subset to test
# test <- dummy.data.frame(data.subset[sample(nrow(data.subset), 1000),], names = dummy.vars, sep = ".")
#write.table(clean.data, file=paste(root,"clean_data/clean_data.csv", sep = ""))
write.table(df.clean, file=paste(root,"clean_data/clean_data.txt", sep = ""), sep=",")


# !!! remove as.y and rename
# !!! add the response var