## STAT406 Deliverable 1

rm(list=ls(all=TRUE))

dat <- read.table(file.choose(), sep=",", header=TRUE)

WD <- paste(getwd(), "/STAT406-final-project/", "data", sep="")
setwd("~/STAT406-final-project/data")
file_names <- WD #where you have your files


files <- list.files(path = WD,pattern = ".csv")
# temp <- lapply(files, fread, sep=",")
# data <- rbindlist( temp )
raw.data <- do.call(rbind,lapply(files,read.csv))


# statename <- ifelse(row.data$state=="20","JH",ifesle(row.data$state=="5"),"UT",
#                     ifesle(row.data$state=="8"),"RJ"),ifesle(row.data$state=="9"),"UP",
# ifesle(row.data$state=="10"),"BH",ifesle(row.data$state=="18"),"AS",
# ifesle(row.data$state=="21"),"OR",ifesle(row.data$state=="22"),"CT",
# ifesle(row.data$state=="23"),"MP")

library('plyr')
df1 <- data.frame(x=c(1:5),y=c(11:15))
df2 <- data.frame(x=c(1:5),y=c(11:15))
mylist <- list(df1 = df1, df2 = df2)

all <- ldply(mylist)

data.v2 <-data.frame(row.data,statename)
