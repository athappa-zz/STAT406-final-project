## STAT406 Deliverable 1

rm(list=ls(all=TRUE))

dat <- read.table(file.choose(), sep=",", header=TRUE)

WD <- paste(getwd(), "/STAT406-final-project/", "data", sep="")
setwd("~/STAT406-final-project/data")
file_names <- WD #where you have your files


files <- list.files(path = WD,pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
raw.data <- do.call(rbind,lapply(files,read.csv))


