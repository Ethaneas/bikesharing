library('data.table')
library('dplyr') # data manipulation
library('tidyr') # data manipulation
library('chron') # contains the is.weekend function
library("lubridate") # contains functions related to dates


# Data Preparation ----
# read in dataset
df1 <- fread("03Jul2019-09Jul2019.csv")
df2 <- fread("10Jul2019-16Jul2019.csv")
df3 <- fread("17Jul2019-23Jul2019.csv")
df4 <- fread("24Jul2019-30Jul2019.csv")

dataset <- rbind(df1,df2,df3,df4)
dim(dataset)
## dataset contains 9 variables and 1019888 observations

# clean dataset
dataset_clean <- na.omit(dataset)
dim(dataset_clean)
## clean dataset contains 9 variables and 1019888 observations

df <- dataset_clean
head(df)
str(df)
names(df) <- c("rental.id", "duration", "bike.id"
               , "end.date", "end.station.id", "end.station.name"
               , "start.date", "start.station.id", "start.station.name")
str(df)

# converting to time
df <- df[, .(start.date = strptime(start.date, "%d/%m/%Y %H:%M")
       , end.date = strptime(end.date, "%d/%m/%Y %H:%M")
       , rental.id
       , bike.id
       , duration
       , start.station.id
       , start.station.name
       , end.station.id
       , end.station.name)]
df[order(start.date)]

# removing maintenance station
start.station <- df[, .(station.id = unique(start.station.id))]
df <- df[(end.station.id %in% start.station$station.id),]

# removing weekends
df <- (df[is.weekend(start.date) == F,])
dim(df)


# Community Detection ----
# https://github.com/konstantinklemmer/bikecommclust/
# Network analysis overview: https://www.youtube.com/watch?v=2_Q7uPAl34M










