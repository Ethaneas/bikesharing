# Libraries ---------------------------------------------------------------
library('data.table')
library('dplyr') # data manipulation
library('tidyr') # data manipulation
library('chron') # contains the is.weekend function
library('lubridate') # contains functions related to dates
library('igraph')
library('ggplot2')
library('ggthemes')
library('NbClust')
library('stringr')

# Dataset prep ------------------------------------------------------------
# read in dataset
rm(list=ls())
setwd("C:/Users/ethansim/Documents/bikesharing")
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

# Community detection -----------------------------------------------------
##  nodes
od.e <- as.data.frame(table(df[,start.station.id],df[,end.station.id]))
dim(od.e)
names(od.e) <- c('origin', 'destination', 'weight')
# in this case, weight is the count of trips from origin to destination

# removing nodes with less than one edge a day on average.
od.e <- od.e[od.e$weight >= 1,]
dim(od.e)
od.e.txt <- data.matrix(od.e)


## output
# visualization
write.csv(od.e,"edges.csv")
# output for infomap
write.table(od.e.txt,"edges.txt",row.names = F,col.names = F,sep = " ")


# graph objects
g <- graph_from_data_frame(od.e, directed = F)
gd <- graph_from_data_frame(od.e, directed = T)

g <- simplify(g)

# reproducible results
set.seed(123)

#### fast greedy
fc <- cluster_fast_greedy(g, merges = TRUE, modularity = TRUE,
                           membership = TRUE, weights = E(g)$weight)

# fc2 <- edge.betweenness.community(g) too much memory
fc3 <- cluster_walktrap(g, weights = E(g)$weight, steps = 4,
                        merges = TRUE, modularity = TRUE, membership = TRUE)
fc4 <- cluster_infomap(gd, e.weights = NULL, v.weights = E(gd)$weight, nb.trials = 20,
                       modularity = TRUE)
fc5 <- cluster_louvain(g, weights = E(g)$weights)


## create membership matrix
# empty matrix
membership <- matrix(0,NROW(fc$membership),1)
membership <- data.frame(membership)
membership$fast_greedy <- fc$membership
membership$membership <- NULL
membership$walks <- fc3$membership
membership$infomap <- fc4$membership
membership$louvain <- fc5$membership
membership$id <- V(g)$name
membership <- membership[,c(5,1,2,3,4)]
write.csv(membership,"communities.csv")

# Dynamic analysis --------------------------------------------------------
## dataset
head(df)
dim(df)

setwd("C:/Users/ethansim/Documents/bikesharing/infomap-0.x")
output <- paste0("/mnt/c/Users/ethansim/Documents/bikesharing/infomap-0.x")
# get hours
#for(h in 0:23){
  df_h <- df[hour(start.date)== 1,]
  od.e <- as.data.frame(table(df_h[,start.station.id], df_h[,end.station.id]))
  names(od.e) <- c("origin","destination","weight")
  od.e <- od.e[od.e$weight > 0,]
  od.e.txt <- data.matrix(od.e)
  write.table(od.e.txt,paste0("edges",1,".txt"),row.names = FALSE,col.names = FALSE,sep = " ")
  infomapcommand <- paste0("bash && ./Infomap ","edges",1,".txt ",output," -N 20 --tree --map --directed")
  system(command = infomapcommand)
# }


