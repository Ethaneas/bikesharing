# Libraries ---------------------------------------------------------------
libraries <- c('data.table', 'dplyr', 'chron'
               , 'lubridate', 'igraph', 'ggplot2'
               , 'ggthemes', 'NbClust', 'stringr'
               , 'mapproj', 'bit64', 'ggmap'
               , 'sna', 'gridExtra', 'RColorBrewer'
               , 'ggraph', 'ggalluvial', 'alluvial'
               , 'forcats', 'grDevices')
lapply(libraries, require, character.only = T)

# Dataset prep ------------------------------------------------------------
# read in dataset
rm(list=ls())
setwd("C:/Users/ethansim/Documents/bikesharing/london")
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

write.csv(df, "df.csv")
df <- fread("df.csv")
setDT(df)

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

membership <- fread("communities.csv")
setDT(membership)

# Dynamic analysis --------------------------------------------------------
## dataset
head(df)
dim(df)

setwd("C:/Users/ethansim/Documents/bikesharing/infomap-0.x")
output <- paste0("/mnt/c/Users/ethansim/Documents/bikesharing/infomap-0.x")
# get hours clusters and trees for infomap
for(h in 0:23){
  df_h <- df[hour(start.date)== h,]
  od.e <- as.data.frame(table(df_h[,start.station.id], df_h[,end.station.id]))
  names(od.e) <- c("origin","destination","weight")
  od.e <- od.e[od.e$weight > 0,]
  od.e.txt <- data.matrix(od.e)
  write.table(od.e.txt,paste0("edges",h,".txt"),row.names = FALSE,col.names = FALSE,sep = " ")
  infomapcommand <- paste0("bash && ./Infomap ","edges",h,".txt ",output," -N 20 --tree --map --directed")
  system(command = infomapcommand)
}



infomapcommand <- paste0("bash && ./Infomap ","edges",0,".txt ",output," -N 20 --tree --map --directed")


# get overall clusters and trees for infomap
# od.e <- as.data.frame(table(df[,start.station.id], df[,end.station.id]))
# names(od.e) <- c("origin","destination","weight")
# od.e <- od.e[od.e$weight > 0,]
# od.e.txt <- data.matrix(od.e)
# write.table(od.e.txt,paste0("edges.overall.txt"),row.names = FALSE,col.names = FALSE,sep = " ")
# infomapcommand <- paste0("bash && ./Infomap edges.overall.txt ",output," -N 20 --tree --map --directed")
# system(command = infomapcommand)

## summarizing result
temp <- list.files(pattern = "*.tree")
outputs <- lapply(temp,function(x){
  imapResults <- read.table(x,header=F,sep=" ") #Read output tree file from infomap
  imapResults <- cbind(imapResults,str_split_fixed(imapResults$V1,":",2)) 
  #Split cluster and index column
  imapResults <- imapResults[ -c(1)] #Delete redundant column
  name <- str_replace(x,"edges","hour")
  name <- str_replace(name,".tree","")
  colnames(imapResults) <- c("flow","name1","ID",name,"clusterID") #Name columns
  imapResults <- imapResults[,c("ID",name)]
  return(imapResults)
})

results <- data.frame("ID" = seq(1,724))

for (i in seq_along(outputs)){
  results <- results %>%
  merge(outputs[[i]], by="ID", all = T)
}

results <- results[, order(names(results))]
results <- results %>%
  select(ID, everything())
write.csv(results,"results.csv")
setwd("C:/Users/ethansim/Documents/bikesharing/infomap-0.x")
results <- fread("results.csv")
setDT(results)
results <- results[,c(-1)]

# Visualization -----------------------------------------------------------
setwd("C:/Users/ethansim/Documents/bikesharing/vancouver")
register_google(key = 'AIzaSyBaubMHXcsneKKUuh1lFJFp8CYxYYTR99k')

## prepare output data
# station coordinates
stations <- function(df){
  start.stations <- df[,.(station.name = start.station.name)]
  end.stations <- df[,.(station.name = end.station.name)]
  stations <- rbind(start.stations,end.stations) %>% distinct()
  stations <- stations[, .(station.name = paste0(station.name, ', London, United Kingdom'))][,station.name]
  stations
}
stations <- stations(df)
output <- lapply(stations, FUN = function(stations){
  cbind(stations, geocode(stations, output = "latlona", source = "google"))
})
map.dataset <- do.call(rbind, output)
setDT(map.dataset)
write.csv(map.dataset,"map.coordinates.csv")

# read dataset
map.dataset <- fread("map.coordinates.csv")
setDT(map.dataset)
map.dataset <- map.dataset[, .(stations,lon,lat)]

# hourly infomap results
head(results)

# cluster objects: fast_greedy, walks, infomap, louvain
head(membership)

# enhance cluster objects with coordinates
start.stations <- df[,.(stations = paste0(start.station.name, ', London, United Kingdom'), id = as.character(start.station.id))]
end.stations <- df[,.(stations = paste0(end.station.name, ', London, United Kingdom'), id = as.character(end.station.id))]
stations <- rbind(start.stations,end.stations) %>% distinct()
head(stations)
cluster.set <- merge(stations, membership, by = 'id') %>% merge(map.dataset, by = 'stations')

## 1 using map.dataset on google map for london
london.stations <- ggmap(get_googlemap(center=c(lon=-0.118092,lat=51.509865),scale=2,size=c(360,360),zoom=11,maptype="roadmap")) + 
  geom_point(data=map.dataset, alpha=0.7, aes(lon,lat), color="red") + #Stations only
  coord_map("mercator")

## 2 comparison of community detection algorithms: BSS stations are colored according to their respective community assignment across the four techniques
# using map.dataset, df id, and membership
head(cluster.set)
london.map <- get_googlemap(center=c(lon=-0.118092,lat=51.509865),scale=2,size=c(360,360),zoom=11,maptype="roadmap", color = "bw")

# fast_greedy <- ggmap(london.map) +
#   geom_point(data = cluster.set, aes(lon, lat), color = cluster.set[,infomap])
# walks <- ggmap(london.map) +
#   geom_point(data = cluster.set, aes(lon, lat), color = cluster.set[,walks])
infomap <- ggmap(london.map) +
  geom_point(data = cluster.set, aes(lon, lat), color = cluster.set[,infomap])
louvain <- ggmap(london.map) +
  geom_point(data = cluster.set, aes(lon, lat), color = cluster.set[,louvain])

p_comp <- arrangeGrob(infomap,louvain,nrow=1)
plot(p_comp)


## 3 
head(results)
temp <- melt(results, id.vars = "ID") %>% na.omit()
temp <- temp[,.(station.id = ID, hour = as.numeric(str_replace(variable,"hour", "")), cluster = value)][order(hour)]

#Create alluvial diagram of dynamic cluster assignment
p_dyn <- ggplot(data = temp,aes(x = hour, stratum = cluster, alluvium=station.id,fill=cluster)) + 
  geom_flow(color = "darkgray",width = 1/1000,linetype="blank") +
  ylab("Station ID") +
  xlab("Time") +
  #scale_fill_manual(palette="rainbow") +
  theme_bw() +
  theme(legend.position="bottom")