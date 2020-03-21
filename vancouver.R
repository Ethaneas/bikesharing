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
setwd("C:/Users/ethansim/Documents/bikesharing/vancouver")

### read
df_spring <- fread("df_spring.csv", header = T)
setDT(df_spring)
df_summer <- fread("df_summer.csv", header = T)
setDT(df_summer)
df_winter <- fread("df_winter.csv", header = T)
setDT(df_winter)


spring <- lapply(1:4, FUN = function(x){
  df <- paste0('df',x) 
  file <- paste0('2018-0',x,'.csv')
  df <- fread(file)
  df <- df[,c(1:6)]
  names(df) <- c("start.date", "end.date", "account.id", "bike.id", "start.station.name", "end.station.name")
  df <- df[,.(start.date = as.POSIXct(start.date, format = "%Y-%m-%d %H:%M")
              , end.date = as.POSIXct(end.date, format = "%Y-%m-%d %H:%M")
              , start.station.name
              , end.station.name)]
})

summer <- lapply(5:8, FUN = function(x){
  df <- paste0('df',x) 
  file <- paste0('2018-0',x,'.csv')
  df <- fread(file)
  df <- df[,c(1:6)]
  names(df) <- c("start.date", "end.date", "account.id", "bike.id", "start.station.name", "end.station.name")
  df <- df[,.(start.date = as.POSIXct(start.date, format = "%Y-%m-%d %H:%M")
              , end.date = as.POSIXct(end.date, format = "%Y-%m-%d %H:%M")
              , start.station.name
              , end.station.name)]
})

winter <- lapply(9:12, FUN = function(x){
  df <- paste0('df',x) 
  file <- if(nchar(x) == 1){
    paste0('2018-0',x,'.csv')
  } else{
    paste0('2018-',x,'.csv')
  }
  df <- fread(file)
  df <- df[,c(1:6)]
  names(df) <- c("start.date", "end.date", "account.id", "bike.id", "start.station.name", "end.station.name")
  df <- df[,.(start.date = as.POSIXct(start.date, format = "%Y-%m-%d %H:%M")
              , end.date = as.POSIXct(end.date, format = "%Y-%m-%d %H:%M")
              , start.station.name
              , end.station.name)]
})

df_spring <- do.call(rbind,spring) %>% na.omit() 
dim(df_spring)
# df_spring contains 122,095 observations and 4 variables
# df_spring from Jan 2018 - April 2018

df_summer <- do.call(rbind,summer) %>% na.omit() 
dim(df_summer)
# df_summer contains 401,390 observations and 4 variables
# df_summer from May 2018 - August 2018

df_winter <- do.call(rbind,winter) %>% na.omit() 
dim(df_winter)
# df_winter contains 209,587 observations and 4 variables
# df_winter from Sept 2018 - Dec 2018

# removing weekends
df_spring <- df_spring[order(start.date)][is.weekend(start.date) == F,]
dim(df_spring)

df_summer <- df_summer[order(start.date)][is.weekend(start.date) == F,]
dim(df_summer)

df_winter <- df_winter[order(start.date)][is.weekend(start.date) == F,]
dim(df_winter)

write.csv(df, "df_spring.csv")
write.csv(df, "df_summer.csv")
write.csv(df, "df_winter.csv")


# Community detection -----------------------------------------------------

### read
summer.membership <- fread("summer_membership.csv")
setDT(summer.membership)
winter.membership <- fread("winter_membership.csv")
setDT(winter.membership)
spring.membership <- fread("spring_membership.csv")
setDT(spring.membership)


## overall
overall_membership <- function(df){
  od.e <- as.data.frame(table(df[,start.station.name],df[,end.station.name]))
  names(od.e) <- c('origin', 'destination', 'weight')
  # in this case, weight is the count of trips from origin to destination
  # removing nodes with less than one edge a day on average
  # edge dataset
  od.e <- od.e[od.e$weight >= 1,]
  od.e.txt <- data.matrix(od.e)
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
  
  membership <- matrix(0,NROW(fc$membership),1)
  membership <- data.frame(membership)
  membership$fast_greedy <- fc$membership
  membership$membership <- NULL
  membership$walks <- fc3$membership
  membership$infomap <- fc4$membership
  membership$louvain <- fc5$membership
  membership$id <- V(g)$name
  membership <- membership[,c(5,1,2,3,4)]
  return(membership)
}

summer.membership <- overall_membership(df_summer)
winter.membership <- overall_membership(df_winter)
spring.membership <- overall_membership(df_spring)

write.csv(summer.membership, "summer_membership.csv")
write.csv(winter.membership, "winter_membership.csv")
write.csv(spring.membership, "spring_membership.csv")


## dynamic
summer.membership.hourly <- lapply(0:23, df = df_summer, FUN = function(h, df){
  df_h <- df[,hour(start.date)== h,]
  df_h <- df[hour(start.date)== h,]
  od.e <- as.data.frame(table(df_h[,start.station.name], df_h[,end.station.name]))
  names(od.e) <- c("origin","destination","weight")
  # in this case, weight is the count of trips from origin to destination
  # removing nodes with less than one edge a day on average
  # edge dataset
  od.e <- od.e[od.e$weight > 0,]
  od.e.txt <- data.matrix(od.e)
  g <- graph_from_data_frame(od.e, directed = F)
  gd <- graph_from_data_frame(od.e, directed = T)
  g <- simplify(g)
  # reproducible results
  set.seed(123123)
  #### fast greedy
  fc <- cluster_fast_greedy(g, merges = TRUE, modularity = TRUE,
                            membership = TRUE, weights = E(g)$weight)
  
  # fc2 <- edge.betweenness.community(g) too much memory
  fc3 <- cluster_walktrap(g, weights = E(g)$weight, steps = 4,
                          merges = TRUE, modularity = TRUE, membership = TRUE)
  fc4 <- cluster_infomap(gd, e.weights = NULL, v.weights = E(gd)$weight, nb.trials = 20,
                         modularity = TRUE)
  fc5 <- cluster_louvain(g, weights = E(g)$weights)
  membership <- matrix(0,NROW(fc$membership),1)
  membership <- data.frame(membership)
  membership$fast_greedy <- fc$membership
  membership$membership <- NULL
  membership$walks <- fc3$membership
  membership$infomap <- fc4$membership
  membership$louvain <- fc5$membership
  membership$id <- V(g)$name
  membership <- membership[,c(5,1,2,3,4)]
  return(membership)
})

winter.membership.hourly <- lapply(0:23, df = df_winter, FUN = function(h, df){
  df_h <- df[,hour(start.date)== h,]
  df_h <- df[hour(start.date)== h,]
  od.e <- as.data.frame(table(df_h[,start.station.name], df_h[,end.station.name]))
  names(od.e) <- c("origin","destination","weight")
  # in this case, weight is the count of trips from origin to destination
  # removing nodes with less than one edge a day on average
  # edge dataset
  od.e <- od.e[od.e$weight > 0,]
  od.e.txt <- data.matrix(od.e)
  g <- graph_from_data_frame(od.e, directed = F)
  gd <- graph_from_data_frame(od.e, directed = T)
  g <- simplify(g)
  # reproducible results
  set.seed(123123)
  #### fast greedy
  fc <- cluster_fast_greedy(g, merges = TRUE, modularity = TRUE,
                            membership = TRUE, weights = E(g)$weight)
  
  # fc2 <- edge.betweenness.community(g) too much memory
  fc3 <- cluster_walktrap(g, weights = E(g)$weight, steps = 4,
                          merges = TRUE, modularity = TRUE, membership = TRUE)
  fc4 <- cluster_infomap(gd, e.weights = NULL, v.weights = E(gd)$weight, nb.trials = 20,
                         modularity = TRUE)
  fc5 <- cluster_louvain(g, weights = E(g)$weights)
  membership <- matrix(0,NROW(fc$membership),1)
  membership <- data.frame(membership)
  membership$fast_greedy <- fc$membership
  membership$membership <- NULL
  membership$walks <- fc3$membership
  membership$infomap <- fc4$membership
  membership$louvain <- fc5$membership
  membership$id <- V(g)$name
  membership <- membership[,c(5,1,2,3,4)]
  return(membership)
})

spring.membership.hourly <- lapply(0:23, df = df_winter, FUN = function(h, df){
  df_h <- df[,hour(start.date)== h,]
  df_h <- df[hour(start.date)== h,]
  od.e <- as.data.frame(table(df_h[,start.station.name], df_h[,end.station.name]))
  names(od.e) <- c("origin","destination","weight")
  # in this case, weight is the count of trips from origin to destination
  # removing nodes with less than one edge a day on average
  # edge dataset
  od.e <- od.e[od.e$weight > 0,]
  od.e.txt <- data.matrix(od.e)
  g <- graph_from_data_frame(od.e, directed = F)
  gd <- graph_from_data_frame(od.e, directed = T)
  g <- simplify(g)
  # reproducible results
  set.seed(123123)
  #### fast greedy
  fc <- cluster_fast_greedy(g, merges = TRUE, modularity = TRUE,
                            membership = TRUE, weights = E(g)$weight)
  
  # fc2 <- edge.betweenness.community(g) too much memory
  fc3 <- cluster_walktrap(g, weights = E(g)$weight, steps = 4,
                          merges = TRUE, modularity = TRUE, membership = TRUE)
  fc4 <- cluster_infomap(gd, e.weights = NULL, v.weights = E(gd)$weight, nb.trials = 20,
                         modularity = TRUE)
  fc5 <- cluster_louvain(g, weights = E(g)$weights)
  membership <- matrix(0,NROW(fc$membership),1)
  membership <- data.frame(membership)
  membership$fast_greedy <- fc$membership
  membership$membership <- NULL
  membership$walks <- fc3$membership
  membership$infomap <- fc4$membership
  membership$louvain <- fc5$membership
  membership$id <- V(g)$name
  membership <- membership[,c(5,1,2,3,4)]
  return(membership)
})

# Visualization -----------------------------------------------------------
setwd("C:/Users/ethansim/Documents/bikesharing/vancouver")
register_google(key = 'AIzaSyBaubMHXcsneKKUuh1lFJFp8CYxYYTR99k')

### read dataset
map.dataset <- fread("map.coordinates.csv")
setDT(map.dataset)
map.dataset <- map.dataset[, .(stations,lon,lat)]


## prepare output data
# station coordinates - longitude and latitude
stations <- function(df){
  start.stations <- df[,.(station.name = start.station.name)]
  end.stations <- df[,.(station.name = end.station.name)]
  stations <- rbind(start.stations,end.stations) %>% distinct()
  stations <- stations[, .(station.name = paste0(station.name, ', Vancouver, British Columbia'))][,station.name]
  stations
}

stations <- stations(df)
output <- lapply(stations, FUN = function(stations){
  cbind(stations, geocode(stations, output = "latlona", source = "google"))
})

map.dataset <- do.call(rbind.fill, output) %>% na.omit()
# contains NA addresses that are removed and will be filled for future research
setDT(map.dataset)
write.csv(map.dataset,"map.coordinates.csv")


# cluster objects: fast_greedy, walks, infomap, louvain
summer.membership <- summer.membership[,c(-1)]
spring.membership <- spring.membership[,c(-1)]
winter.membership <- winter.membership[,c(-1)]
summer.membership
spring.membership
winter.membership

summer.membership.hourly
spring.membership.hourly
winter.membership.hourly

## overall visualization
head(map.dataset)
vancouver.map <- get_googlemap(center=c(lon=-123.115493,lat=49.284025),scale=2,size=c(350,350),zoom=12,maptype="roadmap")
plot <- list()
overall.plot <- function(df, map.data, vancouver.map){
  df <- df[,.(stations = paste0(df[,id],', Vancouver, British Columbia'), fast_greedy, walks, infomap, louvain)]
  cluster.set <- merge(df, map.data, by = 'stations') %>% na.omit()
  vancouver.station <- ggmap(vancouver.map) + 
    geom_point(data=map.data, alpha=0.8, aes(lon,lat), color="red") +
    coord_map("mercator")
  infomap <- ggmap(vancouver.map) +
    geom_point(data=cluster.set, aes(lon, lat), color=cluster.set[,infomap])
  louvain <- ggmap(vancouver.map) +
    geom_point(data=cluster.set, aes(lon, lat), color=cluster.set[,louvain])
  greedy <- ggmap(vancouver.map) +
    geom_point(data=cluster.set, aes(lon, lat), color=cluster.set[,fast_greedy])
  walks <- ggmap(vancouver.map) +
    geom_point(data=cluster.set, aes(lon, lat), color=cluster.set[,walks])
  plot[[1]] <- vancouver.station
  plot[[2]] <- infomap
  plot[[3]] <- louvain
  plot[[4]] <- greedy
  plot[[5]] <- walks
  return(plot)
}

summer.plots <- overall.plot(summer.membership, map.dataset, vancouver.map)
spring.plots <- overall.plot(spring.membership, map.dataset, vancouver.map)
winter.plots <- overall.plot(winter.membership, map.dataset, vancouver.map)

## 3 
summer.membership.hourly[[1]]
spring.membership.hourly
winter.membership.hourly

plot.hourly <- function(df){
  
  for(i in 1:24){
    
  }
}

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
