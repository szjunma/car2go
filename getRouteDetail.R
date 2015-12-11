#include jsonlite
library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)

source('decodeLine.R')

setwd("C:/Users/Jun/Dropbox/Data project/car2go")
rm(list=ls())

time.df <- read.csv('1Timedcar2go_week.csv', header = T)

car.first.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = F))

car.first.app <- car.first.app[duplicated(car.first.app[, 8]) | duplicated(car.first.app[, 8], fromLast = T),]

car.last.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = T))

car.last.app <- car.last.app[duplicated(car.last.app[, 8]) | duplicated(car.last.app[, 8], fromLast = T),]

A <- rbind(car.first.app, car.last.app)


car.name <- as.character(unique(car.first.app$name))
car.route <- data.frame()
trip.info <- data.frame()

for (name in car.name){
  
  car.single <- car.first.app[car.first.app$name == name,]
  car.single.last <- car.last.app[car.last.app$name == name,]
  
  car.route.single <- data.frame()
  trip.single <- data.frame()
  
  for (trip.ct in 1:(nrow(car.single)-1)){  
    
    car.route.all <- route(from = c(car.single[trip.ct, 12], car.single[trip.ct, 13]) , 
                            to = c(car.single[trip.ct+1, 12], car.single[trip.ct+1, 13]), structure = 'route', mode = 'driving',
                            output = 'all')
    
    car.route.temp <- decodeLine( car.route.all$routes[[1]]$overview_polyline$points )
  
    car.route.single <- rbind(car.route.single, car.route.temp)
    
    trip.single.temp <- data.frame(name = name, Dist = car.route.all$routes[[1]]$legs[[1]]$distance$value, 
                                   Fuel = car.single$fuel[trip.ct]-car.single$fuel[trip.ct+1], 
                                   Time = as.numeric(difftime(car.single$Time....Sys.time..[trip.ct+1], car.single.last$Time....Sys.time..[trip.ct], units = "mins")),
                                   startTime = car.single.last$Time....Sys.time..[trip.ct]
                                   )
    trip.single <- rbind(trip.single.temp, trip.single)
  }
  
  car.route <- rbind(car.route, data.frame(car.route.single, name = name))
  trip.info <- rbind(trip.single, trip.info)
  
}

trip.info$Time <- round(trip.info$Time-5,0)
trip.info <- mutate(trip.info, miles = Dist/1609)
trip.info$startTime <- strptime(trip.info$startTime, "%Y-%m-%d %H:%M:%S")

car.route.sub <- filter(car.route, name %in% car.name)
car.app.sub <- filter(car.first.app, name %in% car.name)



map <- get_map(location = 'austin', zoom = 13)
plot.route <- ggmap(map) + geom_path(aes(x = lon, y = lat, color = name), size = 1, data = car.route.sub, lineend = 'butt') + 
  geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme(legend.position="none")
plot.route


map <- get_map(location = 'austin', zoom = 11)
plot.route <- ggmap(map) + 
  geom_path(aes(x = lon, y = lat, color = name), size = 1, data = route_df, lineend = 'round') + 
#   geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.first.app, size =3)+ 
  theme(legend.position="none")

plot.route

plot.dist <-ggplot(data=trip.info, aes(trip.info$miles)) + 
  geom_histogram(aes(fill = ..count..), breaks=seq(0, 15, by = 1) ) + 
  labs(title="Histogram for distance") +
  labs(x="Distance (miles)", y="Count")

plot.dist

plot.time <-ggplot(data=trip.info, aes(trip.info$Time)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 5, breaks=seq(0, 150, by = 5)) + 
  labs(title="Histogram for time") +
  labs(x="Time (minutes)", y="Count")

plot.time

ggplot(trip.info, aes(x=startTime)) + geom_histogram(aes(fill = ..count..),binwidth=1200) +
  labs(title="Number of trips during a day") +
  labs(x="Start time", y="Count")




write.csv(car.route, file = 'Mondayroute.csv')
write.csv(trip.info, file = 'Mondaytrip.csv')

