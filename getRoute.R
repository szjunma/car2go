#include jsonlite
library(jsonlite)
library(ggplot2)
library(ggmap)

setwd("C:/Users/Jun/Dropbox/Data project/car2go")
rm(list=ls())

time.df <- read.csv('1Timedcar2go_week.csv', header = T)

car.first.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = F))

car.first.app <- car.first.app[duplicated(car.first.app[, 8]) | duplicated(car.first.app[, 8], fromLast = T),]

car.last.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = T))

car.last.app <- car.last.app[duplicated(car.last.app[, 8]) | duplicated(car.last.app[, 8], fromLast = T),]

A <- rbind(car.first.app, car.last.app)

for (name in car.name){
  
  car.single <- car.first.app[car.first.app$name == name,]
  car.single.last <- car.last.app[car.last.app$name == name,]
  
  car.route.single <- data.frame()
  trip.single <- data.frame()
  
  for (trip.ct in 1:(nrow(car.single)-1)){  
    
    car.route.temp <- route(from = c(car.single[trip.ct, 12], car.single[trip.ct, 13]) , 
                           to = c(car.single[trip.ct+1, 12], car.single[trip.ct+1, 13]), structure = 'route', mode = 'driving',
                           output = 'simple')
    
    car.route.single <- rbind(car.route.single, car.route.temp)
    
    trip.single.temp <- data.frame(name = name, Dist = sum(car.route.temp$mile), 
                                   Fuel = car.single$fuel[trip.ct]-car.single$fuel[trip.ct+1], 
                                   Time = as.numeric(difftime(car.single$Time....Sys.time..[trip.ct+1], car.single.last$Time....Sys.time..[trip.ct], units = "mins")),
                                   startTime = car.single.last$Time....Sys.time..[trip.ct]
                                   )
    trip.single <- rbind(trip.single.temp, trip.single)
  }
  
  car.route <- rbind(car.route, data.frame(car.route.single, name = name))
  trip.info <- rbind(trip.single, trip.info)
  
}


car.name <- as.character(unique(car.first.app$name))
car.route <- data.frame()
trip.info <- data.frame()



trip.info$Time <- round(trip.info$Time,0)

car.route.sub <- filter(car.route, name %in% car.name[5])
car.app.sub <- filter(car.first.app, name %in% car.name[5])

map <- get_map(location = 'austin', zoom = 12)
plot.route <- ggmap(map) + 
  geom_path(aes(x = lon, y = lat, color = name), size = 1, data = car.route.sub, lineend = 'butt') + 
     geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme(legend.position="none")
plot.route


map <- get_map(location = 'austin', zoom = 11)
plot.route <- ggmap(map) + 
  geom_path(aes(x = lon, y = lat, color = Name), size = 1, data = car.route, lineend = 'round') + 
#   geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.first.app, size =3)+ 
  theme(legend.position="none")

plot.route

plot.dist <-ggplot(data=trip.info, aes(trip.info$Dist)) + 
  geom_histogram(aes(fill = ..count..), breaks=seq(0, 15, by = 1) ) + 
  labs(title="Histogram for distance") +
  labs(x="Distance (miles)", y="Count")

plot.dist

plot.time <-ggplot(data=trip.info, aes(trip.info$Time - 5)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 5) + 
  labs(title="Histogram for time") +
  labs(x="Time (minutes)", y="Count")

plot.time



write.csv(car.route, file = 'route.csv')
