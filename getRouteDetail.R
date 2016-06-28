#include jsonlite
library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)

source('decodeLine.R')
source('init.R')

time.df <- read.csv('data/1Timedcar2go_week.csv', header = T)

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
  
    car.route.single <- rbind(car.route.single, data.frame(car.route.temp, startTime = car.single.last$Time....Sys.time..[trip.ct]))
    
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

#trip time in mins
trip.info$Time <- round(trip.info$Time-5,0)

#convert to miles
trip.info <- mutate(trip.info, miles = Dist/1609)

#convert to time format
trip.info$startTime <- strptime(trip.info$startTime, "%Y-%m-%d %H:%M:%S")

car.route.sub <- filter(car.route, name %in% car.name)
car.app.sub <- filter(car.first.app, name %in% car.name)


trip.info <- read.csv('data/Mondaytrip.csv', header = T)
#plot trip info
plot.dist <- ggplot(data=trip.info, aes(trip.info$miles)) + 
  geom_histogram(aes(fill = ..count..), breaks=seq(0, 15, by = 1) ) + 
  title_with_subtitle("Travel Distance of Each Trip", 'Monday 12/07/2015') +
  labs(x="Distance (miles)", y="Count") +
  theme_tws(base_size = 20) + theme(legend.position=c(0.9, 0.5))

png('plots/dist.png', width=640, height=480)

print(plot.dist)

dev.off()


plot.time <- ggplot(data=trip.info, aes(trip.info$Time)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 5, breaks=seq(0, 150, by = 5)) + 
  title_with_subtitle("Travel Time of Each Trip", 'Monday 12/07/2015') +
  labs(x="Time (minutes)", y="Count") +
  theme_tws(base_size = 20) + theme(legend.position=c(0.9, 0.5))

png('plots/time.png', width=640, height=480)

print(plot.time)

dev.off()


plot.start <- ggplot(trip.info, aes(x=startTime)) + geom_histogram(aes(fill = ..count..),binwidth=1200) +
  title_with_subtitle("Number of Trips", 'Monday 12/07/2015') +
  labs(x="Start time", y="Count") +
  theme_tws(base_size = 20) + theme(legend.position=c(0.5, 0.7))

png('plots/start.png', width=640, height=480)

print(plot.start)

dev.off()


#output files
write.csv(car.route, file = 'data/Mondayroute.csv')
write.csv(trip.info, file = 'data/Mondaytrip.csv')

