#include jsonlite
library(jsonlite)
library(ggplot2)
library(ggmap)


rm(list=ls())

car2goURL <- 'https://www.car2go.com/api/v2.1/vehicles?loc=sandiego&oauth_consumer_key=car2gowebsite&format=json'

#import from JSON
car2goData <- fromJSON(txt = car2goURL)

#unlist and save

coord <- data.frame(matrix(unlist(car2goData[[1]][3]), ncol=3, byrow = T))
car.df <- data.frame(matrix(unlist(car2goData[[1]]), ncol=12, byrow = F))[c(-5, -3, -4)]

car.df <- cbind(car.df, coord)

colnames(car.df) <- c(colnames(car2goData[[1]])[1:2],colnames(car2goData[[1]])[4:10], 'Longitude', 'Latitude', 'Altitude')
head(car.df)
write.csv(car.df, file = 'car2go.csv')

car.df$fuel <- as.numeric(as.character(car.df$fuel))

map <- get_map(location = 'san diego', zoom = 12)
ggmap(map)+ geom_point(aes(x = Longitude, y = Latitude, color = fuel, size = 2), data = car.df)+scale_colour_gradient2(low = 'red', high = 'green', midpoint = 60, mid = 'yellow')





