library(gridExtra)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)

setwd("C:/Users/Jun/Dropbox/Data project/car2go")
source('decodeLine.R')
source('init.R')

rm(list=ls())

car.route.sub <- read.csv('data/Mondayroute.csv', header = T)


map1 <- get_map(location = 'austin', zoom = 11)
plot1 <- ggmap(map1) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
  #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme_tws_map() + theme(legend.position="none",axis.title = element_blank())

map2 <- get_map(location = 'austin', zoom = 12)
plot2 <- ggmap(map2) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
  #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme_tws_map() + theme(legend.position="none",axis.title = element_blank())

map3 <- get_map(location = 'austin', zoom = 13)
plot3 <- ggmap(map3) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
  #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme_tws_map() + theme(legend.position="none",axis.title = element_blank())

map4 <- get_map(location = 'austin', zoom = 14)
plot4 <- ggmap(map4) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
  #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme_tws_map() + theme(legend.position="none",axis.title = element_blank())



png('plotall3.png', width=1280, height=1280)

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow =2)

dev.off()


