library(gridExtra)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)

source('decodeLine.R')
source('init.R')

car.route.sub <- read.csv('data/Mondayroutetime.csv', header = T)

plot.list <-list()

for (i in 1:4){
  map <- get_map(location = 'austin', zoom = 10+i)
  plot <- ggmap(map) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
    #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
    theme_tws_map() + theme(legend.position="none",axis.title = element_blank())
  
  plot.list[[i]] <- plot
}


png('plotall.png', width=1280, height=1280)

grid.arrange(plot.list[[1]], plot.list[[2]], plot.list[[3]], plot.list[[4]], ncol=2, nrow =2)

dev.off()
map <- get_map(location = 'austin', zoom = 12)
car.route.hour <- car.route %>%
  mutate(hour = as.numeric(format(as.POSIXct(startTime), '%H'))) 

for (i in 0:23){
  car.route.sub <- car.route.hour %>%
    filter(hour == i)
  plot <- ggmap(map) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.5, color = '#D55E00') + 
    annotate("text", x = -97.8, y = 30.33, label = paste0(i, ":00 -", i+1, ":00"), size = 8) + 
    theme_tws_map() + theme(legend.position="none",axis.title = element_blank())
  plot.list[[i+1]] <- plot
  png(paste0(i, '.png'), width=640, height=640)
  print(plot)
  dev.off()
}
dev.off()
png(paste0(i, '.png'), width=640, height=640)
plot
dev.off()
png('test.png', width=640, height=640)

plot
dev.off()

plot.list[[i]] <- plot
