#include jsonlite
library(jsonlite)
library(ggplot2)
library(ggmap)


rm(list=ls())

city.info <- read.csv('cities.csv', header = T)

car.df <- data.frame()

for (city in city.info$City){
  car2goURL <- paste('https://www.car2go.com/api/v2.1/vehicles?loc=', city, '&oauth_consumer_key=car2gowebsite&format=json', sep = '' )

  #import from JSON
  car2goData <- fromJSON(txt = car2goURL)
  
  #identify vehicles having charging attibutes or not
  if (length(colnames(car2goData[[1]])) == 10){
    # cities with EVs have a charging attribute
    #unlist and save
    
    coord <- data.frame(matrix(unlist(car2goData[[1]][3]), ncol=3, byrow = T))
    car.df.raw <- data.frame(matrix(unlist(car2goData[[1]]), ncol=12, byrow = F), City = city)[c(-5, -3, -4)]
        

    car.df.raw <- cbind(car.df.raw, coord)
    
    colnames(car.df.raw) <- c(colnames(car2goData[[1]])[1:2],colnames(car2goData[[1]])[4:10], 'City', 'Longitude', 'Latitude', 'Altitude')

    head(car.df)
    
    
    
    } else {
      
    #unlist and save
  
    coord <- data.frame(matrix(unlist(car2goData[[1]][2]), ncol=3, byrow = T))
    car.df.raw <- data.frame(matrix(unlist(car2goData[[1]]), ncol=11, byrow = F), City = city)[c(-2, -3, -4)]
    
    car.df.raw <- cbind(car.df.raw[,1], charging = NA, car.df.raw[,2:9], coord)
    colnames(car.df.raw) <- c(colnames(car2goData[[1]])[1], 'charging', colnames(car2goData[[1]])[3:9], 'City', 'Longitude', 'Latitude', 'Altitude')

  }
  
  car.df <- rbind(car.df, car.df.raw)
  
}

car.df$fuel <- as.numeric(as.character(car.df$fuel))
write.csv(car.df, file = 'car2go.csv')

map <- get_map(location = 'wien', zoom = 11)
ggmap(map)+ geom_point(aes(x = Longitude, y = Latitude, color = fuel, size = 2), data = car.df)+scale_colour_gradient2(low = 'red', high = 'green', midpoint = 60, mid = 'yellow')

car.status.city <- data.frame(City = city.info$City, cleanness = NA, fuel = NA)

for (city in city.info$City){
  
  car.status.city[car.status.city$City == city, ]$cleanness <- 1-sum(car.df[car.df$City == city,]$interior != 'GOOD')/nrow(car.df[car.df$City == city,])
  car.status.city[car.status.city$City == city, ]$fuel <- mean(car.df[car.df$City == city,]$fuel)
}


ggplot(data = car.status.city, aes(x=City, y=cleanness, fill = cleanness)) + geom_bar(stat="identity") +coord_flip()

ggplot(data = car.status.city ) + geom_point(aes(x=cleanness, y=fuel, color=City, size = 2))

