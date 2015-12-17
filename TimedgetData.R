#include jsonlite
library(jsonlite)

for (day in 1:7){

  car.df.time <- data.frame()
  
  for (i in seq(1,288)){
    rm(car.df)
  
    city.info <- read.csv('data/cities.csv', header = T)
    
    car.df <- data.frame()
    
    for (city in city.info$City[1]){
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
      
      car.df <- data.frame(rbind(car.df, car.df.raw), Time = Sys.time())
      
    }
  
    car.df.time <- rbind(car.df.time, car.df)
    
    print(Sys.time())
    Sys.sleep(299)
    
  }
  
  
  car.df.time$fuel <- as.numeric(as.character(car.df.time$fuel))
  write.csv(car.df.time, file = paste(day, 'data/Timedcar2go_week.csv', sep = ''))
  
}


