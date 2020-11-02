# Data originally downloaded from 
## https://data.world/city-of-ny/drex-xx56

ny <- read.csv("https://raw.githubusercontent.com/sarahlotspeich/geogRaphy/main/data/ny_subway.csv", 
               stringsAsFactors = FALSE)

library(ggmap)
library(ggplot2)

ny_map <- get_map(location= 'Lower Manhattan, New York', maptype='roadmap', 
                       color = 'bw', source = 'google', zoom = 13)

map1 <- ggmap(ny_map) + 
  geom_point(data = ny, aes(x = long, y = lat), color = 'seagreen', size = 2, alpha = 0.25)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  xlab('') + ylab('') + ggtitle("New York City subway entrances")

map2 <- ggmap(newyork.map) + 
  geom_point(data = ny, aes(x = long, y = lat, col = aline), size = 2, alpha = 0.25)+
  scale_color_manual(values = c("slategrey", "royalblue"), name = "Access to A line?") + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 0),
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  xlab('') + ylab('') + ggtitle("New York City subway entrances")

map3 <- ggmap(newyork.map) + 
  geom_point(data = ny, aes(x = long, y = lat, col = aline, size = num), alpha = 0.25)+
  scale_color_manual(values = c("slategrey", "royalblue"), name = "Access to A line?") + 
  scale_size(name = "Number of lines:", breaks = c(1, 2, 3, 5, 10)) + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 0),
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  xlab('') + ylab('') + ggtitle("New York City subway entrances")