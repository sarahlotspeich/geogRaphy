library(ggmap)

# Register your API key
ggmap::register_google(key = my_key)
ggmap::register_google(key = my_key, write = TRUE)

# Read in US Farmers Market Registry data 
farm <- read.csv("https://raw.githubusercontent.com/sarahlotspeich/geogRaphy/main/data/us_farmers_markets.csv", stringsAsFactors = F)

# Combine address = street, city, state, zip 
farm <- mutate(farm, address = paste(street, city, state, zip, sep = ", "))

# Geocode the first 10 addresses 
(farm_geo <- ggmap::geocode(location = farm$address[1:10], output = "more", source = "google"))

# Use mutate_geocode() to add longitude, latitude columns to farm data
## For time, just do this on the first 10 rows again
farm10 <- farm[1:10, ]
farm10$lon <- farm10$lat <- NULL
(farm10 <- ggmap::mutate_geocode(data = farm10, location = address, source = "google"))

# Subset to all 4 Knoxville farmers markets 
(knx_fm <- filter(farm, city == "Knoxville", state == "Tennessee"))

# Use Haversine formula to calculate the distance between 
## the first 2 Knoxville markets 
geosphere::distHaversine(p1 = knx_fm[1, c("lon", "lat")], 
                         p2 = knx_fm[2, c("lon", "lat")], 
                         r = 3958.8)

# And then do it using Law of Cosines
geosphere::distCosine(p1 = knx_fm[1, c("lon", "lat")], 
                      p2 = knx_fm[2, c("lon", "lat")], 
                      r = 3958.8)

# Calculate the 4 x 4 distance matrix for Knoxville markets
(dm_knx <- geosphere::distm(x = knx_fm[, c("lon", "lat")], 
                            fun = geosphere::distHaversine))

# Rescale: miles = 0.000621371 * meters 
(dm_knx <- geosphere::distm(x = knx_fm[, c("lon", "lat")], 
                            fun = geosphere::distHaversine) * 0.000621371)

# GO BIG! Calculate the full distance matrix for all US markets 
dm_us <- geosphere::distm(x = farm[, c("lon", "lat")], 
                          fun = geosphere::distHaversine) * 0.000621371
dim(dm_us)

# Use ggmap::route() to calculate the driving distance/time 
## Between the first 2 Knoxville farmers markets (again)

ggmap::route(from = knx_fm$address[1], to = knx_fm$address[2], 
             mode = "driving", structure = "legs")

(drive_route <- ggmap::route(from = knx_fm$address[1], to = knx_fm$address[2], 
                             mode = "driving", structure = "route"))
(walk_route <- ggmap::route(from = knx_fm$address[1], to = knx_fm$address[2], 
                            mode = "walking", structure = "route"))

# For the driving route, calculate the total 
sum(drive_route$miles, na.rm = TRUE) # travel distance and 
sum(drive_route$minutes, na.rm = TRUE) # travel time

# Do the same for the walking route 
sum(walk_route$miles, na.rm = TRUE) # travel distance and 
sum(walk_route$minutes, na.rm = TRUE) # travel time

# Get map data from ggplot2 for 48 contiguous United States 
library(ggplot2)
(contig_us <- ggplot2::map_data("state"))

# Create a map of the US
ggplot(us_map) + 
          geom_polygon(data = contig_us, 
                       aes(x = long, y = lat, group = group), 
                       color = "black") +
          theme_minimal() + 
          theme(axis.ticks = element_blank(), 
                axis.text = element_blank())+
          xlab('') + ylab('') 
us_map

# Layer the farmers market locations over your map of the US
us_map + geom_point(data = farm, 
                    aes(x = lon, y = lat), 
                    color = "darkgreen", 
                    size = 2, alpha = 0.6)

# Layer the farmers market locations over us_map
## color them by "Cheese" 
us_map + geom_point(data = farm, 
                    aes(x = lon, y = lat, color = Cheese), 
                    size = 2, alpha = 0.6) + 
  scale_color_manual(values = c("lightgray", "goldenrod2")) + 
  theme(legend.justification = c(0, 0), legend.position = c(0.83, 0),
        legend.background = element_rect(fill=alpha('white', 0.9)))

# Get a map of Knoxville, TN using Google Maps API 
knox <- get_map(location = 'Knoxville, Tennessee',  zoom = 10,
                maptype = 'roadmap', source = 'google', color = 'bw')
knox_map <- ggmap(knox) + 
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  xlab('') + ylab('')
knox_map

# Layer the Knoxville farmers markets over it
knox_map + geom_point(data = knx_fm, aes(x = lon, y = lat), 
                      color = "darkgreen", size = 8, alpha = 0.6)

# Recenter knox_map on between the first two markets 
colMeans(knx_fm[c(1:2), c("lon", "lat")]) 

knox <- get_map(location = c(lon = -83.91008, lat = 35.97048),  
                zoom = 15, maptype = 'roadmap', 
                source = 'google', color = 'bw')
knox_map <- ggmap(knox) + 
  geom_point(data = knx_fm[c(1:2), ], aes(x = lon, y = lat), 
             color = "darkgreen", size = 8, alpha = 0.6) + 
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  xlab('') + ylab('')
knox_map

# Layer the walking and driving routes calculated previously
knox_map + 
  geom_path(data = drive_route, aes(x = lon, y = lat, color = "driving")) + 
  geom_path(data = walk_route, aes(x = lon, y = lat, color = "walking")) + 
  theme(legend.justification = c(0, 0), legend.position = c(0.83, 0),
        legend.background = element_rect(fill=alpha("white", 0.4))) + 
  scale_color_manual(values = c("driving" = "firebrick2", 
                                "walking" = "dodgerblue2"), 
                     name = "mode")

# Use stat_density2d() to "smooth" over the farmers market locations 
us_map + 
  stat_density2d(data = flt_farm, geom="polygon",
                 aes(x = lon, y = lat, 
                     fill = ..level.., alpha = ..level..)) + 
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", 
                      guide = FALSE) + scale_alpha(guide = FALSE)

# Try using geom_hex() to create a different type of concentration plot
us_map + 
  geom_hex(data = flt_farm, aes(x = lon, y = lat)) + 
  scale_fill_gradient(low = "lightgreen", high = "darkgreen",
                      name = "Registered\nmarkets:") + 
  theme(legend.justification = c(0, 0), 
        legend.position = c(0.83, 0),
        legend.background = element_rect(fill=alpha('white', 0.4)))

# Aggregating the markets in state-level counts
## First, you need to transform farm$state to be all lower case 
farm$state <- tolower(farm$state)
## Then group by state and tabulate the number of markets
farm_summ <- group_by(farm, state)
(farm_summ <- summarize(farm_summ, num_markets = n()))
## Merge farm_summ with the contig_us map data used above
(farm_summ <- right_join(farm_summ, contig_us, by = c("state" = "region")))

# Create a choropleth map by plotting the state polygons and filling by num_markets
ggplot(farm_summ) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = num_markets), 
               color = "black") + 
  scale_fill_gradient2(low = "lightgreen", high = "darkgreen", 
                       name = "Registered\nmarkets:") +
  theme_minimal() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), 
        legend.justification = c(0, 0), 
        legend.position = c(0.83, 0),
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  xlab('') + ylab('') 

# Read in the state-level population data from GitHub
pop <- read.csv("https://raw.githubusercontent.com/sarahlotspeich/geogRaphy/main/data/state_pop_2019.csv", stringsAsFactors = FALSE)

# Join farm_summ with pop (on state)
(farm_summ <- inner_join(farm_summ, pop))

## Calculate the number of markets per person
(farm_summ <- mutate(farm_summ, 
                    per_person = num_markets / pop,
                    per_100thou = per_person * 100000))

ggplot(farm_summ) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = per_100thou), 
               color="black") + 
  scale_fill_gradient2(low = "lightgreen", high = "darkgreen", 
                       name = "Registered\nmarkets per\n100,000 people:") +
  theme_minimal() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), 
        legend.justification = c(0, 0), 
        legend.position = c(0.83, 0),
        legend.background = element_rect(fill=alpha('white', 0.4))) +
  xlab('') + ylab('') 
