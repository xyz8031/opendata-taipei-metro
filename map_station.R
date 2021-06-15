library(maptools)
library(ggfortify)
library(ggplot2)
library(dplyr)

taiwan_shp <- readShapeSpatial("data/taiwan_ shapefile/COUNTY_MOI_1090820.shp")


taiwan_map <- fortify(taiwan_shp)
mask = taiwan_shp$COUNTYNAME %in% c('臺北市','新北市','桃園市','基隆市')

ggplot() +
  geom_point(data = station, aes(x = lon, y = lat, col = line)) + 
  scale_color_manual(values = cols) + 
  xlim(121.15, 121.65) +
  ylim(24.9, 25.2) +
  geom_path(data = taiwan_shp[mask, ], aes(x = long, y = lat, group = group)) + 
  coord_map() + 
  xlab('') + ylab('') + 
  theme(legend.position = 'None',
        panel.grid = element_blank(),
        axis.text = element_blank())
