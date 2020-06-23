#script by Tobias Stalder
library(tidyverse)
library(dplyr)
library(sf)
library(ggmap)
library(ggalt)
library(ggthemes)
library(tmaptools)
library(gganimate)

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\Caribou_locations"
setwd(dir)

#source of the data: Seip DR, Price E (2019) Data from: Science update for the South Peace Northern Caribou (Rangifer tarandus caribou pop. 15) in British Columbia. Movebank Data Repository.

#load data:

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

view(locations)

#get birds with more than 2000 measurements
measurements = data.frame(sort(table(locations$animal_id)))

measurements_1000 = measurements %>%
  filter(Freq > 2000)

colnames(measurements_1000) <- c("animal_id", "n_meas")

#join back to locations:
locs = right_join(locations, measurements_1000, by = "animal_id")

#filter again in locations
locs_fin = locs %>%
  filter(n_meas > 2000)

#get a basemap
borders = ggmap::make_bbox(lon = longitude, lat = latitude, data = locs_fin)
basemap = get_stamenmap(bbox = borders)

#make a overview plot of where the birds are in the seasons (make a density contouring? or encircle?)
seasons = ggmap(basemap) +

  geom_hex(data = locs_fin, aes(x = longitude, y = latitude, color = season, fill = season),
           bins = 100,
             show.legend = TRUE,
             inherit.aes = FALSE,
           alpha = .3,
           size = .5) +
  # geom_point(data = locs_fin, aes(x = longitude, y = latitude, color = season),
  #            show.legend = FALSE,
  #            inherit.aes = FALSE,
  #            alpha = .3,
  #            size = .1) +
  scale_color_manual(values = c("#FF5500", "#0051FF")) +
  scale_fill_manual(values = c("#F56B1B", "#3067FF")) +
  labs(title = "Winter and Summer Locations of Caribous",
       subtitle = "Location Measurements of Caribous with 2000+ Measurements", 
       caption = "plot by @toeb18 \nSource:Movebank Data Repository") +
  coord_cartesian() +
  theme_map()

ggsave(seasons, filename = "seasons.png", width = 20, height = 20, units = "cm", dpi = 300)


#plot of individual birds
#filter top 10 birds

locs_top_birds = locs_fin %>%
  filter(animal_id %in% c("QU_car143",
                          "NA_car133",
                          "NA_car132",
                          "MO_car150",
                          "SC_car171",
                          "QU_car163",
                          "BP_car145",
                          "QU_car159",
                          "QU_car172",
                          "MO_car147"
                          ))
#discrete color values:
palette_explorer()
get_brewer_pal("Set1", n = 10)

birds = ggmap(basemap) +
  geom_hex(data = locs_top_birds, aes(x = longitude, y = latitude),
           bins = 100,
           show.legend = FALSE,
           inherit.aes = FALSE,
           alpha = .1,
           size = 2,
           color = "blue") +
  facet_wrap(.~animal_id) +
  scale_color_manual(values = c("#FF5500", "#0051FF")) +
  # scale_fill_manual(values = c(get_brewer_pal("Set1", n = 10))) +
  labs(title = "Locations of Caribous",
       subtitle = "Locations of top 10 Caribous with the most measurements", 
       caption = "plot by @toeb18 \nSource:Movebank Data Repository") +
  coord_cartesian() +
  theme_map()

ggsave(birds, filename = "birds.png", width = 20, height = 20, units = "cm", dpi = 300)

#gif of travel paths!

paths = ggmap(basemap) +
  geom_point(data = locs_top_birds, aes(x = longitude, y = latitude),
             show.legend = TRUE,
             inherit.aes = FALSE,
             alpha = 1,
             size = 2) +
  scale_color_manual(values = c("#FF5500", "#0051FF")) +
  facet_wrap(.~animal_id) +
  labs(title = "Locations of Caribous",
       subtitle = "Movement of top 10 Caribous with the most measurements in Summer and Winter", 
       caption = "plot by @toeb18 \nSource:Movebank Data Repository") +
  theme_map()
ggsave(paths, filename = "paths.png", width = 20, height = 20, units = "cm", dpi = 300)
paths + transition_reveal(timestamp) +
  shadow_wake(wake_length = 1)-> ani1
animate(ani1,nframes = 50, fps = 3)

anim_save(animation = ani1, filename = "caribou_paths.gif",
          nframes = 20, fps = 3,
          width = 15, height = 15, units = "cm", res = 300,
          end_pause = 5)


