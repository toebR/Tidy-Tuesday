# script by Tobias Stalder (@toeb18)
#july 2020

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\Astronauts"
setwd(dir)


library(tidyverse)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
# library(geonames)
# library(countrycode)
library(rworldmap)
library(patchwork)

#set up geonames settings
# options(geonamesUsername="toeb18")
# options(geonamesHost="api.geonames.org")

#test geocoding
# geonames::GNcountryInfo("KE")

#load astronaut data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


glimpse(astronauts)
head(astronauts)

#make viz about durations
#add R2 and function and p value of correlation
ggplot(astronauts, aes(y=total_hrs_sum, x = year_of_mission)) +
  geom_boxplot(aes(group = year_of_mission),
               outlier.colour = "#FFD900",
               outlier.alpha = .6,
               color = "#4B4B6E",
               fill = "lightgrey") +
  geom_smooth(method = "gam", color = "#FFD900") +
  xlab("Year of Mission") +
  ylab("Total Hours Spent on a Mission") +
  labs(title = "Increase of Hours Spent on A Mission through Time") +
  theme_bw()+
  theme(plot.background = element_rect(fill = "#4B4B6E", color = "#4B4B6E"),
        panel.background = element_rect(fill = "#4B4B6E", color = "grey"),
        panel.border = element_rect(color = "grey"),
        text = element_text(color = "grey"),
        panel.grid = element_line(color = "darkgrey"),
        axis.text = element_text(color = "lightgrey")) -> barplot
ggsave(barplot, filename = "barplot.png", width = 20, height = 10, units = "cm")

#make map of origin of the astronauts.
countries = data.frame(table(astronauts$nationality))
countries


#first change country name to country ID, for it to passt to geonames API to get lon/lat
#first I have to change some country names (states that no not exist anymore, I chose a "best" fit..)
#I drop the UK/US double origins..
countries$Var1 = gsub("Czechoslovakia", "Slovakia", countries$Var1)
countries$Var1 = gsub("Hungry", "Hungary", countries$Var1)
countries$Var1 = gsub("Malysia", "Malaysia", countries$Var1)
countries$Var1 = gsub("Netherland", "Netherlands", countries$Var1)
countries$Var1 = gsub("U.S.S.R/Russia", "Russia", countries$Var1)
countries$Var1 = gsub("U.S.S.R/Ukraine", "Ukraine", countries$Var1)
countries$Var1 = gsub("Korea", "South Korea", countries$Var1)
countries$Var1 = gsub("Republic of South Africa", "South Africa", countries$Var1)
countries$Var1 = gsub("U.K.", "United Kingdom", countries$Var1)
countries$Var1 = gsub("U.S.", "United States of America", countries$Var1)
countries$Var1 = gsub("UAE", "United Arab Emirates", countries$Var1)

countries = countries %>%
  filter(Var1 != "United Kingdom/United States of America")

# #get iso2c codes of all countries for later geocoding
# 
# countries = countries %>%
#   mutate(iso = countrycode(countries$Var1, origin = "country.name", destination = "iso2c"))

#define better colnames in order before geocoding
colnames(countries) = c("ctry_name", "nr_of_astros")

#approach instead of geocoding: get centroids and left join them on countries df

wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
# view(unique(d))

# get a data.frame with centroids
df <- data.frame(centroids)
d <- cbind(rownames(df), data.frame(df, row.names=NULL))
colnames(d) = c("ctry_name", "x", "y")

#now we have a dataframe with longitude and lattitude
#join df on countries

countries_geo = left_join(countries, d, by = "ctry_name")
sf::st_as_sf(countries_geo, coords = c("x","y")) -> countries_geo_sf
st_crs(countries_geo_sf) = 4326
#convert countries df to sf object?


#world map with countries of origin highlighed with centroid points
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world, fill = "grey", color = "#4B4B6E") +
  geom_sf(data = countries_geo_sf, color = "#FFD900", size = 1) +

  geom_sf(data = countries_geo_sf, color = "#FFD900", size = 3, alpha = .5) +
  geom_sf(data = countries_geo_sf, color = "#FFD900", size = 5, alpha = .2) +
  labs(title = "Astronaut's Countries of Origin")+
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#4B4B6E", color ="#4B4B6E" ),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(colour = "grey", hjust = .5)) -> map
ggsave(map, filename = "map.png", width = 15, height = 15, units = "cm")

map

#do infographic with patchwork
map + barplot +
  plot_annotation(title = 'HUMANS IN SPACE - A SMALL DATA STORY\n ',
                  caption = "plot by @toeb18",
                  subtitle = "Astronaut's origins are all over the world but mainly in the western hemisphere.\nThe general additive model suggests that the mission times increased throughout the years.\n",
                  theme = theme(plot.title = element_text(size = 16, hjust = .5),
                                plot.subtitle = element_text(size = 13, hjust= .5),
                                plot.margin = unit(c(1,1,1,1), "cm"),
                                plot.background = element_rect(fill = "#4B4B6E"),
                                text = element_text(colour = "white")))-> astronauts_infographic

ggsave(astronauts_infographic, filename = "astronauts_inforgraphic.png",
       width = 25, height = 15, units = "cm")




