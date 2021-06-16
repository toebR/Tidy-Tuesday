#dataviz on duBois tweets
#TidyTuesday
#by Tobias Stalder
#June 2021


# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(sf)
library(lubridate)
library(spData)
library(ggbump)
library(extrafont)

# data --------------------------------------------------------------------

tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')


# data wrangling ----------------------------------------------------------


## add continent location --------------------------------------------------

#make sf object
tweets %>%
  drop_na(lat, long) %>%
  st_as_sf(coords = c("long", "lat")) -> tweets_spatial
st_crs(tweets_spatial) <- 4326

#spatialjoin with continents
world  %>%   #world polygon geodata directly loaded upon spData library call
 select(continent)  %>%
  filter(continent != "Antarctica")-> continents

st_join(tweets_spatial, continents) -> geodat

## extract date ------------------------------------------------------------

geodat %>%
  drop_na(datetime)%>%
  mutate(date = as_date(datetime)) -> datetime


# calculate count ---------------------------------------------------------
datetime %>%
  drop_na(continent)%>%
group_by(date, continent) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(date, nesting(continent), fill = list(count = 0)) %>%
  complete(date)%>%  #fill empty dates with 0 (not all, just so the baseline is visible)
  drop_na(continent)-> counts


# tweets over time --------------------------------------------------------

unique(counts$continent) -> conts

for (i in conts){
  counts %>%
    filter(continent == i) -> counts_f
  ggplot(counts_f, aes(x = date, y = count,color = continent)) +
    geom_bump(color = "#1DA1F2", show.legend = FALSE) +
    scale_y_continuous(limits = c(0,45))+
    ggtitle(paste0(first(counts_f$continent)))+
    xlab("")+
    ylab("Total Tweets")+
    theme_minimal()+
    theme(text = element_text(family = "Work Sans", color = "grey"),
          plot.background = element_rect(color = "black", fill = "black"),
          panel.background = element_rect(color = "black", fill = "black"),
          panel.grid = element_blank()) +
    ggsave(filename = paste0(first(counts_f$continent), "lineplot.svg"),
           path = paste0(here(), "/plots"),
           width = 2, height = 2)
}



# map of tweets over time -------------------------------------------------


ggplot()+
  geom_sf(data = continents, aes(fill = continent),fill = "gray30", color = "gray12", size = 0.5) +
  geom_sf(data = geodat, aes(color = datetime), color = "white", size = 3.5)  +
  geom_sf(data = geodat, aes(color = datetime), color = "#1DA1F2", size = 3)  +

  scale_color_viridis_c() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")+
  theme_minimal()+
  theme(text = element_text(family = "Work Sans", color = "grey"),
        plot.background = element_rect(color = "black", fill = "black"),
        panel.background = element_rect(color = "black", fill = "black"),
        panel.grid = element_line(color = "gray12")) +
  ggsave(filename = "globe.png",
         path = paste0(here(),"/plots"),
         width = 11.1, height =10.35)




