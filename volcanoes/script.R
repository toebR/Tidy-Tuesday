library(tidyverse)
library(ggpubr)
library(reshape2)
library(sf)
library(leaflet)
library(ggmap)
library(ggthemes)
library(patchwork)
library(tmaptools)
library(shinyjs)
library(gganimate)
library(scales)
library(glue)
palette_explorer()

print(get_brewer_pal("YlOrRd", n = 6, contrast = c(0.54, 1)))

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\volcanoes")
setwd(dir)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')



#all volcanos
ggplot(volcano,aes( x = longitude, y = latitude, color = region )) +
  borders(fill = "grey", colour = "darkgrey") +
  geom_point(alpha = 0.5, size = 2) +
  xlab("X") +
  ylab("Y") +
  theme_solarized() +
  labs(title = "Tidy Tuesday: All Volcanos by Region") +
  labs(caption = "plot by @toeb18") -> plot1
plot1
#last year eruped


volcano$last_eruption_year <- as.numeric(volcano$last_eruption_year)

volcano %>% 
  filter(!is.na(last_eruption_year)) %>%
  filter((last_eruption_year)>1900) %>%
  ggplot(aes(x = longitude, y = latitude, 
  color = as.numeric(last_eruption_year))) +
  scale_color_gradientn("last eruption year",limits = c(1900, 2020), breaks = seq(from = 1900, to = 2020, by = 20),
  colors = c("#FCC3AC","#FB7D5D", "#F5533B", "#E22E26", "#C1151B", "#9D0D14", "#67000D")) +
  #            fill = population_within_5_km)) +   
  # scale_fill_gradientn("People whithin\n5 Km",limits = c(0, 5783287), breaks = seq(from = 0, to = 5783287, by = 1000000),
  #                       colors = c("#FC7836", "#FB4B29", "#E9261F", "#CD0B21", "#AB0026", "#800026"),
  #                       labels = function(x) format(x, scientific = FALSE)) +
  borders(fill = "grey", colour = "darkgrey",) +
  geom_point(size = 3, shape = 21) +
  xlab("X") +
  ylab("Y") +
  theme_solarized() +
  ggtitle("Tidy Tuesday: Eruptions since 1990") +
  labs(caption = "plot by @toeb18") -> plot2

summary(volcano$population_within_5_km)
plot2

set <-(plot1) /
  plot2

ggsave(set, filename = "volcano_map.png", width = 20, height = 25, units = "cm")

#interactive map
volcano %>% 
  filter(!is.na(last_eruption_year)) %>%
  filter((last_eruption_year)>1900) %>%
  leaflet() %>%
  addTiles%>%
  addCircles(data = volcano, ~longitude, ~latitude)

str(volcano$last_eruption_year)

volcano %>% 
  filter(!is.na(last_eruption_year)) %>%
  filter((last_eruption_year)>1900) %>%
  ggplot(aes(x = longitude, y = latitude, fill = population_within_10_km, group = last_eruption_year)) +
  scale_fill_gradientn("People whithin\n10 Km",limits = c(0, 5783287), breaks = seq(from = 0, to = 5783287, by = 1000000),
                       colors = c("#FC7836", "#FB4B29", "#E9261F", "#CD0B21", "#AB0026", "#800026"),
                       labels = function(x) format(x, scientific = FALSE)) +
  borders(fill = "grey", colour = "darkgrey") +
  geom_point(alpha = 1, size = 4, shape = 21) +
  xlab("X") +
  ylab("Y") +
  theme_solarized() +
  labs(caption = "plot by @toeb18") +
  transition_time(last_eruption_year) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.3) +
  ease_aes('linear') +
  ggtitle("my first GIF for TidyTuesday!", subtitle = "Recorded Volcano Eruptions since 1900: Year { round(frame_time) }")-> ani1

animate(ani1,nframes = 120, fps = 3)
anim_save(animation = ani1, filename = "volcano_eruptions.gif",
          nframes = 120, fps = 2,
          width = 25, height = 15, units = "cm", res = 300,
          end_pause = 10)

warnings()
