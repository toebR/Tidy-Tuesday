library(tidyverse)
library(ggpubr)
library(tmaptools)
library(shinyjs)

palette_explorer()

#set working dir
dir <- "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\AfricanAmericanAchievements"
setwd(dir)

#get data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

#glimpse at data
view(firsts)
firsts$gender <- as.factor(firsts$gender)


#idea on firsts: could be interesting if woman or men were more active in certain categories and plot it against the year to see trends?

view(science)
#the science dataset takes too much time for me too mine. Will stick with firsts for this one.


df <- as.data.frame(firsts)

#look at data
ggplot(df) +
  geom_tile(aes(x = year, y = category, fill = gender), 
            position = "dodge") +
  scale_x_continuous()
df$year

#data wrangling: get number of accomplishments per year
#function to floor the years to decades:
floor_decade    = function(value){ return(value - value %% 10) }

#make new column in df with decade
df$decade <- floor_decade(df$year)

#count number of entries(accomplishments) per decade and gender
count <- count(df, decade, gender)


#complete missing decades with 0
completed <- count %>%
  complete(gender, decade = full_seq(decade, 10), 
           fill = list(n = 0))

completed

#colorchoice
print(get_brewer_pal("Purples", n = 20, contrast = c(0.13, 1)))

#heatmap 1
heatmap1 <- ggplot(completed, aes(x = as.character(decade), y = gender, fill = n)) +
  geom_raster(stat = "identity") +
  scale_fill_gradient(low = "grey", high = "black") +
  xlab("Decade") +
  ylab("") +
  ggtitle("African-Americans breaking the Color Barrier",subtitle = "First Accomplishments* of African-American People since 1730") +
  labs(caption = "*First Documented Achievements by People of Color in Categories like Science, Sports, Law, Politics etc.\n plot by @toeb18 | Source: rfordatascience on GitHub") +
  labs(fill = "Number of Accomplishments") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text( size = 20),
        plot.subtitle = element_text( size = 15),
        text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A", color = "#444B5A"),
        panel.background = element_rect(fill = "#444B5A", color = "transparent"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_text(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#969696", linetype = "longdash"),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 14, color = '#969696'),
        axis.text = element_text(color = "#969696"),
  ) +
  # guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  rotate_x_text()

heatmap1

ggsave(heatmap1, filename ="heatmap.png", height = 8.5, width = 40, units = "cm")

