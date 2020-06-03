#plotting some stuff on the Tidytuesdy marbles dataset
#by @toeb18, Rtoeb

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\marbles")
setwd(dir)

library(tidyverse)
library(ggstance)
library(ggpubr)
install.packages("ggbump")
library(ggbump)

#import data
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

#look at data
head(marbles)
summary(marbles)
nrow(marbles)

plot(marbles)

#do some stuff on the basis of the marble teams
unique(marbles$team_name) #get teams

#i want to do a plot of points per time over the season
points <- marbles %>%
  filter(race %in% c("S1R1" ,"S1R2" ,"S1R3", "S1R4" ,"S1R5", "S1R6" ,
                     "S1R7", "S1R8"))


ggplot(points) +
  # geom_line(aes(x = race, y = points, color = team_name, group = team_name)) +
  geom_point(aes(x = race, y = points, group = team_name, color = team_name, size = points), show.legend = FALSE, alpha = 0.8) +
  geom_point(aes(x = race, y = points, group = team_name, color = team_name), size = 8, show.legend = FALSE, alpha = 0.3) +
  geom_bump(aes(x = race, y = points, group = team_name, color = team_name), show.legend = FALSE, size = 0.3, alpha = 0.8) +
  geom_bump(aes(x = race, y = points, group = team_name, color = team_name), show.legend = FALSE, size = 0.2, alpha = 0.5) +
  geom_bump(aes(x = race, y = points, group = team_name, color = team_name), show.legend = FALSE, size = 3, alpha = 0.3) +
  scale_y_continuous(limits = c(-5,30), breaks = seq(0,30,10)) +
  facet_wrap(.~team_name) +
  ggtitle("Jelle's Marble Runs", subtitle = "Ranking of all Teams through Season One") +
  labs(caption = "plot by @toeb 18 | source: Jelle's Marble Runs on Github") +
  xlab("Race") +
  ylab("Points") +
  rotate_x_text() +
  theme(plot.title = element_text(hjust=0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 17),
        text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A", color = "#444B5A"),
        panel.background = element_rect(fill = "#444B5A", color = "transparent"),
        panel.spacing = unit(1.5, "lines"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#969696", linetype = "longdash"),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 14, color = '#969696'),
        axis.text = element_text(color = "#969696"),
        strip.background = element_rect(fill = "#444B5A", color = 'transparent', size = 2),
        strip.text = element_text(color = '#969696', size = 12),
        ) -> plot
ggsave(plot, filename = "plot_marbles.png", width = 30, height = 20, units = "cm")

