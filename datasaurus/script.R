library(tidyverse)
library(ggextra)
library(hrbrthemes)
library(ggthemes)
library(extrafont)
loadfonts()


datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

setwd(r"(C:\Users\tobia\Desktop\ownprojects\dataviz\datasaurus)")

unique(datasaurus$dataset) -> iterator

for (i in iterator){
  datasaurus %>%
  filter(dataset == i) %>%
  ggplot(aes(x = x, y = y)) +
    geom_point(alpha = .5, color = "#85c1e9") +
  geom_hline(yintercept = mean(datasaurus$x), color = "yellow", linetype = "dashed") +
  geom_vline(xintercept = mean(datasaurus$y), color = "yellow", linetype = "dashed") +
    annotate(geom = "text", label = "Mean of X", x = 93, y = 51, color = "yellow",
             size = 3) +
    annotate(geom = "text", label = "Mean of Y", x = 60, y = 0, color = "yellow",
             size = 3) +
  scale_x_continuous(limits = c(0,100)) +
    scale_y_continuous(limits= c(0,100))+
    coord_fixed()+
    theme_ft_rc() +
    theme(text = element_text(family = "Bahnschrift"),
          panel.background = element_rect(color = "darkgrey"))
    ggsave(filename = paste(i, ".png"), dpi = 500, width = 4.08, height = 4, units = "in")
}
  
