##author: (Twitter: @toeb18, Github: toebR)

library(tidyverse)
library(ggpubr)
library(ggimage)
library(cowplot)
library(tmaptools)
library(shinyjs)

#color exploration for later
palette_explorer()
print(get_brewer_pal("Paired", n = 8))
blues <- get_brewer_pal("Blues", n = 30, contrast = c(0.39, 0.95))
greys <-get_brewer_pal("Greys", n = 13, contrast = c(0.33, 1))
print(greys)

#set working dir
dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\drinks")
setwd(dir)


# Get the Data

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

#Since I dont have a lot of time, I will focus on the boston_cocktail because it was said to be cleaner

head(boston_cocktails)
colnames(boston_cocktails)

#.I will only use the categories, so I store that in a new variable
categs <- data.frame(boston_cocktails$category)
categs

#change de data so we have 2 columns: one with category and one with the count
unique(categs)

#this can be done with just one command... :D
categs <- data.frame(table(categs))


#make a lollipo chart with ascending order on the y axis and ADD A COCKTAIL PNG!
ggplot(ordered) +
  geom_point(aes(x = reorder(categs, Freq),y = Freq), size = 3, color = "#FF7F00") +
  geom_segment(aes(y = 0, x = categs,
                   xend =reorder(categs, Freq),
                   yend =Freq), size = 1, color = "#FF7F00") +
  geom_text(aes(label = Freq, x = reorder(categs, Freq),y = Freq),
            vjust = -0.65, size = 3.5, color = "#969696") +
  rotate_x_text() +
  xlab("Category") +
  ylab("Cocktails") +
  coord_flip() +
  ggtitle("I'd like a...", subtitle = "Number of Unique Cocktails per Category") +
  theme(text = element_text(color = "#969696"),
    plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_line(color = '#4d5566'),
    panel.grid.minor.x = element_line(color = '#4d5566'),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(color = "#969696"),
    axis.ticks.x = element_blank()) -> plot
plot
ggsave(plot, filename = 'lollipop_cocktails.png', width = 15, height = 15, units = "cm")



