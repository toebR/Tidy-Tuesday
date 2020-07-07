#???script by @toeb18
#july 2020


library(tidyverse)
library(reshape2)
library(ggpubr)
library(prismatic)
library(crayon)
library(tmaptools)

palette_explorer()

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\xmen"

setwd(dir)

#load data of interest

character_visualization <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')

char = data.frame(character_visualization)

charlong = melt(char, id.vars = c("issue", "costume", "character"))
charlong$viz = as.factor(charlong$viz)

colnames(charlong) = c("issue", "costume", "character", "viz", "number")

#filter for favourite mutants and unequal 0 at number of appereances:)

unique(charlong$character)



#make plot that shows the type of viz for all xmnen and shows if they are in stumes or not
charlong %>%
  # filter(number != 0) %>%
  filter(character %in% c("Professor X = Charles Xavier (no costume)*",
                          "Wolverine = Logan", 
                          "Storm = Ororo Munroe", 
                          "Magneto = Erik Magnus*",
                          "Rogue = Name Unknown",
                          "Marvel Girl/Phoenix = Jean Grey",
                          "Mystique = Name Unknown*"
                          )) -> charlong1


charlong1$character = gsub("Wolverine = Logan", "Wolverine", charlong1$character)
charlong1$character = gsub("Professor X = Charles Xavier (no costume)", "Professor X", charlong1$character)
charlong1$character = gsub("Storm = Ororo Munroe", "Storm", charlong1$character)
charlong1$character = gsub("Magneto = Erik Magnus", "Magneto", charlong1$character)
charlong1$character = gsub("Rogue = Name Unknown", "Rogue", charlong1$character)
charlong1$character = gsub("Marvel Girl/Phoenix = Jean Grey", "Phoenix", charlong1$character)
charlong1$character = gsub("Mystique = Name Unknown", "Mystique", charlong1$character)
charlong1$character = gsub("*(no costume)*", "", charlong1$character)
charlong1$character = gsub("Professor X = Charles Xavier", "Professor X", charlong1$character)
charlong1$character = gsub("()", "", charlong1$character)

#colors
color(get_brewer_pal("Blues", n = 30, contrast = c(.5,1)))

ggplot(charlong1) +
  geom_point(aes(x = issue, y = number, group = viz), alpha = .2, size = 1.5, color = "#FC7D37FF") +
  geom_line(aes(x = issue, y = number, group = viz), alpha = .8, color = "#FEB34DFF") +

  facet_grid(character~viz) +
  ylab("Count of Character Appereance") +
  xlab("Issue Nr.") +
  labs(title = "Chris Claremont's 16 Year Run on Uncanny X-Men #97-278",
       subtitle = "Character Visualisations of my Favourite Mutants",
       caption = "Plot by @toeb18, toebR") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(color = "#08306BFF", fill = "#474747FF"),
        text = element_text(color = "#61A7D2FF"),
        axis.text = element_text(color = "#2B7BBAFF"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color ="#2B7BBAFF"),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(color = "#61A7D2FF"),
        panel.background = element_rect(color = "#61A7D2FF", fill ="#474747FF"))+
  rotate_x_text() -> plot
plot


ggsave(plot, filename = "plot.png", width = 15, height = 19, units = "cm")

