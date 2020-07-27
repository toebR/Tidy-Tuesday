#script by tobias stalder (@toeb18)
#july 2020

library(tidyverse)

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\animals"
setwd(dir)

#read in data, I dont have time to clean up the rawdata. That's why i use the dataset with less vars
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

view(penguins)
#data seems plot ready :D

#get data per species:
Adelie = penguins %>%
  filter(species == "Adelie")

Gentoo = penguins %>%
  filter(species == "Gentoo")
  
Chinstrap = penguins %>%
  filter(species == "Chinstrap")

#make overview of bodymass and compare species, sex and locations(island)


# first plot: explanation of viz plot!! ----------------------------------------

ggplot()+
  geom_jitter(data = penguins,
              aes(x = penguins$species, y = penguins$body_mass_g),
              width = .2,
              alpha = .3) +

  geom_point(data = Gentoo,
             aes(y = mean(Gentoo$body_mass_g, na.rm = TRUE), x = species),
             size = 3, color = "white") +
  geom_point(data = Gentoo,
             aes(y = mean(Gentoo$body_mass_g, na.rm = TRUE), x = species),
             size = 2.5, color = "#FF6600") +
  geom_hline(yintercept = 5076.016, color = "#FF6600") + #Gentoo mean
  geom_point(data = Adelie,
             aes(y = mean(Adelie$body_mass_g, na.rm = TRUE), x = species),
             size = 3, color = "white") +
  geom_point(data = Adelie,
  aes(y = mean(Adelie$body_mass_g, na.rm = TRUE), x = Adelie$species),
  size = 2.5, color = "#FF3DF5") +
  geom_point(data = Chinstrap,
             aes(y = mean(Chinstrap$body_mass_g, na.rm = TRUE), x = species),
             size = 3, color = "white") +
  geom_point(data = Chinstrap,
  aes(y = mean(Chinstrap$body_mass_g, na.rm = TRUE), x = species),
  size = 2.5, color = "#4840E6") +
  
  geom_hline(yintercept = 3700.662, color = "#FF3DF5") + #Adeline mean
  geom_hline(yintercept = 3733.088, color = "#4840E6") + #Chinstrap mean
  scale_y_continuous(limits = c(0,7000), breaks = seq(0,7000, 2000)) +
  coord_polar() +
  annotate("text", label = "0",
           x = .5, y = 0.1,
           size = 3) +
  annotate("text", label = "2000",
           x = 0.5, y = 2000,
           size = 3) +
  annotate("text", label = "4000",
           x = 0.5, y = 4000,
           size = 3) +
  annotate("text", label = "6000",
           x = 0.5, y = 6000,
           size = 3) +
  annotate("text", label = "Body Mass [g]",
           x = 0.5, y = 7000,
           size = 3) +
  geom_segment(aes(x =3, y = 5200, xend = 2.5, yend = 7000),
               color = "#FF6600", linetype = "dashed") +
  annotate("text", label = "Big dot = Group mean \nCircle = Visual expand",
           x = 2.46, y = 7000,
           size = 2) +
  labs(title = "Penguin Body Weight Among Species",
       subtitle = "A new visual approach to compare grouped means",
       caption = "plot by @toeb18") +
  theme(
    plot.background = element_rect(color = "#84E6EB", fill = "#84E6EB"),
    panel.background = element_rect(color = "#84E6EB", fill = "#84E6EB"),
    plot.title = element_text(hjust= 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
    )-> expl
expl
ggsave(expl, filename = "expl.png", width = 15, height = 16, units = "cm", dpi = 500) 






