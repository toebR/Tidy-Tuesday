#tidy tuesday on mobile subs and landline subs
#by Tobias Stalder Nov 2020


library(tidyverse)
library(patchwork)
library(ggpubr)
library(hrbrthemes)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")
library(Cairo)

setwd(r"(C:\Users\tobia\Desktop\ownprojects\dataviz\mobiles)")

#load data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')



# simple dataprep ---------------------------------------------------------


mobile %>%
  group_by(year, continent) %>%
  summarise(sum_subs = mean(mobile_subs, na.rm = TRUE)) -> conts_mob

landline %>%
  group_by(year, continent) %>%
  summarise(sum_subs = mean(landline_subs, na.rm = TRUE)) -> conts

mobile %>%
  group_by(year) %>%
  summarise(sum_subs = mean(mobile_subs, na.rm = TRUE)) -> mob_cummulative

landline %>%
  group_by(year) %>%
  summarise(sum_subs = mean(landline_subs, na.rm = TRUE)) -> land_cummulative

mob_cummulative %>%
  mutate(class = "mobile") -> mob_cummulative

land_cummulative %>%
  mutate(class = "landline") -> land_cummulative

rbind(mob_cummulative, land_cummulative) -> cummulative



# plots -------------------------------------------------------------------


ggplot()+
  geom_area(data = conts_mob, aes(x = year, y = sum_subs, fill = continent),
            color = "black", show.legend = TRUE) +
  scale_fill_viridis_d("", option = "plasma")+ 
  scale_x_continuous(limits = c(1990, 2017), breaks = c(seq(1990, 2017, 2)))+
  xlab("Year")+
  ylab("Subscriptions / 100 People") +
  # facet_wrap(.~continent, scales = "free_x")+
  scale_y_continuous(limits = c(0,550))+
  labs(subtitle = "Mobile")+
  theme_tufte() +
  theme(axis.line = element_line(color = "darkgrey") ) -> mobs
mobs

  ggplot()+
    geom_area(data = conts, aes(x = year, y = sum_subs, fill = continent),
              color = "black", show.legend = TRUE) +
    scale_fill_viridis_d("", option = "plasma")  +
    scale_x_continuous(limits = c(1990, 2017), breaks = c(seq(1990, 2017, 2)))+
    xlab("Year")+
    ylab("Subscriptions / 100 People") +
    # facet_wrap(.~continent, scales = "free_x")+
    scale_y_continuous(limits = c(0,550))+
    labs(subtitle = "Landline")+
    theme_tufte() +
    theme(axis.line = element_line(color = "darkgrey") ,
          plot.subtitle = element_text(size = 10)) -> lands
  lands

  
ggplot()+
    geom_line(data = cummulative, aes(x = year, y = sum_subs, color = class),
              show.legend = FALSE, position = "identity",
              alpha = 1)+
  geom_point(data = cummulative, aes(x = year, y = sum_subs, color = class),
            show.legend = FALSE, position = "identity",
            alpha = 1)+
  scale_x_continuous(limits = c(1990, 2017), breaks = c(seq(1990, 2017, 2)))+
  scale_color_manual(values = c("orange", "purple"))+
  xlab("Year")+
  ylab("Subscriptions / 100 People") +
  annotate(geom = "text", label = "mobile", x = 2010, y= 100, family = "Bahnschrift",
           size = 3, color = "white")+
  annotate(geom = "text", label = "landline", x = 2010, y= 27, family = "Bahnschrift",
           size = 3, color = "white")+
  labs(subtitle = "Mean Global Subscriptions")+
    theme_tufte() +
  theme(axis.line = element_line(color = "darkgrey")) -> overview
    
  

# patchwork ---------------------------------------------------------------

  
  overview / mobs / lands + plot_annotation(title = "The Rise of Mobile Phones",
                                              subtitle = "Global Mobile and Landline Subscriptions\n",
                                            caption = "Plot by Tobias Stalder | Source: ourworldindata.org") + plot_layout(guides = "collect") &
  theme(legend.position = 'bottom', text = element_text(family = "Bahnschrift", color = "white"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        plot.background = element_rect(fill = "black", color = "black")) -> plot



# export ------------------------------------------------------------------
ggsave(plot = plot,dpi = 300, width = 8, height = 10,
       filename = "plot.png")

