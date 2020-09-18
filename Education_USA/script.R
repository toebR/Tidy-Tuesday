##tidytuesday education spendings
##by tobias stalder September 2020
library(tidyverse)
library(sf)
library(USAboundariesData)
library(reshape2)
library(patchwork)
library(extrafont)

loadfonts(device = "win")

#load data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

kids$state_name <- kids$state

USAboundaries::us_states() -> state

#clean data for later join (data on 2000, 2008, 2016)
#get data on elementary and secondary eduction in 1000$/y for each state inflation adjusted
kids %>%
  group_by(year, state_name)%>%
  filter(variable == "PK12ed") %>%
  summarise(mean_child = inf_adj_perchild) %>%
  filter(year %in% c(2000, 2008, 2016)) -> kids_cleaned


#join data by state
inner_join(state, kids_cleaned, by = "state_name") -> ed_poly

ed_poly = ed_poly %>%
  filter(state_name != "Hawaii") %>%
  filter(state_name != "Alaska")

ed_poly_2000 = ed_poly %>%
  filter(year == "2000")
  ed_poly_2008 = ed_poly%>%
    filter(year == "2008")
  ed_poly_2016 = ed_poly%>%
    filter(year == "2016")



#make a map
ggplot(ed_poly_2000) +
  ggtitle("2002") +
  labs(subtitle = "State spendings on primary and secondary education")+
  geom_sf(aes(fill = mean_child), color = "transparent") +
  scale_fill_viridis_c(option = "inferno",
                       breaks = c(0,6,12,17.5),
                       limits = c(0,17.5),
                       begin = 0) +
  guides(fill = guide_colorsteps(barwidth = 15,
                                 barheight = .5,
                                 title.position = "top",
                                 title.hjust = .5,
                                 title = "Inflation-adjusted\n[1000 US$ / Year]")) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .08),
        plot.subtitle = element_text(hjust = .17)) -> P2000

ggplot(ed_poly_2008) +
  ggtitle("2008") +
  labs(subtitle = "State spendings on primary and secondary education")+
  geom_sf(aes(fill = mean_child), color = "transparent") +
  scale_fill_viridis_c(option = "inferno",
                       breaks = c(0,6,12,17.5),
                       limits = c(0,17.5),
                       begin = 0) +
  guides(fill = guide_colorsteps(barwidth = 15,
                                 barheight = .5,
                                 title.position = "top",
                                 title.hjust = .5,
                                 title = "Inflation-adjusted\n[1000 US$ / Year]")) +
  theme_void()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .08),
        plot.subtitle = element_text(hjust = .17)) -> P2008

ggplot(ed_poly_2016) +
  ggtitle("2016") +
  labs(subtitle = "State spendings on primary and secondary education")+
  geom_sf(aes(fill = mean_child), color = "transparent") +
  scale_fill_viridis_c(option = "inferno",
                       breaks = c(0,6,12,17.5),
                       limits = c(0,17.5),
                       begin = 0) +
  guides(fill = guide_colorsteps(barwidth = 15,
                               barheight = .5,
                               title.position = "top",
                               title.hjust = .5,
                               title = "Inflation-adjusted\n[1000 US$ / Year]")) +
  theme_void()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .08),
        plot.subtitle = element_text(hjust = .17),
        plot.background = element_rect(fill =  "#fff9c4", color = "#fff9c4")) -> P2016


max(ed_poly$mean_child)
min(ed_poly$mean_child)

P2000 / P2008 / P2016 + 
  plot_annotation(title = 'Education Spendings in the USA',
                  subtitle = "This visualisation suggests that there is an overall spending increase from 2000-2016.\nHowever, there are differences in development between the states\n",
                  caption = "plot by @toeb18",
                  theme = theme(plot.title = element_text(size = 18, hjust = .5),
                                plot.subtitle = element_text(size = 10, hjust= .5),
                                plot.margin = unit(c(1,1,1,1), "cm"),
                                plot.background = element_rect(fill =  "#fff9c4", color = "#fff9c4"),
                                )) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom',
                                          text = element_text(family = "Bahnschrift")) -> po
  ggsave(po, filename =  "test_ed_poster.png", width = 15, height = 32, units = "cm")

