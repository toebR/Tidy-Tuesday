#script by Tobias Stalder
#Jan 2021
#tobias-stalder.netlify.app
#dataviz on transit projects based on tidytuesday data


# load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(here)
library(tidytuesdayR)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggpubr)
library(extrafont)
library(ggforce)
library(Cairo)


here()


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 2)

transit_cost <- tuesdata$transit_cost


# clean dataset -----------------------------------------------------------
# 1) remove rows with NA
transit_cost %>%
  drop_na(country) %>%
  
  # 2) drop columns that are not of interest to me
  select(-c(line, rr, tunnel, source1, source2, currency, cost, reference, real_cost, ppp_rate, year)) %>%
  
  # 3) filter for tunnel projects that are done 100% and projects that ended before 2021
  filter(tunnel_per == "100.00%",
         end_year <2021) -> transit_clean



# geocode and make spatial object (OBSOLETE) -----------------------------------------

# get country polygons
ne_countries(scale = 110, type = "countries", continent = NULL,
             country = NULL, geounit = NULL, sovereignty = NULL,
             returnclass = c("sf")) %>%
  select(iso_a2, sovereignt, continent) -> country_polygons

#join data on polygons
left_join(country_polygons, transit_clean,
          by = c("iso_a2" = "country")) -> transit_geo

#filter data for available information and create min_max per country
transit_geo %>%
  st_drop_geometry() %>%
  drop_na(cost_km_millions) %>%
  group_by(continent, sovereignt) %>%
  summarise(max = max(cost_km_millions),
            min = min(cost_km_millions),
            n = n(),
            mean = mean(cost_km_millions))-> country_segs

gsub("United States of America", "USA", country_segs$sovereignt) -> country_segs$sovereignt

#create factor levels for later facet reordering
country_segs %>%
  group_by(continent, sovereignt) %>%
  arrange(mean, "asc") -> country_ordered

country_segs %>%
  mutate(sovereignt = factor(sovereignt,
                             levels = c(country_ordered$sovereignt))) -> country_segs
  

# dataviz -----------------------------------------------------------------


ggplot(country_segs) +
  geom_segment(aes(x = sovereignt, xend = sovereignt, y =min, yend = max), color = "bisque4") +
  geom_point(aes( x = sovereignt, y = mean, color = continent), show.legend = FALSE, size = 3) +
  geom_point(aes(x = sovereignt,  y = min), color = "bisque4", size = 2)+
  geom_point(aes(x = sovereignt,  y = max), color = "bisque4", size = 2) +
  geom_text(aes(x = sovereignt,  y = max + 100, label = n, color = continent), show.legend = FALSE) +
  geom_mark_ellipse(aes(x = sovereignt, y = mean, fill = continent,group = sovereignt),  color = "bisque4",position = "identity",
                   expand = unit(5, "mm")) +
  scale_fill_manual(values = c("#B56576", "#6D597A", "#EAAC8B","#E56B6F", "#355070"))+
  scale_color_manual(values = c("#B56576","#6D597A", "#EAAC8B", "#E56B6F", "#355070"))+
  xlab("") +
  ylab("Cost [Million US$ / Km]")+
  labs(title = "Costs of Transit Projects",
       subtitle = "This data visualisation shows summary statistics of transit project costs until 2020 in different countries.")+
 
  facet_wrap(continent~sovereignt, nrow = 1, scales = "free_x")+
  theme_bw() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(color = "transparent"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(color = "bisque4"),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "bisque4"),
        legend.key = element_rect(color = "transparent"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(family = "Times New Roman", size = 24, color = "gray28", face = "bold"),
        plot.subtitle =  element_text(family = "Times New Roman", size = 18, color = "gray28"),
        axis.text = element_text(family = "Times New Roman", color = "gray28", size = 14),
        axis.title.y =  element_text(family = "Times New Roman", color = "gray28")) +
  rotate_x_text()-> p
p

# Inset plot for legend
country_segs %>%
  filter(sovereignt == "Germany") %>%
  ggplot() +
  geom_segment(aes(x = sovereignt, xend = sovereignt, y =min, yend = max), color = "bisque4") +
  geom_point(aes( x = sovereignt, y = mean, color = continent), show.legend = FALSE, size = 3) +
  geom_point(aes(x = sovereignt,  y = min), color = "bisque4", size = 2)+
  geom_point(aes(x = sovereignt,  y = max), color = "bisque4", size = 2) +
  geom_text(aes(x = sovereignt,  y = max + 15, label = n, color = continent), show.legend = FALSE) +
  geom_mark_ellipse(aes(x = sovereignt, y = mean, fill = continent,group = sovereignt),  color = "bisque4",position = "identity",
                    expand = unit(5, "mm"), show.legend = FALSE) +
  scale_fill_manual(values = c("#B56576FF"))+
  scale_color_manual(values = c("#B56576FF")) +
  xlab("")+
  ylab("")+
  
  # annotate the elements
  annotate(geom = "text", label = "Mean", x = .7, y = 240, color = "#B56576", angle = 90)+
  annotate(geom = "text", label = "Min", x = .7, y = 89, color = "#B56576", angle = 0)+
  annotate(geom = "text", label = "Max", x = .7, y = 358, color = "#B56576", angle = 0)+
  annotate(geom = "text", label = "n", x = .7, y = 370.5, color = "#B56576", angle = 0)+
  
  theme_bw()+
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "bisque4"),
        panel.border = element_rect(color = "transparent"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y =  element_text(family = "Times New Roman", color = "gray28"),
        axis.title = element_blank(),
        axis.text = element_blank()) -> legend

p + legend + plot_layout(widths = c(8,1)) +
  plot_annotation(caption = "Plot by Tobias Stalder | tobias-stalder.netlify.app\nSource: Transit Costs Project | transitcosts.com\n
                  #TidyTuesday" ,
                  theme = theme(plot.background = element_rect(color = "black"),
                                plot.caption = element_text(color = "gray28", family = "Times New Roman", size = 12)),
                  )-> inf
inf

ggsave(inf, filename = "transit_cost.png", width = 42, height = 21, units = "cm", dpi = 300, type = "cairo-png")
