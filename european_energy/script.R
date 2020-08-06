dir = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\R\tidytuesday\european_energy)"
setwd(dir)

library(tidyverse)
library(reshape2)
library(sf)
library(tidyr)
library(ggthemes)
library(prismatic)
library(tmaptools)
library(patchwork)

options(scipen = 999)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

#read in a european polygon for administrative boundaries
poly <- st_read(dsn = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\R\own_projects\eurostat\city_audits_2018\europe_poly.shp)")
colnames(poly) = c("country", "country_name", "Name_Enlg", "ISO3", "FID1", "geometry")

#join energy data to the polygon
polyjoin <- left_join(poly, country_totals, by = "country")
# view(polyjoin)

polyjoin = polyjoin %>%  #drop NA of join
  filter(type != is.na(type))



# (1) Maps on Countries Import and Export Bilances ------------------------
#calculate import-export bilance for earch country and maybe new index that shows result if net-importing or net-exporting
ImpExp <- polyjoin %>%
  filter(type %in% c("Imports", "Exports")) %>%
  select("country", "type", "2018")

# ImpExp = st_transform(ImpExp, 4326) #transform to WGS84

Exp = ImpExp %>%
  filter(type == "Exports")

Imp = ImpExp %>%
  filter(type == "Imports")

Tot = polyjoin %>%
  filter(type %in% c("Total net production")) %>%
  select("country", "type", "2018")


ggplot(Exp) +
  geom_sf(aes(fill = `2018`), color = "black") +
  scale_fill_gradientn(colors =c(get_brewer_pal("YlGnBu", n = 6, contrast = c(0.1, 0.6)))) +
  labs(fill ="[GWh]", title = "Exported Energy") +
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "black"),
        text = element_text(color = "white"),
        legend.position = "left",
        plot.title = element_text(hjust = .4)) -> ExpMap

ggplot(Imp) +
  geom_sf(aes(fill = `2018`), color = "black") +
  scale_fill_gradientn(colors =c(get_brewer_pal("YlGnBu", n = 6, contrast = c(0.1, 0.6)))) +
  labs(fill ="[GWh]", title = "Imported Energy") +
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "black"),
        text = element_text(color = "white"),
        legend.position = "left",
        plot.title = element_text(hjust = .4)) -> ImpMap
ImpMap


ggplot(Tot) +
  geom_sf(aes(fill = `2018`), color = "black") +
  scale_fill_gradientn(colors =c(get_brewer_pal("YlGnBu", n = 6, contrast = c(0.1, 0.6)))) +
  labs(fill ="[GWh]", title = "Total net production") +
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "black"),
        text = element_text(color = "white"),
        legend.position = "left",
        plot.title = element_text(hjust = .4)) -> Totmap
Totmap

Totmap + ExpMap + ImpMap +
plot_annotation(title = 'Energy Production in Europe for 2018\n ',
                caption = "plot by @toeb18",
                subtitle = "Total Net Production are highest in Germany and France, which are also the biggest exporters.\nItaly is the biggest energy importer\n",
                theme = theme(plot.title = element_text(size = 16, hjust = .5, color = "white"),
                              plot.subtitle = element_text(size = 13, hjust= .5, color = "white"),
                              plot.margin = unit(c(1,1,1,1), "cm"),
                              plot.background = element_rect(fill = "black"),
                              text = element_text(colour = "white"))) -> infographic
infographic
ggsave(infographic, filename = "energy_2018.png", width = 35, height = 20, units = "cm")
