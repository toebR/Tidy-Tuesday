
# header ------------------------------------------------------------------

#' Tidytuesday plot on afrilearndata
#' by tobias stalder
#' tobias-stalder@netlify.app
#' github: toebR
#' tiwtter: @toeb18

options(scipen = 999)

# libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(sf)
# remotes::install_github("afrimapr/afrilearndata")
library(afrilearndata)
library(ggrepel)
library(extrafont)
library(Cairo)


# wrangle -----------------------------------------------------------------
#get two cities with highest population
africapitals %>%
  slice_max(order_by = pop, n = 2) -> top

#retrieve coordinates and drop geometry for later ggrepel
st_coordinates(top) -> topcoords
cbind(top, topcoords) %>%
  st_drop_geometry()-> topfull 

#split
top1 <- topfull[1,]
top2 <- topfull[2,]

#get capitals without population 
africapitals %>%
  filter(is.na(pop)) %>%
  mutate(Nodata = "NA")-> nodat
nodat[2,] -> label_NA
st_coordinates(label_NA) -> label_NA_coords
cbind(label_NA, label_NA_coords) %>%
  st_drop_geometry() -> NA_countries
# viz ---------------------------------------------------------------------

ggplot()+

  #country polygons
  geom_sf(data = africountries,
          aes(group = name_pt),
          color = "white",
          fill = "lightgrey")+
  
  #highways
  geom_sf(data = afrihighway,
          aes(group = Name),
          color = "black",
          linetype = "dashed") +
  
  #label for two most populated cities and NA population cities
  geom_text_repel(data = top1,
                  aes(x = X, y = Y,label = capitalname),
                  nudge_x = -10,
                  family = "Bahnschrift")+
  geom_text_repel(data = top2,
                  aes(x = X, y = Y, label = capitalname),
                  nudge_x = 8,
                  family = "Bahnschrift")+
  geom_text_repel(data = NA_countries,
                  aes(x = X, y = Y, label = Nodata),
                  nudge_x = 8,
                  family = "Bahnschrift")+
  
  #capitals
  geom_sf(data = africapitals,
          aes(color = pop, size = pop)) +
  geom_sf(data = nodat, #no data
          aes(color = Nodata), color = "black", shape = 1, size = 3) +
  
  #scales & colors
  scale_color_gradient("population size \n [millions]",
                         breaks = c(seq(1000000,9000000, 2000000)),
                         limits = c(0,9000000),
                         labels = c("0-1 ", "1-3 ", "3-5 ", "5-7 ", "7-9 "),
                        low = "#3a1c71",
                       high = "#c33764") +
  scale_size_continuous("population size \n [millions]",
                        breaks = c(seq(1000000,9000000, 2000000)),
                        limits = c(0,9000000),
                        labels = c("0-1 ", "1-3 ", "3-5 ", "5-7 ", "7-9 "))+
  scale_x_continuous(limits = c(-20, 65))+

  #combine legend
  guides(color= guide_legend(), size=guide_legend())+
  
  #texts
  annotate(geom = "text", x =40, y = -32,   #inset caption
           label =  "Map by Tobias Stalder \nData: {afrilearndata}\n#TidyTuesday\nCRS: WGS 84",
           hjust = 0, size = 3, lineheight = 1,family = "Bahnschrift") +
  
  annotate(geom = "text", x =45, y = 40,   #title
           label =  "African Capitals",
           hjust = 0, size = 12, lineheight = 1,family = "Bahnschrift", angle = -55, hjust = 0) +
  annotate(geom = "text", x =41, y = 37,   #subtitle
           label =  "Population sizes & \ncontinental highways",
           hjust = 0, size = 6, lineheight = 1,family = "Bahnschrift", angle = -55, hjust = 0) +

  
  #theme
  theme_void()+
  theme(legend.position = c(0.2, 0.25), #inset legend -> P1
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        text = element_text(family = "Bahnschrift"))  -> P1

P1

#export
ggsave(P1, filename = paste0(here(), "/test.png"), type = "cairo-png", bg = "white",
       width = 25, height = 25, units = "cm")



