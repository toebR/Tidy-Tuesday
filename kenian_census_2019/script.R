#tidytuesday viz for kenian census 2019 data
#by tobias stalder
#Jan 2021
#see more at tobias-stalder.netlify.app


# load libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(rKenyaCensus)
library(sf)
library(ggtext)
library(extrafont)
library(Cairo)
here()

# load data directly from package---------------------------------------------------------------
shp <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

glimpse(shp)


rural_pop <- rKenyaCensus::V2_T2.2a
urban_pop <- rKenyaCensus::V2_T2.2b
# glimpse data ------------------------------------------------------------

rural_pop
urban_pop

#will focus on total household and persons/km^2 on the county level for rural and urban

# join data of interest together

rural_pop %>%
  select(County, Households_Total, `Density(Persons per Sq km)`) %>%
  mutate(class = "rural") -> rural_pop

urban_pop %>%
  select(County, Households_Total, `Density(Persons per Sq km)`) %>%
  mutate(class = "urban") -> urban_pop

rbind(rural_pop, urban_pop) -> pop

#check join IDs
join_check <- function(data, ID1, ID2) {
  data %>%
    mutate(join_check = ifelse(ID1 %in% ID2, 
                               "ok",
                               "nope"))
}

join_check(pop, pop$County, shp$County)


#claen up join ID
# gsub("NAIROBI CITY", "NAIROBI", pop$County) -> pop$County

#drop "Kenya" county since this is the whole country most likely
pop %>%
  filter(County != "KENYA") -> pop

#re-check IDs after cleaning
join_check(pop, pop$County, shp$County)

#join geodata information on pop
st_as_sf(left_join(pop, shp, by = "County")) -> pop_shp

#drop unnecessary data and rename the density and categorise the density
summary(pop_shp)

pop_shp %>%
  rename(density = `Density(Persons per Sq km)`) %>%
  select(County, Households_Total, density, class, Population, Area, geometry) -> pop_shp



# dataviz (just for urban class - Time issues)-----------------------------------------------------------------
pop_shp %>%
  filter(class == "urban") -> pop_urb

theme_set(theme_void() +
            theme(plot.background = element_rect(color = "white"),
                  legend.position = "bottom",
                  legend.text = element_text(angle = 0),
                  plot.title = element_markdown(family = "Times New Roman",size = 35, hjust = .5),
                  plot.subtitle = element_text(family = "Times New Roman", size = 18, hjust = .5),
                  plot.margin = margin(1,1,1,1, unit = "cm")))


ggplot(pop_urb) +
  geom_sf(aes(fill = density), color = "white", size = .5) +
  scale_fill_gradient("Population Density\n[Persons / Km^2]",low = "#360940", high = "#F05F57",
                      breaks = c(seq(0,6500, 1000))) +

  
  #add underlined title
  labs(title = "Urban Population in Kenya",
       subtitle = "\nPopulation Density of People living in Urban Areas",
       caption = "<br><br>Map by **Tobias Stalder** | tobias-stalder.netlify.app<br>
       Data by **{rkenyacensus}** | github.com/Shelmith-Kariuki/rKenyaCensus<br>
       #TidyTuesday")+
  
  theme(plot.caption = element_markdown(family = "Times New Roman", size = 10, hjust = .5,
                                        lineheight = 1.5))+
  
  #guides
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5,title.position = "top", title.hjust = .5)) +
  ggsave(path = paste(here()), filename = "map.png", width = 21, height = 29, units = "cm", dpi = 300,
         type = "cairo-png")




