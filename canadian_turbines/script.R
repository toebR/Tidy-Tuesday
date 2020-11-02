library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggpubr)
library(prismatic)
library(extrafont)
loadfonts()

memory.limit(120000)
options(scipen = 999)
Sys.setenv(lang = "en_US")

setwd(r"(C:\Users\tobia\Desktop\ownprojects\dataviz\canadian_turbines)")

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

turbines<-st_as_sf(wind_turbine, coords = c("longitude", "latitude"))
st_crs(turbines) <- 4326
turbines <- st_transform(turbines, 3979)

canada <- rnaturalearth::ne_countries(country = "canada", returnclass = "sf")
canada <- st_transform(canada, 3979)

grid_all <- st_make_grid(canada,
                         cellsize =5 *20000,
                         # Kms
                         
                         what = "polygons",
                         square = FALSE
)

plot(grid_all)
st_as_sf(grid_all) -> gridSF

gridSF$turbines <- lengths(st_intersects(gridSF, turbines))

# color(c(
#   "#d1f2eb",
#   "#a3e4d7",
#   "#76d7c4",
#   "#48c9b0",
#   "#1abc9c",
#   "#17a589",
#   "#148f77",
#   "#117864",
#   "#0e6251"))

c(
  "white",
  # "#d1f2eb",
  "#a3e4d7",
  "#76d7c4",
  "#48c9b0",
  "#1abc9c",
  "#17a589",
  "#148f77",
  "#117864",
  "#0e6251") -> pal1

ggplot(gridSF) +
  geom_sf(aes(fill = turbines), color = "darkgrey") +
  scale_fill_gradientn("Number of \nWind Turbines",colors = pal1)+
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(family = "Nirmala UI")) +
  guides(fill = guide_colorsteps(title.position = "top",
                                 barwidth = 15, barheight = .5)) -> map2
ggsave(plot = map2, filename = "hexbin_map.png", dpi = 500, width = 4, height = 4, units = "in")



# classify roto diameter and hub_height for heatmap -----------------------
turbines$rotor_class <- cut(turbines$rotor_diameter_m,
                            breaks = c(0,10,20,30,40,50,60,70,80,
                            90,100,110,120,130,140,150),
                            labels = c("0-10", "11-20", "21-30", "31-40",
                            "41-50", "51-60", "61-70", "71-80", "81-90", 
                            "91-100", "101-110", "111-120", "121-130", 
                            "131-140", "141-150"))

turbines$hub_class <- cut(turbines$hub_height_m,
                            breaks = c(0,10,20,30,40,50,60,70,80,
                                       90,100,110,120,130,140),
                            labels = c("0-10", "11-20", "21-30", "31-40",
                                       "41-50", "51-60", "61-70", "71-80", "81-90", 
                                       "91-100", "101-110", "111-120", "121-130", 
                                       "131-140"))

turbines %>%
  st_drop_geometry()%>%
  group_by(rotor_class, hub_class, .drop = FALSE) %>%
  summarise(kw = mean(turbine_rated_capacity_k_w, na.rm = TRUE)) -> turbines_capacity

turbines_capacity$kw <- ifelse(is.nan(turbines_capacity$kw),
                               0,
                               turbines_capacity$kw)


  ggplot(turbines_capacity,aes(x = rotor_class, y = hub_class, fill = kw))+
  geom_tile() +
  # scale_fill_viridis_c("Mean Turbine\nCapacity [kw]",breaks = c(seq(400,3600,400)))+
  scale_fill_gradientn("Mean Turbine\nCapacity [kW]",
                       breaks = c(seq(0,3800,500)),
                       limits = c(0,4000),
                       colours = c(
                         "white",
                         "#d1f2eb",
                         "#a3e4d7",
                         "#76d7c4","#48c9b0",
                                   "#1abc9c",
                                   "#17a589",
                                   "#148f77",
                                   "#117864",
                                   "#0e6251"
))+
  xlab("Rotor Diameter [m]") +
  ylab("Hub Height [m]")+
  theme_light() +
  theme(text = element_text(family  = "Nirmala UI"),
    legend.position = "right",
        legend.title.align = 1)+
  guides(fill = guide_colorsteps(title.position = "top",
                                 barwidth = .5, barheight = 15)) +
  rotate_x_text() +
  coord_fixed() +
  ggsave("height_rotor.png", dpi = 500, width = 5, height = 5, units = "in")

  
#add up project per province and calculate total project capacities per province.
turbines %>%
  group_by(province_territory,.drop = FALSE) %>%
  summarise(project_capacity = sum(total_project_capacity_mw)) %>%
  ggplot(aes(x = reorder(province_territory, project_capacity),
             y = project_capacity)) +
  geom_col(fill = "#117864") +
  ylab("Cummulative turbine\nprojects capacity [MW]")+
  xlab("Canadian Province") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(family  = "Nirmala UI"),
        legend.position = "right",
        legend.title.align = 1) +
  ggsave("provinces.png", dpi = 500, width = 5, height = 4, units = "in")


turbines %>%
  group_by(province_territory,.drop = FALSE) %>%
  summarise(project_capacity = sum(total_project_capacity_mw)) %>%
  ggplot(aes(x = reorder(province_territory, project_capacity),
             y = project_capacity)) +
  geom_col(fill = "#117864") +
  ylab("Cummulative turbine\nprojects capacity [MW]")+
  xlab("Canadian Province") +
  # coord_flip() +
  theme_light() +
  theme(text = element_text(family  = "Nirmala UI"),
        legend.position = "right",
        legend.title.align = 1) +
  ggsave("provinces_wide.png", dpi = 500, width = 5, height = 4, units = "in")
