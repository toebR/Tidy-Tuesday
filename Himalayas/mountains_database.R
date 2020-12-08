#by tobias stalder
#september 2020

library(tidyverse)
library(ggrepel)
library(ggmap)
library(sf)
library(prismatic)
library(extrafont)
library(ggbump)
library(ggpubr)
loadfonts(device = "win")

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


#filter 10 most expedited peaks
expeditions %>%
  group_by(peak_name, peak_id) %>%
  count(peak_name) %>%
  arrange(-n) %>%
  head(10) -> top_exp_peaks

#approximate geocoding the top peaks
Everest_geo <- c(27.987691, 86.924682, "EVER")
Ama_Dablam_geo <- c(27.861729, 86.861545, "AMAD")
Cho_Oyu_geo <- c(28.095539, 86.661622, "CHOY")
Manaslu_geo <- c(28.549328, 84.559248, "MANA")
Lhotse_geo <- c(27.962527, 86.933630, "LHOT")
Dhaulagiri_I_geo <- c(28.696669, 83.494496, "DHA1")
Makalu_geo <- c(27.885826, 87.088115, "MAKA")
Baruntse_geo <- c(27.871488, 86.982502, "BARU")
Pumori_geo <- c(28.009453, 86.825175, "PUMO")
Annapurna_I_geo <- c(28.614478, 83.872508, "ANN1")

rbind(Everest_geo,
      Ama_Dablam_geo,
      Cho_Oyu_geo,
      Manaslu_geo,
      Lhotse_geo,
      Dhaulagiri_I_geo,
      Makalu_geo,
      Baruntse_geo,
      Pumori_geo,
      Annapurna_I_geo) -> geo_codes

geo_codes <- data.frame(geo_codes)
colnames(geo_codes) <- c("X", "Y", "peak_id")
geo_codes <- geo_codes %>%
  mutate(X = as.numeric(X),
         Y = as.numeric(Y))
#join geocodes to all other dataframes by peak_id and filter the top 10 of most expeditions

members_geo <- members %>%
  filter(peak_id %in% top_exp_peaks$peak_id) %>%
  left_join(geo_codes, by = c("peak_id"))

expeditions_geo <- expeditions %>%
  filter(peak_id %in% top_exp_peaks$peak_id) %>%
  left_join(geo_codes, by = c("peak_id"))

peaks_geo <- peaks %>%
  filter(peak_id %in% top_exp_peaks$peak_id) %>%
  left_join(geo_codes, by = c("peak_id"))


# ideas on data analysis and visualisation for the top 10 expedite --------

theme_set(theme_classic() + theme(
  text = element_text(family = "Bahnschrift"),
  plot.subtitle = element_text(size = 10)))

#get height and first ascended year on expeditions data per peak (join from peaks_geo)
ggplot(peaks_geo) +
  geom_point(aes(y = height_metres, x = first_ascent_year), size = 4, alpha = .2, color = "blue") +
  geom_point(aes(y = height_metres, x = first_ascent_year), size = 2, alpha = .3, color = "blue") +
  geom_point(aes(y = height_metres, x = first_ascent_year), size = 1, alpha =.5, color = "blue") +
  geom_point(aes(y = height_metres, x = first_ascent_year), size = .5, alpha =.7, color = "blue") +
  ggrepel::geom_text_repel(aes(y = height_metres, x = first_ascent_year, label = peak_name),
                           nudge_y = 85, family = "Bahnschrift", size = 3, segment.alpha = 0) +
  scale_x_continuous(limits = c(1950, 1965), breaks = c(seq(1950, 1965, 1)) )+
  xlab("Year") +
  ylab("Height [m]") +
  ggtitle("Year of First Successfull Expedition") +
  labs(subtitle = "There is no apparent relationship between the height\nof the mountains and their first ascent.") +
  rotate_x_text()-> Peak_ascent
ggsave(Peak_ascent, filename = "peaks_ascent.png", width = 12, height = 12, units = "cm", dpi = 300, type = "cairo")

#show age distribution over the years as geom_line per year (and maybe hired or non-hired?)
members_geo %>%
  filter(citizenship == "Nepal" & hired == TRUE) %>%
  group_by(year,age, hired) %>%
  count() -> nepali

ggplot(nepali, aes(x= year, y = age, fill = n)) +
  geom_tile() +
  scale_x_continuous(limits = c(1921, 2019), breaks = c(1920,
                                                        1930,
                                                        1940,
                                                        1950,
                                                        1960,
                                                        1970,
                                                        1980,
                                                        1990,
                                                        2000,
                                                        2010,2019)) +
  scale_fill_gradientn(colours = c("#0d47a1","#2196f3", "#4fc3f7", "#bbdefb", "#e1f5fe"))+
  ggtitle("Hired Nepali Citizens on Expeditions") +
  labs(subtitle = "The hiring of Nepali citizens increases strongly from the 90's onwards.\nMost people hired are 20-50 years old.") +
  guides(fill = guide_colorsteps()) +
  xlab("Year")+
  rotate_x_text()-> nepali_hired
ggsave(nepali_hired, filename = "nepali_hired.png", width = 15, height = 12, units = "cm", dpi = 300, type = "cairo")


#plot dead expedition members per year on these 10 peaks
members_geo %>% 
  group_by(year, success, died) %>%
  count() -> suc_death

  
suc_death %>%
  filter(died == TRUE) %>%
  ggplot() +
  geom_bump(aes(x = year, y = n), color = "#0d47a1") +
  geom_point(aes(x = year, y = n), color = "#0d47a1")+
  annotate(geom = "text", family = "Bahnschrift", angle = 90, label = "missing data\nin the early expedition years",
           x = 1942, y = 10, size = 3)+
  ggtitle("Dead Expedition Members") +
  labs(subtitle = "A lot of people die on these mountains every year.") +
  xlab("Year")+
  ylab("Deaths")+
  scale_x_continuous(limits = c(1921, 2019), breaks = c(1920,
                                                        1930,
                                                        1940,
                                                        1950,
                                                        1960,
                                                        1970,
                                                        1980,
                                                        1990,
                                                        2000,
                                                        2010,
                                                        2019)) +
  scale_y_continuous(breaks = c(0,5,10, 15, 20, 25, 30), limits = c(0,30)) -> Exp_Deads
ggsave(Exp_Deads, filename = "exp_deads.png", width = 25, height = 12, units = "cm", dpi = 300, type = "cairo")

#plot on expeditions through the years.
expeditions_geo %>%
  group_by(year, highpoint_metres) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x= year, y = n, fill = highpoint_metres), width = .7, position = "dodge", color = "white") +
  # scale_fill_viridis_c() +
  ggtitle("Expeditions and their Max Altitute Reached") +
  labs(subtitle = "Number of expeditions increases over time.\nMost expeditions reach 7000-8000 metres.")+
  guides(fill = guide_colorsteps(title = "Altitude [m]")) +
  scale_fill_gradientn(colours = c("#0d47a1","#2196f3",
                                   # "#4fc3f7",
                                   "#bbdefb")) +
                                   # "#e1f5fe")) +
  scale_x_continuous(limits = c(1921, 2019), breaks = c(1920,
                                                        1930,
                                                        1940,
                                                        1950,
                                                        1960,
                                                        1970,
                                                        1980,
                                                        1990,
                                                        2000,
                                                        2010,2019)) -> Exp_Altitude
ggsave(Exp_Altitude, filename = "exp_alt.png", width = 25, height = 12, units = "cm", dpi = 300, type = "cairo")





#make map that shows the ten peaks with peak_name and height (get very reduced basemap!)
st_as_sf(peaks_geo, coords = c("Y", "X")) -> peaks_sf

coords <- data.frame(st_coordinates(peaks_sf))
                     
height <- max(coords$Y) - min(coords$Y)
width <- max(coords$X) - min(coords$X)

borders <- c(bottom  = min(coords$Y)  - 0.5 * height, 
             top     = max(coords$Y)  + 0.5 * height,
             left    = min(coords$X) - 0.1 * width,
             right   = max(coords$X) + 0.1 * width)
                     
 map2 <- get_stamenmap(borders, zoom = 10, maptype = c("terrain-background"), color = "bw")

ggmap(map2) +
  geom_point(data = peaks_geo, inherit.aes = FALSE, aes(x = Y, y = X,color = height_metres), color = "#0000FF",
          size = 1, alpha = 1, shape = 16) +

  geom_point(data = peaks_geo, inherit.aes = FALSE, aes(x = Y, y = X,color = height_metres), color = "#0000FF",
             size = 1, alpha = .5, shape = 16) +

  geom_point(data = peaks_geo, inherit.aes = FALSE, aes(x = Y, y = X,color = height_metres), color = "#0000FF",
             size = 3, alpha = .4, shape = 16) +
  
  geom_point(data = peaks_geo, inherit.aes = FALSE, aes(x = Y, y = X,color = height_metres), color = "#0000FF",
             size = 5, alpha = .3, shape = 16) +
  
  geom_label_repel(data = peaks_geo, inherit.aes = FALSE,
                   aes(x = Y, y = X, label = paste0(peak_name,"\n [",height_metres, "m]")),
                   family = "Bahnschrift",
                   fill = "white",
                   alpha = .7,
                   nudge_y      = 1.2,
                   direction    = "x",
                   angle        = 90,
                   vjust        = 0,
                   segment.size = .5,
                   size = 4,
                   point.padding = .7) +
  theme_void() +
  ggtitle("Top 10 Most Climbed Peaks from 1905 to 2019") +
  theme(plot.title = element_text(family = "Bahnschrift"))-> peak_map
ggsave(peak_map, filename = "map.png", width = 25, height = 15, units = "cm", dpi = 300, type = "cairo")



# patchwork ---------------------------------------------------------------
#plot composing will be done in inskcape
