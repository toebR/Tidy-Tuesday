#by Tobias Stalder

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\American_Slavery"
setwd(dir)
#libraries
library(tidyverse)
library(sf)
library(reshape2)
library(scales)
library(ggpubr)
library(ggforce)
library(ggmap)
library(markdown)
#load dta
blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')
# 
# view(slave_routes)



#since only a little time, lets mine some summaries about census, african names and slave routes.
#in slave routes, e.g. which were the ports that the most slaves arrived? which was the ship that transported the most slaves?
#for african names, which was the gender and age distribution of slaves that were freed during their transport?

#first off, lets look at the census
#create a dummy column for division + region
census$regdiv = paste(census$region, census$division, sep = "_")

regions = census %>%
  filter(regdiv %in% c("Midwest_NA", "Northeast_NA", "South_NA", "West_NA"))

regions$regdiv

#get long format of numerics
regions_long = regions %>%
  melt(id.vars = c("year", "region", "regdiv"))

#rename regions
regions_long$regdiv = gsub("_NA", "", regions_long$regdiv)
colnames(regions_long) = c("year", "region", "regdiv", "category", "value")
regions_long

#filter away the division numeric and the totals
regions_long = regions_long %>%
  filter(category != "division") %>%
filter(category != "total") %>%
filter(category != "black")

regions_long$value = as.numeric(regions_long$value)

regions_long

#plot
ggplot(regions_long, aes(x = year, y = value, color = category)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~regdiv) +
  labs(title = "North-American Census from 1790 to 1870") +
  scale_y_continuous(labels = comma, limits = c(0, 13000000), breaks = c(seq(0, 13000000, 2000000))) +
  scale_x_continuous(limits = c(1790,1870), breaks = c(seq(1790, 1870, 10))) +
  xlab("Year") +
  ylab("Nr. of People")+
  theme_bw() +
  theme(text = element_text(color = "lightgrey"),
    legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(color = "#2E2B42", fill = "#2E2B42"),
        panel.background = element_rect(color = "#2E2B42", fill = "#2E2B42"),
        legend.background = element_rect(color = "#2E2B42", fill = "#2E2B42"),
    legend.key = element_rect(color = "#2E2B42", fill = "#2E2B42"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "darkgrey"),
    axis.text = element_text(color = "lightgrey")) +
  rotate_x_text() -> census_plot
census_plot
ggsave(census_plot, filename = "census.png", width = 15, height = 15, units = "cm")

c("#2E2B42", "#8EA324", "#FFFFFF")


#now look at the slave routes, which ships transported the most slaves, which harbor originated the most and which harbor arrived the most.
slave_routes

coords = geocode(slave_routes$port_origin)

pd = slave_routes %>% gather_set_data(3:7)
view(pd)

ggplot(head(pd), aes(x = x, id = id, split = y, value = n_slaves_arrived)) +
  geom_parallel_sets()
#then look at the african names, i.e. slaves freed during transport


