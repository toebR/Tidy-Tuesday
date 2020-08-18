
library(tidyverse)
library(ggforce)
library(reshape2)
library(ggpubr)
library(extrafont)
library(patchwork)
library(hrbrthemes)
# extrafont::font_import()
loadfonts(device = "win")
fonts()

setwd(r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\R\tidytuesday\plant)")

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
glimpse(plants)
view(plants)
plants = plants %>%
  select(-c(action_LWP, action_SM, action_EA,
            action_LP, action_NA, action_RM))
plants_long = melt(plants, id.vars = c("binomial_name", "country", "continent", "group", "year_last_seen"))
colnames(plants_long) = c("binomial_name", "country", "continent", "group", "year_last_seen", "threat_name", "threatened")


#summarise by threat type and country
plants_long %>%
  group_by(continent,country, group,threatened) %>%
  summarise('Threatened or Extinct' = n()) %>%
  filter(threatened != 0) -> threat_dist

threat_dist$threatened=gsub("1", "Threatened", threat_dist$threatened) 
view(threat_dist)

cont_label = threat_dist %>%
  group_by(continent, group) %>%
  summarise()

# plot --------------------------------------------------------------------
ggplot(threat_dist) +
  geom_jitter(aes(x = group, y = continent, size = `Threatened or Extinct`, color = continent), alpha = .5,
              height = .3, width = .3) +
  scale_color_discrete(guide = "legend") +
  scale_size(range = c(2,10))+
  xlab("Taxonomic Group")+
  ylab("Continent")+
  theme_classic() +
  rotate_x_text()+
  theme(legend.position = "bottom",
        text = element_text(family = "Dubai"))+
  guides(color= FALSE)
#BUID SOME ANNOTATION TEXT ON WHATS INTERESTING WITH GGFORCE!


plants_long = plants_long %>%
  filter(threat_name != "red_list_category")

plants_long$threat_name = gsub("threat_*", "", plants_long$threat_name)
unique(plants_long$threat_name) #gsub this shit, look at tweet from thomas neitmann on how to do this efficiently

# plot2  ---------------------------------------------------------
#make plot on most common threat and count of these threats by country, continent
plants_long$threat_name = gsub("AA", "Agriculture & Aquaculture", plants_long$threat_name)
plants_long$threat_name = gsub("BRU", "Biological Resource Use", plants_long$threat_name)
plants_long$threat_name = gsub("RCD", "Commercial Development", plants_long$threat_name)
plants_long$threat_name = gsub("ISGD", "Invasive Species", plants_long$threat_name)
plants_long$threat_name = gsub("EPM", "Energy Production & Mining", plants_long$threat_name)
plants_long$threat_name = gsub("CC", "Climate Change", plants_long$threat_name)
plants_long$threat_name = gsub("HID", "Human Intrusion", plants_long$threat_name)
plants_long$threat_name = gsub("P", "Pollution", plants_long$threat_name)
plants_long$threat_name = gsub("TS", "Transportation Corridor", plants_long$threat_name)
plants_long$threat_name = gsub("NSM", "Natural System Modification", plants_long$threat_name)
plants_long$threat_name = gsub("GE", "Geological Events", plants_long$threat_name)
plants_long$threat_name = gsub("NA", "Unknown", plants_long$threat_name)



#try multigsub (install java 64bit first and do library(rJava)!
#plot: try a tile plot that shows plants, threats and fill = count.
threat_til = plants_long %>%
  group_by(threat_name, group) %>%
  summarise(count = sum(as.numeric(threatened)))

ggplot(threat_til) +
  geom_col(aes(x =reorder(threat_name, count), y = count, fill = group), position = "dodge") +
  geom_curve(aes(x = 12, y = 220, xend = 11, yend = 200), curvature = .5, color = "grey")+
  annotate(geom = "text", label = "Agriculture endangers\nthe most Species", x = 11, y =190,
           family = "Dubai", size = 3) +
  geom_curve(aes(x = 12, y = 220, xend = 11, yend = 200), curvature = .5)+
  annotate(geom = "text", label = "A lot of Threats\nare Unknown", x = 10, y =170,
           family = "Dubai", size = 3) +
  annotate(geom = "text", label = "Flowering Plants\n are the most affected", x = 5, y =120,
           family = "Dubai", size = 5) +
  ylab("Threatened Species") +
  xlab("Threat Activity") +
  scale_fill_manual(values = c("#d0ece7", "#a9dfbf", "#73c6b6", "#27ae60", "#117a65", "#145a32"))+
  scale_y_continuous(limits = c(0, 220))+

  theme_classic() +
  rotate_x_text() +
  theme(legend.position = "right",
        text = element_text(family = "Dubai")) +
  guides(guide_legend(reverse = TRUE, title = "Taxonomic\nGroup")) -> P2
P2
ggsave(P2, "P2.png", width = 20, height = 20, units = "cm", device = "cairo")
#add annotation that shows highest and lowest as well as unkniwn threat and which group is the most at risk
