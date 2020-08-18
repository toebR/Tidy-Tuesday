
library(tidyverse)
library(ggforce)
library(reshape2)
library(ggpubr)
library(extrafont)
library(patchwork)
library(hrbrthemes)
loadfonts(device = "win")
fonts()

setwd(r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\R\tidytuesday\plant)")

## Data import and some cleaning
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
  summarise(Count = n()) %>%
  filter(threatened != 0) -> threat_dist

threat_dist$threatened=gsub("1", "Threatened", threat_dist$threatened) 
view(threat_dist)

cont_label = threat_dist %>%
  group_by(continent, group) %>%
  summarise()

# plot 1--------------------------------------------------------------------
ggplot(threat_dist) +
  geom_jitter(aes(x = group, y = continent, size = Count, color = group), alpha = .5,
              height = .3, width = .3) +
  scale_size(range = c(2,10))+
  scale_color_manual(values = c("#d0ece7", "#a9dfbf", "#73c6b6", "#27ae60", "#117a65", "#145a32"))+
  xlab("Taxonomic Group")+
  ylab("Continent")+
  theme_classic() +
  rotate_x_text()+
  theme(legend.position = "right",
        text = element_text(family = "Dubai", color = "lightgrey"),
        panel.background = element_rect(fill = "#293133", color = "#293133"),
        plot.background = element_rect(fill = "#293133", color = "#293133"),
        legend.background = element_rect(fill = "#293133", color = "#293133"),
        axis.line.x.bottom  = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.text = element_text(color = "lightgrey", size = 8),
        axis.ticks = element_blank()) +
  guides(color= FALSE) -> P1
P1

#prepare plot2

plants_long = plants_long %>%
  filter(threat_name != "red_list_category")

plants_long$threat_name = gsub("threat_*", "", plants_long$threat_name)
unique(plants_long$threat_name) #gsub this shit, look at tweet from thomas neitmann on how to do this efficiently

# plot2  ---------------------------------------------------------
#clean the reduced strings
plants_long$threat_name = gsub("AA", "Agriculture & Aquaculture", plants_long$threat_name)
plants_long$threat_name = gsub("BRU", "Biological Resource Use", plants_long$threat_name)
plants_long$threat_name = gsub("RCD", "Commercial Development", plants_long$threat_name)
plants_long$threat_name = gsub("ISGD", "Invasive Species", plants_long$threat_name)
plants_long$threat_name = gsub("EPM", "Energy & Mining", plants_long$threat_name)
plants_long$threat_name = gsub("CC", "Climate Change", plants_long$threat_name)
plants_long$threat_name = gsub("HID", "Human Intrusion", plants_long$threat_name)
plants_long$threat_name = gsub("P", "Pollution", plants_long$threat_name)
plants_long$threat_name = gsub("TS", "Transportation Corridor", plants_long$threat_name)
plants_long$threat_name = gsub("NSM", "Natural System Modification", plants_long$threat_name)
plants_long$threat_name = gsub("GE", "Geological Events", plants_long$threat_name)
plants_long$threat_name = gsub("NA", "Unknown", plants_long$threat_name)



threat_til = plants_long %>%
  group_by(threat_name, group) %>%
  summarise(count = sum(as.numeric(threatened)))

#plot2

ggplot(threat_til) +
  geom_col(aes(x =reorder(threat_name, count), y = count, fill = group, color = group), position = "dodge") +
  geom_curve(aes(x = 12, y = 210, xend = 11, yend = 200), curvature = .5, color = "lightgrey")+
  annotate(geom = "text", label = "Agriculture endangers\nthe most Species", x = 11, y =190,
           family = "Dubai", size = 3, color = "lightgrey") +
  geom_curve(aes(x = 10.2, y = 110, xend = 9.5, yend = 150), curvature = .5, color = "lightgrey")+
  annotate(geom = "text", label = "A lot of Threats\nare Unknown", x = 9, y =150,
           family = "Dubai", size = 3, color = "lightgrey") +
  ylab("Threatened Species") +
  xlab("Threat Activity") +
  scale_fill_manual(values = c("#d0ece7", "#a9dfbf", "#73c6b6", "#27ae60", "#117a65", "#145a32"))+
  scale_color_manual(values = c("#d0ece7", "#a9dfbf", "#73c6b6", "#27ae60", "#117a65", "#145a32"))+
  scale_y_continuous(limits = c(0, 220), expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+

  theme_classic() +
  rotate_x_text() +
  theme(legend.position = "right",
        text = element_text(family = "Dubai", color = "lightgrey"),
        panel.background = element_rect(fill = "#293133", color = "#293133"),
        plot.background = element_rect(fill = "#293133", color = "#293133"),
        legend.background = element_rect(fill = "#293133", color = "#293133"),
        axis.line.x.bottom  = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.text = element_text(color = "lightgrey", size = 8),
        axis.ticks = element_blank()) +
  guides(guide_legend(reverse = TRUE, title = "Taxonomic\nGroup")) -> P2
P2

#make infographic with patchwork:
P1 + P2 +
  plot_annotation(title = "The Loss of Biodiversity - A small Datastory\n",
                  subtitle = "We see a huge loss or endangered Species in Africa.\nMost Species affected belong to the Flowering Plants Taxonomic Group.\nAgriculture plays a Major Role in these Processes but there are also Uknown Drivers.\n\n",
                  caption = "plot by @toeab18\nSource: IUCN Red List",
                  theme = theme(
                    text = element_text(family = "Dubai", color = "lightgrey"),
                    plot.background = element_rect(fill = "#293133", color = "#293133"),
                    plot.title = element_text(size = 20),
                    plot.subtitle = element_text(size = 12)
                  )) -> plant_info
ggsave(plant_info, device = "png", filename = "infographic.png", width = 30, height = 15, units = "cm")
