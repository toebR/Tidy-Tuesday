
# meta --------------------------------------------------------------------

# tidytuesday on starbucks data
# by Tobias Stalder
# 08.01.2022


# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(extrafont)
loadfonts()
library(ggtext)
library(Cairo)

# load data ---------------------------------------------------------------
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')



# data wrangling ----------------------------------------------------------
#for correlation map, make a selection of drinks (e.g. > than xy caffeine in it, or a size limitation of the drinks)
starbucks %>%
  filter(size %in% c("tall", "grande", "venti")) %>%
  dplyr::filter(!grepl('tea|Youthberry|Tea|Apple|Hibiscus|Smoothie|Lemonade|Flat|Hot Chocolate|Frappuccino|Cool Lime|decaf', product_name)) %>%
  mutate(size = factor(size, levels = c("tall", "grande", "venti"))) %>%
  mutate(WHO_sugar = 50)-> data

#Of product_name and size duplicates, only take entry with max caffeine to avoid overplotting
data %>% group_by(product_name,size) %>% slice(which.max(caffeine_mg)) -> dat_sliced


#create dfs for labelling thresholds and explanations
FDA <- data.frame(size  = factor("tall"),
                  caffeine_mg = 400,
                  y = 10)

WHO <- data.frame(size  = factor("tall"),
                  caffeine_mg = 260,
                  y = 20)

arrow_WHO <- data.frame(size = factor("tall"),
                        x = 250, xend = 170, y = 20, yend = 22)

arrow_maxcaff <- data.frame(size = factor("grande"),
                        x = 250, xend = 170, y = 20, yend = 22)

arrow_maxsod <- data.frame(size = factor("venti"),
                        x = 250, xend = 210, y = 6, yend = 9)

# viz ---------------------------------------------------------------------



ggplot(dat_sliced)+
  geom_vline(xintercept = 400, linetype = "dashed", color = "black")+
  # geom_vline(xintercept = 0, linetype = "solid", color = "black")+
  geom_segment(aes(x = caffeine_mg-10, y =  reorder(product_name,caffeine_mg), xend = 0, yend = product_name, color = sodium_mg), size = 1.5, alpha = 0.8)+
  geom_curve(data = arrow_WHO, aes(x = x, y =y, yend = yend, xend = xend), size = 0.3)+
  geom_curve(data = arrow_maxsod, aes(x = x, y =y, yend = yend, xend = xend), size = 0.3, arrow = arrow(length = unit(0.3, "inches")))+
  geom_point(aes(x = caffeine_mg, y = reorder(product_name,caffeine_mg), size = sugar_g, color = sodium_mg), shape = 16)+
  geom_point(aes(x = caffeine_mg, y = reorder(product_name,caffeine_mg), size = WHO_sugar), shape = 1)+
  geom_text(data = FDA, aes(x = caffeine_mg, y = y), label = "Daily max. caffeine by FDA", angle = 90,
            vjust = 1.6, size = 4, family = "Bahnschrift")+
  geom_text(data = WHO, aes(x = caffeine_mg, y = y), label = "Circle: Daily max. sugar\nby WHO (50g)", angle = 0,
            vjust = 1.3, size = 4, family = "Bahnschrift")+
scale_x_continuous(expand = c(0,0), limits = c(0,500), breaks = seq(100,400,100))+
  scale_size_continuous("Sugar [g]", range = c(2,10))+
  scale_color_gradient("Sodium [mg]", low = "#413026", high = "#006442")+
  xlab("Caffeine [mg]")+
  ylab("")+
  labs(title = "What You Might Find In Your **Coffee**",
       subtitle = "Selected Starbucks products and their ingredients<br>",
       caption = "Datavizualisation by Tobias Stalder \n Data | ")+
  facet_wrap(.~size, ncol = 4)+
  # theme_minimal()+
  theme(plot.title = element_markdown(hjust = 0, size = 24),
        plot.subtitle = element_markdown(hjust = 0, size = 16),
    panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Bahnschrift"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#f8f4e6"),
        panel.background = element_rect(fill = "#f8f4e6"),
        legend.background = element_rect(fill = "#f8f4e6"),
        strip.background = element_rect(fill = "#006442"),
        strip.text = element_text(color = "white", size = 12),
        legend.spacing.x = unit(0.2, "cm")) +
  guides(color = guide_colorbar(barwidth = 12, barheight = 0.5, title.position = "top"),
         size = guide_legend(title.position = "top",override.aes=list(colour="#413026"))) -> P

P

ggsave(P, filename = paste0(here(), "starbucks_coffee.png"), type = "cairo-png", height = 12, width = 18)

             