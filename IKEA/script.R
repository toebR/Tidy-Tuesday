#script by Tobias Stalder
#tobias-stalder.netlify.app

#TidyTuesday on IKEA furniture

#dataanalysis on ikea furniture

library(tidyverse)
library(ggpubr)
library(ggthemes) 
library(hrbrthemes)
library(extrafont)
font_import()
loadfonts(device="win")
loadfonts(device = "postscript")
fonttable()


ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# plot on what ikea actually sells (items per category) and the total price range in US$

ikea %>%
  select(category, name, price) %>%
  group_by(category) %>%
  summarise(price_low = min(price),
            price_high = max(price)) -> price_range
  
ikea %>%
  select(category, name, price) %>%
  group_by(category) %>%
  count() -> category_items

left_join(category_items, price_range, by = "category") -> categories

categories$price_low_dollar <- categories$price_low * 0.27
categories$price_high_dollar <- categories$price_high * 0.27


ggplot(categories) +
  geom_segment(aes(x = price_low_dollar, xend = price_high_dollar, y = category, yend = category,
                   color = n))+
  geom_point(aes(x = price_high_dollar, y = category, size = price_high_dollar, color = n), shape = 15) +
  geom_point(aes(x = price_low_dollar, y = category, size = price_low_dollar, color = n), shape = 15) +
  scale_x_continuous(breaks = c(seq(0,2500,500)))+
  scale_color_gradientn("Amount of Products", colors = c("lightblue", "blue", "darkblue"),
                        breaks = c(seq(0,700,100)))+
  scale_size_continuous(limits = c(0,10000), breaks = c(seq(0,10000,1000)), range = c(1,20) )+
  ggtitle("IKEA's Range of Products") +
  labs(subtitle = "Price Ranges and Number of Individual Products per Category",
       caption = "Plot by Tobias Stalder\nSource: https://www.kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping")+
  xlab("Price Range [US$]\ntransformed from Suadi Riyal (3.11.2020)") +
  guides(size = guide_none(),
         color = guide_colorsteps(barwidth = 25, barheight = .5)) +
  theme_minimal()+
  theme(text = element_text(family = "Verdana", color = "#04348c"),
    plot.caption = element_text(hjust = 1),
    axis.text = element_text(family = "Verdana", color = "#04348c"),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(color = "#04348c", fill = "#FFDA1A") ,
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(color = "#FFDA1A", fill = "#FFDA1A"),
    legend.position = "bottom") +
  ggsave(filename = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\IKEA\plot.svg)",
         width = 25, height = 20, units = "cm")

#font will be changed in inkscape because extrafont cant install Futura Press somehow..
