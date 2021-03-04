#Tobias Stalder
#March 2021
#script on tidytuesday on superbowl ads
#tobias-stalder.netlify.app

options(scipen = 999)

# load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(Cairo)
library(ggbump)
library(ggtext)
library(extrafont)
loadfonts()


# load data ---------------------------------------------------------------

dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


# glimpse & wrangling -----------------------------------------------------

#select data (we want year, brand, strategies , YT views, likes and dislikes & comment count)

dat_sel <- dat %>%
  select(year, like_count,
         dislike_count)

#change to long format in two categories
dat_sel %>%
  group_by(year) %>% #summarise by year
  summarise(like_counts = sum(like_count, na.rm = TRUE),
            dislike_counts = sum(dislike_count, na.rm = TRUE),
            n_ads = n())%>%
  mutate(dislikes = dislike_counts *-1)-> dat_plot





# plot --------------------------------------------------------------------


ggplot(dat_plot) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .5)+
  
  geom_hline(yintercept = -100000, linetype = "solid", size = .3, color = "darkgrey")+
  geom_hline(yintercept = 100000, linetype = "solid", size = .3, color = "darkgrey")+
  geom_hline(yintercept = 200000, linetype = "solid", size = .3, color = "darkgrey")+
  geom_hline(yintercept = 300000, linetype = "solid", size = .3, color = "darkgrey")+
  
  annotate(geom = "text", label = "Ad from 2012 by Dorrito is uploaded\non YT in 2013 and hits > 275k likes",
           x = 2015, y = 360000, size = 2.5, family = "Segoe UI")+
  
  geom_bump(aes(x = year, y = like_counts), color = "#383e56", size = 1.5) +
  geom_point(aes(x = year, y = like_counts), color = "#383e56", size = 3)+
  geom_point(aes(x = year, y = like_counts), color = "white", size = 2)+
  
  geom_bump(aes(x = year, y = dislikes), color = "#fb743e", size = 1.5) +
  geom_point(aes(x = year, y = dislikes), color = "#fb743e", size = 3) +
  geom_point(aes(x = year, y = dislikes), color = "white", size = 2) +
# 
  geom_text(aes(x = year, y = -25000, label = n_ads), size = 2)+
  annotate(geom = "text", x = 2000.5, y = -40000, label = "Nr. of Ads", size = 2.5)+
  
  xlab("")+
  ylab("")+
  
  labs(title = "Superbowl Commercials on Youtube",
subtitle = "<span style = 'color: #383e56;'>**Likes**</span> and <span style = 'color: #fb743e;'>**Dislikes**</span> on all Ads from Superbowls from 2000 to 2020<br>",
caption = "Plot by **Tobias Stalder** | tobias-stalder.netlify.app<br>Data from **FiveThirtyEight** | originally by superbowl-ads.com<br>**#TidyTuesday**") +
  
  scale_y_continuous(breaks = c(seq(-100000, 300000, 100000)),
                     labels = (c("100000", "0", "100000", "200000", "300000")))+

  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.caption = element_markdown(hjust = 0, color = "darkgrey"),
                          plot.subtitle = element_markdown(),
                          text = element_text(family = "Segoe UI"),
                          plot.title = element_text(size = 20))-> p1

p1

ggsave(p1, dpi = 300, width = 20, height = 20, unit = "cm", type = "cairo-png", filename = "superbowl_ads.png")

