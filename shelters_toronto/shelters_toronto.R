
# info --------------------------------------------------------------------
# 
# script by Tobias Stalder
# Dec 2020
# tidytuesday on shelters in toronto
# tobias-stalder.netlify.app



# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(extrafont)
library(mdthemes)
library(ggtext)

loadfonts(device = "win")

# load data ---------------------------------------------------------------

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')


# data manipulation -------------------------------------------------------

#1 convert to dataframe
shelters <- data.frame(shelters)

#2 calculate sum of capacities and occupancies per sector
shelters %>%
  group_by(occupancy_date, sector) %>%
  summarise(sum_cap = sum(capacity, na.rm = TRUE),
            sum_occ = sum(occupancy, na.rm = TRUE)) -> occ_cap

#3 summarise further for montly values
occ_cap %>%
  mutate(year_month = format(occupancy_date, "%Y %m"),
         year = year(occupancy_date)) %>%
  group_by(year,year_month, sector) %>%
  summarise(mean_cap = mean(sum_cap),
            mean_occ = mean(sum_occ)) -> monthly

#4 convert year_month to factor
factor(monthly$year_month) -> monthly$year_month

#5 filter against co-ed and youth
monthly %>%
  filter(sector %in% c("Families", "Men", "Women")) -> monthly

#6 check if occupation is > capacity at any point:
monthly$check <- ifelse(monthly$mean_cap < monthly$mean_occ, 1, 0)
sum(monthly$check)  #result: it's never the case



# dataviz: occupancy and capacity over time per client focus group --------

##the following creation of dfs is necessary because I only want to annotate
##selected facets and not all of them:

#1 create df for later sector annotation
ann_text <- data.frame(year_month = factor(c("2017 03",
                                             "2017 03",
                                             "2017 03")),
                       mean_cap = c(1850, 1500, 800),
                       lab = c("Men", "Families", "Women"),
                       year = c(2017,2017,2017),
                       sector = c("Men", "Families", "Women"))

#2 create df for later range annotation explanation
  ann_range <- data.frame(year_month = factor(c("2018 10",
                                                "2018 03")),
                        mean_cap = c(2400, 3500),
                        lab = c("Occupancy", "Capacity"),
                        year = c(2018,2018))

#3 create df for later geom_curve for range annotation
ann_curve <- data.frame(x = factor(c("2018 07",
                                     "2018 07")),
                        y = c(2810, 3200),
                        xend = factor(c("2018 08",
                                        "2018 05")),
                        yend = c(2400, 3500),
                        year = c(2018,2018))



# plot --------------------------------------------------------------------
ggplot(monthly) +
  
  #1 add custom scale lines:
  geom_hline(yintercept = 1000, color = "#A8A7A7", alpha = .6) +
  geom_hline(yintercept = 2000, color = "#A8A7A7", alpha = .6) +
  geom_hline(yintercept = 3000, color = "#A8A7A7", alpha = .6) +
 
  
  #2 add points for capacity and occupancy (looks like butt ends on the ranges)
  geom_point(aes(x = year_month, y = mean_cap, color = sector), size = 2) +
  geom_point(aes(x = year_month, y = mean_occ, color = sector, group = sector,
                 fill = sector), size = 2) +
  
  #3 plot actual range with geom_segment
  geom_segment(aes(x = year_month, xend = year_month, y = mean_occ, yend = mean_cap, color = sector),
               size = .7,
               lineend = "butt") +
  
  
  #4 add annotations that for groups
  geom_text(data = ann_text,
            aes(label = lab,
                x = year_month,
                y = mean_cap,
                color = sector), family = "Verdana") +
  
  #5 add annotations for the range
  geom_text(data = ann_range,
            aes(label = lab,
                x = year_month,
                y = mean_cap),
            family = "Verdana",
            color = "#A8A7A7",
            size = 3) +
  
  #6 add curve to the text annotations of the range
  geom_curve(data = ann_curve,
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = .3,
             color = "#A8A7A7")+
  
  #7 get rid of xlab
  xlab("") +
  
  #8 define ylab
  ylab("Monthly Mean\nOccupancy and Capacity\n")+
  
  #9 colors
  scale_fill_manual(values = c("#cc527a", "#e8175d", "white"))+
  scale_color_manual(values = c("#cc527a", "#e8175d", "white"))+
  
  #10 scale x axis, define new labels
  scale_x_discrete(breaks = c("2017 01", "2017 06", "2017 12",
                              "2018 01", "2018 06", "2018 12",
                              "2019 01", "2019 06", "2019 12"),
                   labels = c(rep(c("Jan", "Jun", "Dec"),3)))+
  
  #11 facets per year, plot facet label on bottom instead of top
  facet_wrap(.~year,
             scales = "free_x",
             switch = "x")+
  
  
  #12 title, subtitle, caption
  labs(title = "Shelters in Toronto",
       
       subtitle = "<br>This visualisation shows the monthly capacity and occupancy of all shelters in toronto.<br><br>
       The total capacity for <span style = 'color: #cc527a;'>families</span> increased rapidly in early 2018.<br>
       Shelters specifially targeting  <span style = 'color: white;'>women</span> are always occupied without a strong tendency<br>
       of a capacity increase. Since 2017, the shelter occupancy in programs for <span style = 'color: #e8175d;'>men</span> increased <br>without a simultanious upgrade in capacity.",
       
       caption = paste("Data Visualisation by Tobias Stalder",
                       "tobias-stalder.netlify.app",
                       "Data source: {opendatatoronto}",
                       sep = "\n"))+
  
  #13 theming
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(color = "transparent"),
        text = element_text(family = "Verdana", color = "#A8A7A7"),
        plot.title = element_text(family = "Bell MT", face = "bold", size = 40),
        plot.subtitle = element_markdown(lineheight = 1.2),
        plot.margin = unit(c(1,1,1,.5), units = "cm"),
        strip.text = element_text(size = 14, color = "#A8A7A7"),
        strip.background = element_rect(color = "#474747", fill = "#474747"),
        plot.background = element_rect(color = "#363636", fill = "#363636"),
        panel.background = element_rect(color = "transparent", fill = "#474747"),
        panel.grid = element_blank(),
        axis.text = element_text(family = "Verdana", color = "#A8A7A7")) +
  rotate_y_text()+
  
  #14 export
  ggsave(path = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\shelters_toronto)",
         filename = "shelter-toronto.png",
         width = 23,
         height = 27,
         units = "cm",
         dpi = 300)



