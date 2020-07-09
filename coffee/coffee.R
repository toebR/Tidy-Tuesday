# Title     : coffee tidytuesday
# Objective : TODO
# Created by: tobias S, @toeb18, toebR
# Created on: 08.07.2020

#data vizualisation on the tidytuesday coffee dataset

#load libraries
library(tidyverse)
library(ggpubr)
library(reshape2)
library(ggcorrplot)
library(ggcharts)
library(tmaptools)
library(prismatic)
library(patchwork)
library(gridExtra)


palette_explorer()


#set working dir
dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\coffee"
setwd(dir)

#load data
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee_ratings = data.frame(coffee_ratings, stringsAsFactors = FALSE)

#do a vizualization on the following parameters, see what comes up.
#total_cup_points, country_of_origin, species, harvest_year
colnames(coffee_ratings)

#look at top 5 producers
#brazil, Vietnam, Colombia, Indonesia, Ethopia

#plot 1, total rating per country of origin
coffee_ratings %>%
  filter(country_of_origin %in% c("Brazil", "Vietnam", "Colombia", "Indonesia", "Ethiopia")) %>%
ggplot() +

  geom_jitter(aes(y = country_of_origin, x = total_cup_points),
  height = .2, size = 2,
  alpha = .5,
  color = "#daa520")  +
  geom_boxplot(aes(y = country_of_origin, x = total_cup_points),
               alpha = 0,
               outlier.shape = NA,
               coef = 0,
               color = "#AF3E03FF",
               fill = "transparent") +
  geom_point(aes(y = country_of_origin, x = mean(total_cup_points)),
             size = 4)+
  geom_vline(xintercept = mean(coffee_ratings$total_cup_points),
             linetype = 2,
             size = 1.2)+
  geom_curve(x = 82, y = 4.5, xend = 75, yend = 4) +
  geom_curve(x = 83.5, y = 5, xend = 87, yend = 5.1)+
  annotate("text", label = "Mean Rating\n of ALL Producer Countries", x = 75, y = 3.8) +
  annotate("text", label = "Mean Rating\n of Country", x = 87, y = 5.3) +
  geom_segment(aes(x = mean(coffee_ratings$total_cup_points), y = country_of_origin, yend= country_of_origin, xend=  83 )) +
  labs(title = "",
  subtitle = "The 5 largest Coffee Producers Worldwide and their Deviation from the Overall Rating Mean")+
  xlab("Total Cup Points") +
  ylab("Country of Origin")+
  theme_bw() +
  theme(legend.position = "none",
  plot.background = element_rect(fill = "white"),
   panel.background = element_rect(fill = "white", color = "white"),
  panel.border = element_rect(color = "#daa520"),
  panel.grid = element_blank(),
  axis.text  = element_text(color = "#AF3E03FF"))-> p1
p1


#plot2: correlation between some coffe indicators
#make correlation matrix
ratings = coffee_ratings %>%
  select(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness)

cormat = round(cor(ratings), 1)  %>%
  melt() %>%
  data.frame(stringsAsFactors = FALSE)

glimpse(cormat)
cormat$value = as.numeric(cormat$value)

arrange(cormat, desc(value)) -> cormat

color(get_brewer_pal("YlOrBr", n = 5, contrast = c(.3, 1)))


ggplot(cormat) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(Var1, Var2, label = value)
    , size = 4, color = "black") +
  scale_fill_gradient2(low = "#FED676", high = "#AF3E03FF", mid = "white", midpoint = 0)+
  labs(title = "", subtitle = "Pearson Correlation Coefficient of  Coffee Rating Indices") +
  xlab("Coffee Rating Indices") +
  ylab("")+
  theme_bw() +
  theme(legend.position = "none",
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white", color = "white"),
  panel.border = element_rect(color = "#daa520"),
  axis.text = element_text(color = "#AF3E03FF")) +
  rotate_x_text() -> p2
p2

#PATCH IT UP
p1 + p2 +
 plot_annotation(title = 'GLOBAL COFFEE RATINGS - A SMALL DATA STORY\n ', caption = "plot by @toeb18", subtitle = "The countries with the highest coffee production factor also have quite a high rating of their coffee.\nLooking at the coffee rating indices, it becomes clear that some of them seem to have a relationship.\n ",
 theme = theme(plot.title = element_text(size = 16, hjust = .5),
               plot.subtitle = element_text(size = 13, hjust= .5),
               plot.margin = unit(c(1,1,1,1), "cm")))-> plots



plots

ggsave(plots, filename = "plots.png", width =40, height = 20, units = "cm")


