#script by tobias stalder
#Dec 2020
#dataviz on big mac index data by tidytuesday


# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(here)
library(lubridate)
library(Cairo)
library(ggtext)
library(extrafont)

# set paths ---------------------------------------------------------------
here()


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-12-22')

bigmac <- tuesdata$`big-mac`


# data processing ---------------------------------------------------------
#I will try to make a simple viz the price of a big mac in dollars and the raw index, relative
#to US dollar.

data <- bigmac %>%
  select(date, name, dollar_price, usd_raw) %>%
  mutate(year_month = format(date, "%Y %m"),
       year = year(date)) %>%
  group_by(year, name) %>%
  summarise(mean_dollar_price = mean(dollar_price), #assume an arithmetic mean by year
            mean_usd_raw = mean(usd_raw))

CH <- data %>%
  filter(name == "Switzerland")

US <- data %>%
  filter(name == "United States")

# data vizualisation ------------------------------------------------------



ggplot(data) +

  geom_line(aes(x = year, y = mean_usd_raw, group = name),
            show.legend = FALSE,
            color = "darkgrey", size = .5, alpha = .7)+
  geom_line(data = CH, aes(x = year, y = mean_usd_raw),
            color = "darkred", size = 1.2) +
  geom_point(data = CH, aes(x = year, y = mean_usd_raw),
            color = "darkred") +
  geom_line(data = US, aes(x = year, y = mean_usd_raw),
            color = "darkblue", size = 1.2) +
  geom_smooth(aes(x = year, y = mean_usd_raw),
              method = "lm",
              linetype = "dashed",
              color = "black",
              se = FALSE,
              size = .5)  +
  geom_smooth(data = CH, aes( x = year, y = mean_usd_raw),
              method = "lm",
              color = "red",
              linetype = "dashed",
              se = FALSE,
              size = .5) +
  
  annotate(geom = "label", label = "Reference Line US$",
           x = 2001, y = 0, color = "darkblue", size = 3)+
  
  annotate(geom = "label", label = "Linear Regression:\nAll Countries",
           x = 2001, y = -0.13, color = "black", size = 3)+
  
  annotate(geom = "label", label = "Linear Regression:\nSwitzerland",
           x = 2001, y = .68, color = "red", size = 3)+
  
  ylab("Big Mac Index\n(relative to US$)")+
  xlab("")+
  
  scale_x_continuous(expand = c(0,.3))+
  
  labs(title = "<span style = 'font-size:30pt', 'line-height=30pt'> The Big Mac Index </span><br>
       <span style = 'font-size:18pt'><br>Putting <span style = 'color:darkred'>Switzerland</span> in Context of Purchasing Power Parity</span>",
       subtitle = "<br>Switzerland is one of the richest countries in the world.
       The Big Mac index shows how under- or overvalued a currency is relative to another.<br>
       In this data vizusalisation, we see that the Swiss Franc is overvalued relative to the <span style = 'color:darkblue'>U.S dollar</span>.
       A  <span style = 'color:red'>linear regression </span>of Switzerlands data shows <br>
       a weak trend towards the baseline over time. However, <span style = 'color:black'>the linear regression </span>over the data of all available countries suggests that there is a<br>
       general trend of falling in the index between 2000 and 2020 (relative to the US$).",
       caption = "**Plot by** Tobias Stalder | tobias.stalder.netlify.app<br>
       **Source:**The Economist<br>
              **#TidyTuesday**")+
  
  theme_bw()+
  theme(panel.border =element_blank(),
        axis.line.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "darkgray"),
        axis.ticks.y = element_blank(),
        plot.title = element_markdown(family = "Bell MT"),
        plot.subtitle = element_markdown(family = "Verdana", color = "gray6", lineheight = 1.2),
        plot.caption = element_markdown(family = "Verdana", hjust = 0))+
  
  
  ggsave(filename = "dollar_raw_index.png", type = "cairo-png", dpi = 300,
         units = "cm", width = 40, height = 20)




