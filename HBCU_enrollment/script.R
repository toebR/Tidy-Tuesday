#script by tobias stalder
#tidytuesday
#dataviz on HBCU enrollment

options(scipen = 999)

# load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(extrafont)
loadfonts(device = "win")
library(here)
here()

# load data ---------------------------------------------------------------
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')



# data cleaning -----------------------------------------------------------
# get data per year: female and male total enrollment, public vs. private school 4-year enrollments from 1990 onwards
hbcu_all %>%
  filter(Year >= 1990) %>%
  select(Year, Males, Females)%>%
  mutate(diff = Females - Males) %>%
  pivot_longer(cols = c(Males, Females)) %>%
  rename(Gender = name,
         Enrollments = value)-> dat_gender


# create dataframes for individual layer plotting -------------------------

dat_gender %>%
  group_by(Gender) %>%
  summarise(mean = mean(Enrollments),
            SE = sd(Enrollments)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats

stats_males <- stats %>%
  filter(Gender == "Males")

stats_females <- stats %>%
  filter(Gender == "Females")
  
Males <- dat_gender %>%
  filter(Gender == "Males")

Females <- dat_gender %>%
  filter(Gender == "Females")

diff <- dat_gender %>%
  filter(Gender == "Males") %>%
  mutate(x_pos = Enrollments + (diff/2))


# cool lollipop chart and male vs. female enrollment through time! and add male and female median enrollment through that time with 1 SE!
ggplot(dat_gender)+
  
  geom_rect(xmin = stats_males$meanneg, xmax = stats_males$meanpos,
            ymin = 2016, ymax = 1989, fill = "#762a83", alpha = .05)+
  geom_vline(xintercept = stats_males$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
  
  geom_rect(xmin = stats_females$meanneg, xmax = stats_females$meanpos,
            ymin = 2016, ymax = 1989, fill = "#009688", alpha = .05)+  
  geom_vline(xintercept = stats_females$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
  
  
  geom_segment(data = Males, aes(x = Enrollments, y = Year, yend = Females$Year, , xend = Females$Enrollments),
               color = "#aeb6bf", size = 4.5, alpha = .5) +
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("#009688","#762a83"))+


  

  geom_text(data = diff, aes(label = paste("∆",diff), x = x_pos, y = Year), fill = "white", color = "#4a4e4d", size = 2.5, family = "Segoe UI") +
  
  geom_text(x = stats_females$mean - 1500, y = 1990, label = "MEAN", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  geom_text(x = stats_females$meanpos -1500, y = 1990, label = "STDEV", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  scale_size_continuous(range = c(2, 8), breaks = c(2,4,6,7,8),)+
  xlab("Enrollments")+
  
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")+
  labs(subtitle = "<span style = 'color: #762a83;'>**Male**</span> and <span style = 'color: #009688;'>**Female**</span> Enrollment from 1990 to 2015<br>",
       caption = "Plot by **Tobias Stalder** | tobias-stalder.netlify.app<br>Data from **data.world** | data.world/nces | originally by NCES<br>**#TidyTuesday**")+
  
  
  
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 14, hjust = -.133),
        plot.title = element_text(size = 16, hjust = -.8)) +
  ggsave(filename = "test.png", width = 20, height = 29, units = "cm")
  

