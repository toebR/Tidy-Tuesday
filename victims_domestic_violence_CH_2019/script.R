#by tobias stalder

library(tidyverse)
library(reshape2)
library(readxl)
library(corrplot)
library(ggpubr)

dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\victimology_domestic_violence_CH_2019"
setwd(dir)

#import data
#data source: https://www.bfs.admin.ch/bfs/de/home/statistiken/kriminalitaet-strafrecht/polizei/haeusliche-gewalt.assetdetail.12327156.html
dat_R <- as.data.frame(read_excel("dat_R.xlsx"))

dat_R

#get everything to long format

dat_R_long = melt(data = dat_R, id.vars = c("Age", "gender"))
dat_R_long

#rename columns
colnames(dat_R_long) = c("Age group", "Gender", "Criminal offense", "count")

#visual data mining on age distributions general, and per offense, same for gender and gender+age.

#drop the total category
dat_R_long = dat_R_long %>%
  filter(`Criminal offense` != "Total")

# prepare plot on samples ---------------------------------------------------------------------
dat_R_long %>%
  filter(`Criminal offense` == "sexual assault") %>%
  ggplot() +
  geom_area(aes(x = `Age group`, y = count, color = Gender, group = Gender, fill = Gender),
            show.legend = FALSE,
            alpha = .5) +
  scale_color_manual(values = c("cyan", "orange")) +
  scale_fill_manual(values = c("cyan", "orange")) +
  ylab("Number of Offenses")+
  labs(title = "Criminal Offense")+
  theme_bw()+
  theme(legend.position = c(.9,.9),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
        panel.background = element_rect(fill = "#2B303D", color = "#8A8A8A"),
        text = element_text(color = "lightgrey"),
        axis.text=element_text(color = "lightgrey", size = 6),
        panel.grid.major = element_line(color = "#8A8A8A"),
        panel.grid.minor = element_line(color = "#8A8A8A"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 10, hjust = .5),
        axis.title = element_text(size = 8),
)+
  rotate_x_text() -> test
test
ggsave(test, filename = "test.png", width = 6, height = 6, units = "cm")

# for loop to create plots for each offense -------------------------------
iterator = c("homicide",
             "aggravated assault",
             "assault",
             "adhibiting a hazardous substance to a child",
             "defamation",
             "abuse of a telecommunication system",
             "duress",
             "sexual abuse of a child",
             "rape",
             "sexual harassment",
             "attempted homicide",
             "assault occasioning actual bodily harm",
             "endangering a life",
             "libel",
             "insult",
             "menace",
             "abduction",
             "sexual assault",
             "desecration",
             "other")
for (i in iterator) {
dat_R_long %>%
  filter(`Criminal offense` == i) %>%
  ggplot() +
  geom_area(aes(x = `Age group`, y = count, color = Gender, group = Gender, fill = Gender),
            show.legend = FALSE,
            alpha = .5) +
  scale_color_manual(values = c("cyan", "orange")) +
  scale_fill_manual(values = c("cyan", "orange")) +
  ylab("Number of Offenses")+
  labs(title = paste(i))+
  theme_bw()+
  theme(legend.position = c(.9,.9),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
        panel.background = element_rect(fill = "#2B303D", color = "#8A8A8A"),
        text = element_text(color = "white"),
        axis.text=element_text(color = "white", size = 6),
        panel.grid.major = element_line(color = "#8A8A8A"),
        panel.grid.minor = element_line(color = "#8A8A8A"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 8, hjust = .5),
        axis.title = element_text(size = 8),
  )+
  rotate_x_text() -> test
ggsave(test, filename = paste(i,".png", sep = ""), width = 6, height = 6, units = "cm")
}

