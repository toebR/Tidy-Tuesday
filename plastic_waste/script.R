#Tobias Stalder
#tobias-stalder.netlify.app
#dataviz on plastic waste data by tidytuesday


# libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(ggtext)
library(prismatic)
library(extrafont)
library(Cairo)
loadfonts()
here()


# load data ---------------------------------------------------------------

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')


# data wrangling ----------------------------------------------------------



# select data on hdpe, ldpe, pet, pp, pvc and calculate new subtotal per country.
#only focus on 2019, the total of all companies and the top 10 countries with the highest total count of these polymners.
plastics %>%
  filter(year == 2019,
         parent_company == "Grand Total",
         country != "EMPTY") %>%
  select(country, year, hdpe, ldpe, pet, pp, pvc) %>%
  group_by(country) %>%
  mutate(total = sum(hdpe, ldpe, pet, pp, pvc, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  top_n(6) %>%
  pivot_longer(cols = c(hdpe, ldpe, pet, pp, pvc, total)) %>%
  rename(polymer = name,
         count = value)-> p_clean

#rename nigeria for common nomenclature
gsub("NIGERIA", "Nigeria", p_clean$country) -> p_clean$country

gsub("hdpe", "high-density PE", p_clean$polymer) -> p_clean$polymer
gsub("ldpe", "low-density PE", p_clean$polymer) -> p_clean$polymer
gsub("pet", "PET", p_clean$polymer) -> p_clean$polymer 
gsub("pp", "PP", p_clean$polymer) -> p_clean$polymer
gsub("pvc", "PVC", p_clean$polymer) -> p_clean$polymer

#dataframe only holding totals:


# dataviz -----------------------------------------------------------------
ggplot(p_clean) +

  geom_segment(aes(x = polymer, xend = polymer, y = 0, yend = 55000, color = polymer), size = .75, show.legend = TRUE)+
  geom_point(aes(x = polymer, y = count, group = country, color = polymer, size = count), show.legend = TRUE) +
  geom_text(aes(x = polymer, y = count, label = count), size = 3, color = "white")+

  facet_wrap(.~ country, ncol = 3, scales = "free_x") +

  scale_color_manual(values = c("#4e8b7c", "#325a50", "#624c9c", "#3757d5", "#384e78", "#202b43")) +
  scale_size_continuous(range = c(7, 17))+
  scale_y_continuous(breaks = c(0, 25000, 55000), limits = c(0, 55000))+
  
  xlab("Polymer Type") +
  ylab("Count")+
  
  ggtitle("Let's Get Rid of the Plastic Waste")+
  labs(subtitle = "Selected Plastic Polymers found by breakfreefromplastic in 2019 | The Top 6 Countries",
       caption = "Plot by **Tobias Stalder** | tobias-stalder.netlify.app <br>
       Data from **Break Free from Plastic courtesy of Sarah Sauve** | breakfreefromplastic.org <br>
       #TidyTuesday")+
  
  theme_bw()+
  theme(plot.caption = element_markdown(hjust = 0, lineheight = 1.2),
        plot.title = element_text( size = 25),
    panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#363636", size = .5, linetype = "dashed"),
        strip.background = element_rect(color = "#363636", fill = "#474747"),
        plot.background = element_rect(color = "#363636", fill = "#363636"),
        panel.background = element_rect(color = "transparent", fill = "#474747"),
        panel.border  = element_blank(),
        axis.text.x = element_blank(),
    axis.text.y = element_text(color = "#A8A7A7"),
        text = element_text(color = "#A8A7A7", family = "Times New Roman"),
        strip.text = element_text(color = "#A8A7A7", size = 12),
        legend.position = "bottom",
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.key =element_rect(color = "transparent", fill = "transparent"),
        plot.margin = margin(1, 1, .5, .2, unit = "cm"))+
  
  guides(size = guide_none(),
         color = guide_legend(keyheight = 1, keywidth = 5, title = ""))+
  
  ggsave(filename = paste0(here(),"/test.png"), width = 23, height = 25, units = "cm", type = "cairo-png", dpi = 300)

         