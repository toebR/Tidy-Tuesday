#a dataviz for/from the tate gallery art repository
#by Tobias Stalder, Jan 2021
#╔TidyTuesday

#data source: https://github.com/tategallery/collection



# load libraries ----------------------------------------------------------

library(here)
here()
library(tidyverse)
library(treemapify)
library(extrafont)
library(Cairo)
library(ggtext)


# load data ---------------------------------------------------------------


artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')


# dataviz -----------------------------------------------------------------
#very simple: how many times a artist-title combination pops up in the data.

artwork %>%
  group_by(artist,title) %>%
  count()-> title

#clean titles for unknown titles (we don't want a title named "blank" or unknown in the data)
title %>%
  filter(!title %in% c("[no title]", "[title not known]", "[blank]", "Blank", "Untitled")) -> title_cleaned


# create new color steps with cut() and then scale discrete with a more complex gradient palette...
title_cleaned %>%
  mutate(clr_step = cut(n, breaks = c(-Inf,1,2,5,10,50, 100, 150 ,200,400, 600),
                        labels = c("1","2","3-5", "6-10", "11-50", "51-100", "101-150", "151-200", "201-400", "401-600"))) -> title_cleaned

quantile(title_cleaned$n)

unique(title_cleaned$clr_step)



#plot
title_cleaned  %>%
  # filter(n>=10) %>%
  ggplot() +
  geom_treemap(aes(area = n, fill = clr_step, color = "transparent"), show.legend = TRUE, color = "#2a2d2f", size = 2.5) +
  scale_fill_manual("",values = c("#F72585", "#B5179E", "#7209B7", "#560BAD",
                                                         "#480CA8", "#3A0CA3", "#3F37C9", "#4361EE", "#4895EF", "#4CC9F0"))+
  scale_x_continuous(limits = c(0, 1)) + #create arbitrary scales for labelling
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Artist - Artwork Combinations in the Tate Gallery Collection<br>") +
  labs(subtitle = "Artwork titles corrected for descriptions such as 'Untitled', 'Unknown' etc.*<br><br>",
       caption = "plot by **Tobias Stalder** | tobias-stalder.netlify.app<br>
       Source: **Tate Gallery Collection Repository** | github.com/tategallery/collection<br>
       *Data as of October 2014") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "#2a2d2f", color = "#2a2d2f"),
        plot.title = element_markdown(vjust = 2.5, hjust = .5, size = 40),
        plot.subtitle = element_markdown(vjust = 2.5, hjust = .5, size = 20),
        plot.margin = margin(2,2,2,2, "cm"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 16),
        text = element_text(family = "Times New Roman", color = "#eaeded"),
        plot.caption = element_markdown(size = 20, hjust = 0, lineheight = 1.5),
        legend.background = element_rect(color = "#2a2d2f", fill = "#2a2d2f"),
         legend.spacing.x = unit(1, 'cm'),
        legend.key = element_rect(color = "transparent", fill = "transparent"))+
  guides(fill = guide_legend(nrow = 1,  label.position = "bottom",
                               override.aes = list(color = "transparent", size = 10)),
         alpha = guide_none())-> p1

ggsave(p1, filename = "treemap.png", type = "cairo-png", width = 42, height = 59, units = "cm", dpi = 300)

