#by tobias stalder
#tidytuesday on bechdel test data, March 2021
#tobias-stalder.netlify.app


# load libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(tidytuesdayR)
library(Cairo)
library(ggtext)
library(extrafont)
loadfonts()

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies



# clean data to purpose ---------------------------------------------------

movies %>%
  select(year, binary, budget, imdb_rating, genre)%>%  #select important cols
  #get all entries with available imdb score and genre description
  drop_na(genre, imdb_rating, binary) -> movies_clean

#calculate min_max per year
movies_clean %>%
  group_by(year, binary) %>%
  summarise(min = min(imdb_rating),
            max = max(imdb_rating),
            median = median(imdb_rating)) -> min_max

# plot: viz about imdb rating and bechdel test results (binary) --------


ggplot(movies_clean, aes(x = year, y = imdb_rating, fill = binary, color = binary))+
  # geom_boxplot(aes(group = year), outlier.colour = "transparent", alpha = .4, color = "darkgrey") +
  geom_jitter(alpha = 0.15, size = 1, width = .15, show.legend = FALSE) +
  geom_segment(data = min_max, aes(x = year, y = min, xend = year, yend= max), color = "gray", size = 0.5, show.legend = FALSE)+
  geom_point(data =min_max, aes( x = year, y = min, color = binary), size = 2, color = "gray", show.legend = FALSE)+
  geom_point(data =min_max, aes( x = year, y = max, color = binary), size = 2, color = "gray", show.legend = FALSE)+
 
  ggbump::geom_bump(data = min_max, aes(x = year, y = median, color = binary), show.legend = FALSE)+

  scale_color_manual(values = c("#F26B38","#2F9599")) +
  facet_wrap(.~binary, ncol = 1) +
  
  ylab("IMDB Rating") +
  xlab("")+
  
  labs(title = "Gender Bias in Movies Over Time",
       subtitle = "Median and Minimum/Maximum IMDB Ratings for Movies with <span style = 'color: #2F9599;'>**Passed**</span> and <span style = 'color: #F26B38;'>**Failed**</span> Test Results",
       caption = "Plot by **Tobias Stalder** | tobias-stalder.netlify.app<br>Data from **fivethirtyeight.com**<br>**#TidyTuesday**") +
  
  theme_minimal() +
  theme(plot.subtitle = element_markdown(),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
      text = element_text(family = "Segoe UI"),
      panel.grid.minor = element_blank(),
      strip.text = element_blank())+
  ggsave(filename = paste0(here(),"/test.png"), width = 22, height = 15, units = "cm",
         type = "cairo-png", dpi = 600)



