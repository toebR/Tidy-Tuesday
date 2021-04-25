#tidy tuesday week 17
#by Tobias Stalder
#tobias-stalder@netlify.app
#data on netflix titles


# libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(lubridate)
library(extrafont)
loadfonts(device = "win")
library(here)


# TO DO: ------------------------------------------------------------------

# add caption with authot, data etc
# create R project for outputs
# clean and comment code
# make viz more attractive in terms of design, fonts etc.

# import data -------------------------------------------------------------

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')



# data wrangling ----------------------------------------------------------

#parse date added to a datetime format

input <- netflix_titles$date_added

toDate <- function(input) {
  parse_number(input) -> days
  word(input) -> months
  as.numeric(match(months, month.name)) -> months_num
  str_extract_all(input, "\\d{4}") -> year1
  
  date_added_new <- as.Date(
    paste0(year1, "-", months_num, "-", days)
  )
  
}

dates_as_date <- toDate(netflix_titles$date_added)

netflix_titles %>%
  mutate(added_date = dates_as_date,
         added_year = year(added_date)) %>%
  select(release_year, show_id, type, added_year, country) %>%
  filter(added_year != 2021,
         release_year != 2021) -> data


#construct a theme
theme_netflix <- theme(plot.background = element_rect(fill = "black", color = "black"),
                       panel.background = element_rect(fill = "black", color = "black"),
                       axis.line= element_line(color = "gray30", linetype = "dashed", size = 1),
                       axis.ticks = element_blank(),
                       axis.text.x = element_text(color = "gray30"),
                       panel.grid = element_blank(),
                       plot.title = element_text(color = "red", hjust = 0.5, size = 16))


P_movies <- data %>%
  filter(type == "Movie") %>%
  ggplot(aes(x = added_year, y = release_year))+
  geom_jitter(show.legend = FALSE,
              alpha = 0.3,
              width = 0.3,
              color = "red")+
  scale_y_continuous(limits = c(1940, 2021), position = "left",
                     breaks = c(1940, 2020)) +
  scale_x_continuous(breaks = c(2008, 2020))+
  ggtitle("Movies")+
  xlab("Year Added on Netflix")+
  ylab("Year Released")+
  theme_netflix +
  #remove y axis since we dont need it on this plot
  theme(axis.title = element_text(color = "gray30"),
        axis.text.y = element_text(color = "gray30"),
        axis.line.y.left = element_line(color = "gray30"),
        axis.title.x = element_text(color = "gray30")
)

P_movies


P_shows <- data %>%
  filter(type == "TV Show") %>%
  ggplot(aes(x = added_year, y = release_year))+
  geom_jitter(show.legend = FALSE,
              alpha = 0.3,
              width = 0.3,
              color = "red")+
  scale_y_continuous(limits = c(1940, 2021), position = "right",
                     breaks = c(1940, 2020)) +
  scale_x_continuous(breaks = c(2008, 2020))+
  ggtitle("TV Shows")+
  xlab("")+
  ylab("")+
  theme_netflix +
  theme(axis.title.y = element_text(color = "gray30"),
        axis.title.x = element_text(color = "gray30"),
        axis.text.y = element_text(color = "gray30"))

P_shows

P_movies + P_shows +
  plot_annotation(title = 'NETFLIX',
                  subtitle = "Content original release years vs. year added to Netflix\n",
                  caption = "plot by Tobias Stalder | tobias-stalder@netlify.app\nData: Shivam Bansal | collected from Flixable\n#TidyTuesday",
                  theme = theme(plot.title = element_text(size = 30, hjust = .5, color = "red"),
                                                 plot.subtitle = element_text(size = 16, hjust= .5, color = "gray30"),
                                                plot.caption = element_text(size = 8, color = "gray30", hjust = -0, lineheight = 1.05),
                                                 plot.margin = unit(c(1,1,1,1), "cm"),
                                                 plot.background = element_rect(color = "black", fill = "black"))) -> viz

viz

ggsave(viz, path = paste0(here()), filename = "Netflix.png", width = 20, height = 15, units = "cm")
