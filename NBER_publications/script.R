
#dataviz on tidytuesday on NBER publication data
#september/october 2021
#tobias stalder



# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(here)
library(extrafont)
loadfonts(device = "win")

# load data ---------------------------------------------------------------

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')



# data wrangling on papers and programs -----------------------------------
left_join(papers, paper_programs, by = "paper") %>%
  drop_na() %>% #drop papers with no program specification
  left_join(programs, by = "program") %>%
  group_by(year, program_desc) %>%
  count() %>%
  filter(year != 2021)-> data

data_2020 <- data %>%
  filter(year == 2020) %>%
  mutate(program_desc = factor(program_desc))


data_2020$program_desc = fct_reorder(.f = data_2020$program_desc, data_2020$n, .desc = TRUE)
levels(data_2020$program_desc) -> levels_fct

data$program_desc <- factor(data$program_desc, levels = levels_fct)


#make dot plot that scales with size for each subcategory

data %>%
  ggplot(aes(x = year, y = program_desc))+
  geom_point(aes(alpha = n, color = n, size = n), shape = 15) +
  scale_alpha_continuous("Nr. of Publications", range = c(0.3,1), breaks = c(50, 100, 200, 300, 400), labels = c("1-50", "51-100", "101-200", "201-300", "301-450"))+
  scale_size_continuous("Nr. of Publications", range = c(2, 6), breaks = c(50, 100, 200, 300, 400), labels = c("1-50", "51-100", "101-200", "201-300", "301-450"))+
  scale_color_continuous("Nr. of Publications", low = "#2471a3", high = "black", breaks = c(50, 100,200,300,400), labels = c("1-50", "51-100", "101-200", "201-300", "301-450"))+
  ylab("")+
  xlab("Publication Year")+
  labs(title = "National Bureau of Economic Research",
       subtitle = "Publication History in Different Programs from 1970-2020\n",
       caption = "By Tobias Stalder\n#TidyTuesday\nData: {nberwp} by Bern Davis")+
  guides(color= guide_legend(), size=guide_legend(), alpha = guide_legend())+
  facet_wrap(.~program_desc, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = "bottom",
        text = element_text(family = "Gill Sans MT"),
        plot.title = element_text(size = 25, color = "#5499c7"),
        plot.subtitle = element_text(size = 18)) -> plot



plot

ggsave(plot, filename = paste0(here(), "/NBER_publications.png"), dpi = 300, width = 20, height = 30, units = "cm")

