library(tidyverse)
library(ggpubr)
library(reshape2)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\volleyball")
setwd(dir)

#load data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

vb_matches <- data.frame(vb_matches)
summary(vb_matches)
head(vb_matches)
colnames(vb_matches)

vb_matches %>%
  select(gender, w_rank, l_rank) -> sel

colnames(sel) <- c("gender", "winner", "loser")

vb_long <- melt(sel, id.vars = 'gender')
colnames(vb_long) <- c("gender", "result", "rank")
vb_long
nrow(vb_long)

vb_long$rank <- as.numeric(vb_long$rank)

#plot on rank distribution!!

test <- ggplot(vb_long) +
  geom_density(aes(rank, after_stat(count), color = result, fill = result),
               alpha = 0.3, size = 1) +
  scale_x_continuous(limits = c(1,50), breaks = c(1,10,20,30,40,50)) +
  scale_y_continuous(limits = c(0, 4000), breaks = c(seq(from = 0, to = 4000, by = 1000))) +
  scale_fill_manual(values = c("cyan", "purple")) +
  scale_color_manual(values = c("cyan", "purple")) +
  labs(title = "Team Rank Distribution of Losers and Winners", subtitle = "",
       caption = "plot by @toeb18") +
  theme(text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_line(color = '#4d5566'),
        panel.grid.major = element_line(color = '#586174'),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(color = "#969696"))

ggsave(test, filename = "rank_dist1.png", width = 15, height = 15, units = "cm")

