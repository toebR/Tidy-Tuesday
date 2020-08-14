library(tidyverse)
library(ggpubr)
library(stringr)
library(ggpubr)
library(ggbump)
library(prismatic)
library(extrafont)
library(cowplot)
library(png)
library(patchwork)


loadfonts(device = "win")
memory.limit(size = 10000)

setwd(r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\R\tidytuesday\avatar)")

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

# view(avatar)

#calcualte the counts of appeareance for some characters for each chapter to make an overviewplot with the rating
avatar %>%
  group_by(chapter) %>%
  select(-c(full_text, character_words, writer, director)) %>%
  filter(character %in% c("Zuko", "Katara", "Aang", "Toph")) %>%
  group_by(book,chapter_num, character) %>%
  mutate(count = n()) %>%
  distinct(book_num, chapter, chapter_num, character, imdb_rating, count, .keep_all = TRUE) ->avatar_counts




# plot 1: relation imdb rating per chapter and book. --------
ggplot(avatar_counts, aes(x = id, y = imdb_rating, color = book)) +
  geom_bump(show.legend = FALSE)  +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  annotate(geom = "rect", xmin = 0, xmax =4620, ymin = 7, ymax = 10, fill = "blue", alpha = .1) +
  annotate(geom = "rect", xmin = 4622, xmax =9024, ymin = 7, ymax = 10, fill = "#C95A00", alpha = .1) +
  annotate(geom = "rect", xmin = 9025, xmax =13364, ymin = 7, ymax = 10, fill = "#FF704C", alpha = .2) +
  annotate(geom = "text", x = 2300, y = 9.8, label = "WATER", color = "blue", family = "Rage Italic", size = 6,
           alpha = .5)+
  annotate(geom = "text", x = 6800, y = 9.8, label = "EARTH", color = "#C95A00", family = "Rage Italic", size = 6,
           alpha = .5)+
  annotate(geom = "text", x = 11250, y = 9.8, label = "FIRE", color = "red", family = "Rage Italic", size = 6,
           alpha = .5)+
  scale_color_manual(values = c("#944F00", "#FF4326", "#4A7AFF"))+
  ylab("IMDB Rating Score of each Episode")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text (family = "Consolas", size =8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family = "Consolas")) -> P1
P1
ggsave(P1, filename = "imdb_score.jpg", width = 30, height = 10, units = "cm", dpi=300)




# #plot3, show occurences of ALL characters on a timeline (avatar  --------


avatar_counts %>%
  group_by(book) %>%
  summarise(idmin = min(id), idmax = max(id)) -> books_id_tresh

P3 = ggplot() +
  annotate(geom = "rect", xmin = 0, xmax =4620, ymin = 0, ymax = 85, fill = "blue", alpha = .1) +
  annotate(geom = "rect", xmin = 4622, xmax =9024, ymin = 0, ymax = 85, fill = "#C95A00", alpha = .1) +
  annotate(geom = "rect", xmin = 9025, xmax =13364, ymin = 0, ymax = 85, fill = "#FF704C", alpha = .2) +
  annotate(geom = "text", x = 2300, y = 78, label = "WATER", color = "blue", family = "Rage Italic", size = 6,
           alpha = .5)+
  annotate(geom = "text", x = 6800, y = 78, label = "EARTH", color = "#C95A00", family = "Rage Italic", size = 6,
           alpha = .5)+
  annotate(geom = "text", x = 11250, y = 78, label = "FIRE", color = "red", family = "Rage Italic", size = 6,
           alpha = .5)+
  annotate(geom = "text", x = 3600, y = 66, label = "Aang", color = "#00C2AB", family = "Rage Italic",
           size = 5)+
  annotate(geom = "text", x = 2000, y = 58, label = "Katara", color = "#4A7AFF", family = "Rage Italic",
           size = 5)+
  annotate(geom = "text", x = 10250, y = 62.5, label = "Toph", color = "#944F00", family = "Rage Italic",
           size = 5)+
  annotate(geom = "text", x = 12750, y = 41.5, label = "Zuko", color = "#FF4326", family = "Rage Italic",
           size = 5)+
  geom_bump(data = avatar_counts, aes(x = id, y  = count, color = character), show.legend = FALSE)+
  ylab("Character Speech Occurence per Episode")+
  rotate_x_text() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("#00C2AB", "#4A7AFF", "#944F00", "#FF4326")) +
  # facet_wrap(.~book) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text (family = "Consolas", size = 8),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family = "Consolas"))
P3
# ADD GEOM CURVE AND ANNOTATIONS FOR THE CHARACTER NAMES
ggsave(P3, filename = "overview.jpg", width = 30, height = 10, units = "cm", dpi=300)



# try to arrange with cowplot ---------------------------------------------
Kat <- magick::image_read("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\avatar\\Aang2.jpg")

ggplot() +
  background_image(Kat) +
  coord_fixed()-> img

P3 / (P1+ img) +
  plot_annotation(title = 'AVATAR - The Last Airbender ', caption = "plot by @toeb18",
                  subtitle = "A Small Inforgraphic on Character Speech Occurence and IMDB Ratings per Episode",
                  theme = theme(plot.title = element_text(size = 30, hjust = .5, family = "Rage Italic"),
                                plot.subtitle = element_text(size = 20, hjust= .5, family = "Rage Italic"),
                                plot.margin = unit(c(1,1,1,1), "cm"),
                                plot.caption = element_text(family = "Consolas")))-> Patch

ggsave(Patch, filename = "avatar.png", width = 40, height = 20, units = "cm")





