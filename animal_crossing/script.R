library(tidyverse)
library(readr)
library(ggpubr)
library(tidytext)
library(ggthemes)
library(gridExtra)
library(ggcharts)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\animal_crossing")
setwd(dir)


#import data from github
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#have a look at the data
criticpl <- data.frame(critic, stringsAsFactors = FALSE)
user_reviewspl <- data.frame(user_reviews, stringsAsFactors = FALSE)
itemspl <- data.frame(items, stringsAsFactors = FALSE)
villagerspl <- data.frame(villagers, stringsAsFactors = FALSE)



#analytical ideas: 
#(1) normalize critic values /10 to land the same scale (0-10) isntead of 100. make comparing plots of the scoring, make distributions
#(2) what words are prominently used in good and bad scores ? analyse in groups below and over median scoring. then analyse most used words.
#(3) with villagers, show most popular race etc. and see if it connects to any other variable. are there connections between, gender, race, song and personality attributes?
# make cool plots for 1, 2 and 3...

##(1)---------------------------------------critics and user reviews for animal crossing--------------------------------------
#resize by order of magnitude (grad of critics)
criticpl$gradnorm <- criticpl$grade / 10
#small plot to test:
a <- ggplot() +
  geom_density(aes(x = criticpl$gradnorm, y = ..count..), fill = "lightblue", color = "white") +
  geom_vline(aes(xintercept = median(criticpl$gradnorm)), 
             linetype = "dashed", size = 0.6,
             color = "black")+ 
  geom_text(aes(x=median(criticpl$gradnorm), label="\nMedian", y=100), colour="black", angle=90) +
  ggtitle("Critics Grading of Animal Crossing") +
  xlab("Critics Review Grade") +
  
  theme_pubr()

b <- ggplot() +
  geom_density(aes(x = user_reviewspl$grad, y = ..count..), fill = "lightgreen", color = "white") +
  geom_vline(aes(xintercept = median(user_reviewspl$grad)), 
             linetype = "dashed", size = 0.6,
             color = "black") +
  geom_text(aes(x=median(user_reviewspl$grad), label="\nMedian", y=350), colour="black", angle=90) +
  ggtitle("User Reviews Grading of Animal Crossing") +
  xlab("User Reviews Grade") +
  theme_pubr()


#-----------------------------------------------set up overview for grading distribution--------------------------------------
summarized <- grid.arrange(a,b)
ggsave(summarized, filename = "Grading Density.png", width = 15, height = 15, units = "cm")

#-----------------------------------------------text analysis----------------------------------------------------


#filter both both, critics and user_reviews below and over median and make text analysis
user_top <- filter(user_reviews, grade >= median(grade))

user_low <- filter(user_reviews, grade < median(grade))



#convert to tibble and get tibble with words
user_top_text <- tibble(user_top, text = user_top$text)
user_top_text_df <- mutate(user_top_text, text = text)
user_top_words <- unnest_tokens(user_top_text_df, word,text)

#get rid of stop words like "of", "thereby" etc.
user_top_words <- user_top_words %>%
  anti_join(stop_words)

user_top_words

#get count
count <- count(user_top_words,word, sort = TRUE)

#most common positive and negative words:

#sentiment analysis
user_top_sentiments <- user_top_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  head(10) #get 10 most used words of people giving over median grading
  
user_top_sentiments



sent_user_top <- ggdotchart(user_top_sentiments, x = "word", y = "n",
            color = "sentiment",                                # Color by groups # Custom color palette
           sorting = "descending",                       # Sort value in descending order                             # Add segments from y = 0 to dots
            rotate = TRUE,                                # Rotate vertically
            group = "sentiment",
           y.text.col = TRUE, # Order by groups
           dot.size = 2.5,
           palette = c("#D2A75C", "#62B7AD"),# Large dot size
           ggtheme = theme_pubr()                       # ggplot2 theme
) + 
  ylim(100, 600) +
  labs(subtitle = paste("User Reviews with > / = Median grading of Animal Crossing\n" ,"n =", nrow(user_top))) +
  theme(legend.position = "right") +
  theme_cleveland()
print(sent_user_top)

# -------------------------------user LOW------------------------------------
 


#convert to tibble and get tibble with words
user_low_text <- tibble(user_low, text = user_low$text)
user_low_text_df <- mutate(user_low_text, text = text)
user_low_words <- unnest_tokens(user_low_text_df, word,text)

#get rid of slow words like "of", "thereby" etc.
user_low_words <- user_low_words %>%
  anti_join(stop_words)

user_low_words

#get count
count <- count(user_low_words,word, sort = TRUE)

#most common positive and negative words:

#sentiment analysis
user_low_sentiments <- user_low_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  head(10) #get 10 most used words of people giving over median grading

user_low_sentiments


sent_user_low <- ggdotchart(user_low_sentiments, x = "word", y = "n",
           color = "sentiment",                                # Color by groups # Custom color palette
           sorting = "descending",                       # Sort value in descending order                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "sentiment",
           y.text.col = TRUE, # Order by groups
           dot.size = 2.5,
           palette = c("#D2A75C", "#62B7AD"),# Large dot size
           ggtheme = theme_pubr()                       # ggplot2 theme
) + 
  ylim(100,600) +
  labs(caption = "@TidyTuesday \nAuthor: @toeb18") +
  labs(subtitle = paste("User Reviews with < Median grading of Animal Crossing\n" ,"n =", nrow(user_low))) +
  theme(legend.position = "right") +
  theme_cleveland()
print(sent_user_low)

# ----------------------------------------------CRITIC TOP--------------------------------

critic_top <- filter(critic, grade >= median(grade))



#convert to tibble and get tibble with words
critic_top_text <- tibble(critic_top, text = critic_top$text)
critic_top_text_df <- mutate(critic_top_text, text = text)
critic_top_words <- unnest_tokens(critic_top_text_df, word,text)

#get rid of slow words like "of", "thereby" etc.
critic_top_words <- critic_top_words %>%
  anti_join(stop_words)

critic_top_words

#get count
count <- count(critic_top_words,word, sort = TRUE)

#most common positive and negative words:

#sentiment analysis
critic_top_sentiments <- critic_top_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  head(10) #get 10 most used words of people giving over median grading

critic_top_sentiments


sent_critic_top <-ggdotchart(critic_top_sentiments, x = "word", y = "n",
           color = "sentiment",                                # Color by groups # Custom color palette
           sorting = "descending",                       # Sort value in descending order                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "sentiment",
           y.text.col = TRUE, # Order by groups
           dot.size = 2.5,
           palette = c("#62B7AD"),# Large dot size
           ggtheme = theme_pubr()                       # ggplot2 theme
) + 
  ylim(1, 16) +
  labs(title = "Sentiment Analysis of top 10 Words used",
       subtitle = paste("Critics Reviews with > / = Median grading of Animal Crossing\n" ,"n =", nrow(critic_top))) +
  theme(legend.position = "right") +
  theme_cleveland()

print(sent_critic_top)
#----------------------------------------------critics low-------------------------------------------
critic_low <- filter(critic, grade < median (grade))

#convert to tibble and get tibble with words
critic_low_text <- tibble(critic_low, text = critic_low$text)
critic_low_text_df <- mutate(critic_low_text, text = text)
critic_low_words <- unnest_tokens(critic_low_text_df, word,text)

#get rid of slow words like "of", "thereby" etc.
critic_low_words <- critic_low_words %>%
  anti_join(stop_words)

critic_low_words

#get count
count <- count(critic_low_words,word, sort = TRUE)

#most common positive and negative words:

#sentiment analysis
critic_low_sentiments <- critic_low_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  head(10) #get 10 most used words of people giving over median grading

critic_low_sentiments


sent_critic_low <-ggdotchart(critic_low_sentiments, x = "word", y = "n",
           color = "sentiment",                                # Color by groups # Custom color palette
           sorting = "descending",                       # Sort value in descending order                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "sentiment",
           y.text.col = TRUE, # Order by groups
           dot.size = 2.5,
           palette = c("#62B7AD"),# Large dot size
           ggtheme = theme_pubr()                       # ggplot2 theme
) + 
  ylim(1, 16) +
  labs(subtitle = paste("Critics Reviews with < Median grading of Animal Crossing\n" ,"n =", nrow(critic_low))) +
  theme(legend.position = "right") +
  theme_cleveland()

print(sent_critic_low)

#-------------------------------------------Setting up overview for sentiment analytics---------------------------------------
sentimentanalysis <- grid.arrange(sent_critic_top, sent_user_top,sent_critic_low, sent_user_low)
ggsave(sentimentanalysis, filename = "sentiment_analysis.png", width = 30, height = 20, units = "cm")


#----------------------------factor analysis of users-------------------------
villagerspl
colnames(villagerspl)

counts <- data.frame(count(villagerspl, species, gender))
counts




