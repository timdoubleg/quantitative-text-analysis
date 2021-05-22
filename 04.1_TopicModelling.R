# Quantitative Text Analysis 2021 
# Group paper 
#                                 
# by                              
# Ella Stanisch                   
# David Klug                       
# Swen Hartlieb                   
# Tim Graf                        


# 0 Setup ----
#===================#

library(quanteda)
library("quanteda.textplots")
library(data.table)
library(tidytext) #text mining, unnesting
library(topicmodels) #the LDA algorithm
library(tidyr) #gather()
library(dplyr) #awesome tools
library(ggplot2) #visualization
library(kableExtra) #create attractive tables
library(knitr) #simple table generator
library(ggrepel) #text and label geoms for ggplot2
library(gridExtra)
library(formattable) #color tile and color bar in `kables`
library(tm) #text mining
library(circlize) #already loaded, but just being comprehensive
library(plotly) #interactive ggplot graphs
library(stm)

rm(list=ls())

# set wd to where file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_withcountry.csv')


# 1 ... ----
#===================#


#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}

# three_sources_tidy_balanced <- fread('./data/three_sources_tidy_balanced.csv')
# 
# three_sources_tidy_balanced %>%
#   group_by(source) %>%
#   mutate(word_count = n()) %>%
#   select(source, genre, word_count) %>% #only need these fields
#   distinct() %>%
#   ungroup() %>%
#   #assign color bar for word_count that varies according to size
#   #create static color for source and genre
#   mutate(word_count = color_bar("lightpink")(word_count),  
#          source = color_tile("lightblue","lightblue")(source),
#          genre = color_tile("lightgreen","lightgreen")(genre)) %>%
#   my_kable_styling("Three Sources Stats")

# Data Cleaning ----
#===================#

# convert everything to lower case
data$text <- sapply(data$text, tolower)

# remove special characters
removeSpecialChars <- function(x) gsub("[^a-z ]", " ", x)
data$text <- sapply(data$text, removeSpecialChars)

# combine title and text 
data$text <- with(data, paste0(title, text))

# examine first 300 characters of text
str(data[1, ]$text, nchar.max = 300)
str(data)

# manual exclusion of words
presidents <- tolower(unique(data$president))
presidents <- unlist(strsplit(presidents, " "))
undesirable_words <- c('about', 'search', 'president', 'united', 'states', 'of', 'america', 'american', 
                       'executive', 'order', 'presidency', 'secretary', 'section', 'act')
undesirable_words <- append(undesirable_words, presidents)

# tidy up data to word dataframe
eo.words <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% undesirable_words) %>% 
  select(-c(document_type, title, iso))

nrow(eo.words) # we have over 1500k non-unique words!


# Simple Analysis  ----
#===================#

#create the decade column
data <- data %>%
  mutate(decade = 
           ifelse(data$year %in% 1950:1959, "1950s",
                  ifelse(data$year %in% 1960:1969, "1960s",
                         ifelse(data$year %in% 1970:1979, "1970s", 
                                ifelse(data$year %in% 1980:1989, "1980s", 
                                       ifelse(data$year %in% 1990:1999, "1990s", 
                                              ifelse(data$year %in% 2000:2009, "2000s", 
                                                     ifelse(data$year %in% 2010:2019, "2010s", 
                                                            ifelse(data$year %in% 2020:2021, "2020s", 
                                                                   "NA")))))))))

# Plot amount of EOs over the decades
plot.decades <- data %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_eos = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_eos), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("EOs Counts per decade") +
  labs(x = NULL, y = "EO count")
plot.decades

# count words
full_word_count <- data %>%
  unnest_tokens(word, text) %>% # split a column in to tokens
  group_by(eo_number) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))
full_word_count$perc_words = full_word_count$num_words/sum(full_word_count$num_words)
full_word_count <- left_join(full_word_count, data, on = 'eo_number') %>% 
  select(-c(text, date, document_type, iso, decade))


# plot EOs with top word counts
plot.topwords <- full_word_count[1:10,] %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(eo_number = color_tile("lightpink","lightpink")(eo_number)) %>%
  kable("html", escape = FALSE, align = "c", caption = "EOs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)
plot.topwords

# plot the distribution of word counts
# we can see that it is heavily skewed
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words)) +
  ylab("EO Count") + 
  xlab("Word Count per EO") +
  ggtitle("EOs Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# let's manually look at the content of the EOs with highest word count
top1 <- full_word_count$eo_number[1]
top2 <- full_word_count$eo_number[2]
top3 <- full_word_count$eo_number[3]

str(data[data$eo_number==top1, 'text'], nchar.max = 1000)
str(data[data$eo_number==top2, 'text'], nchar.max = 1000)
str(data[data$eo_number==top3, 'text'], nchar.max = 1000)


# histogram with filtered
cut_off <- 5000
plot.wordcount.hist <- full_word_count %>%
    filter(num_words<cut_off) %>%
    ggplot() +
    geom_histogram(aes(x = num_words)) +
    ylab("EO Count") + 
    xlab("Word Count per EO") +
    ggtitle("EOs Word Count Distribution") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.minor.y = element_blank())
plot.wordcount.hist

# count top n words
# this shows simply that we need to work with a TF-IDF to get the rare words
eo.words %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("EO Count") +
  ggtitle("Most Frequently Used Words in EOs (1950 - 2021)") +
  coord_flip()

# TF-IDF, inverse document frequency
tfidf.decade <- eo.words %>%
  left_join(data[, c('country', 'decade', 'eo_number')], on = 'eo_number') %>% 
  count(decade, word, sort = TRUE) %>%
  bind_tf_idf(word, decade, n) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(decade) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())

tfidf.eonumber <- eo.words %>%
  left_join(data[, c('country', 'decade', 'eo_number')], on = 'eo_number') %>% 
  count(eo_number, word, sort = TRUE) %>%
  bind_tf_idf(word, eo_number, n) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(eo_number) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>%
  arrange(eo_number, tf_idf) %>%
  mutate(row = row_number())

tfidf.onlychina <- eo.words %>%
  left_join(data[, c('country', 'decade', 'eo_number')], on = 'eo_number') %>% 
  filter(country == 'China') %>%
  count(decade, word, sort = TRUE) %>%
  bind_tf_idf(word, decade, n) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(decade) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())


plot <- function(df, title) {
  ggplot(df, aes(x = row, y=tf_idf, fill = decade)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  labs(title = 'Important Words using TF-IDF', 
       subtitle = title) +
  theme_lyrics() +  
  facet_wrap(~decade, 
             ncol = 3, nrow = 3, 
             scales = "free") +
  scale_x_continuous(  # this handles replacement of row 
    breaks = df$row, # notice need to reuse data frame
    labels = df$word) +
  theme(plot.subtitle=element_text(size=9, hjust=0.5, face="italic", color="black")) +
  coord_flip()
}

# plot the different types
plot(tfidf.decade, '(Grouped by decades)')
plot(tfidf.onlychina, '(Grouped by China)')





