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
library(RColorBrewer)


rm(list=ls())

# set wd to where file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_withcountry.csv')


# 1 Functions ----
#===================#


#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_custom <- function(aticks = element_blank(),
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
undesirable_words <- c('President', 'United', 'States', 'of', 'America', 'American', 
                       'Executive', 'Order', 'order', 'Presidency', 'secretary', 'Section', 'section', 'Act', 'sec.', 
                       'Federal register')
presidents <- (unique(data$president))
presidents <- unlist(strsplit(presidents, " "))
alphabet <- c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)', '(g)', '(h)', '(i)', '(g)', '(h)', '(i)', '(j)', '(k)', '(l)' , '(m)', '(n)', '(o)', '(p)', '(q)', '(r)', '(s)', '(t)', '(u)', '(v)', '(w)', '(x)', '(y)', '(z)')
romanNumber <- c('(i)', '(ii)', '(iii)', '(iv)', '(v)', '(vi)', '(vii)', '(viii)', '(ix)', '(x)')
month <- c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
USA <- c("United","America","American","Americans","Washington")
start <- (c('About Search', 'By the authority',  'vested in me as President by the Constitution', 'the laws of the United States of America', 'it is hereby ordered as follows', 'by virtue of the authority vested in me'))
end <-  (c('The American Presidency Project', 'The American Presidency ProjectJohn Woolley and Gerhard PetersContact, Twitter Facebook, Copyright', 'The American Presidency ProjectTerms of Service'))
undesirable_words <- tolower(append(undesirable_words, c(presidents, alphabet, romanNumber, month, day, USA, start, end))) 


# tidy up data to word dataframe
eo.words <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% undesirable_words) %>% 
  select(-c(document_type, title))

nrow(eo.words) # we have over 1500k non-unique words!


# Simple Analysis  ----
#===================#

data$year <- year(data$date)

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
  group_by(decade) %>%
  summarise(number_of_eos = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_eos), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = 'EO Count per decade', 
       y = 'EO counts',
       x = '',
       subtitle = paste0('n = ', nrow(data))) +
  theme_bw()
plot.decades

# set color palette
colourCount = length(unique(data$president))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# plot EOs per president
data$president <- factor(data$president, levels = c("Harry S. Truman", "Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", 
                                                  'Richard Nixon', 'Gerald R. Ford', 'Jimmy Carter', 'Ronald Reagan', 'George Bush', 
                                                  'William J. Clinton', 'George W. Bush', 'Barack Obama', 'Donald J. Trump', 'Joseph R. Biden'))
plot.president <- data %>%
  group_by(year, president) %>%
  summarise(number_of_eos = n()) %>%
  ggplot() + 
  geom_bar(aes(x = year, y = number_of_eos, fill = president), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = 'EOs per President (1950 - 2021)', 
       y = 'EO count',
       x = 'Year',
       subtitle = paste0('n = ', nrow(data))) +
  theme_bw() + 
  scale_fill_manual(values = getPalette(colourCount))
plot.president


# count words
full_word_count <- data %>%
  unnest_tokens(word, text) %>% # split a column in to tokens
  group_by(eo_number) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))
full_word_count$perc_words = full_word_count$num_words/sum(full_word_count$num_words)
full_word_count <- left_join(full_word_count, data, on = 'eo_number') %>% 
  select(-c(text, date, document_type, decade))


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
plot.freqwords <- eo.words %>%
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
plot.freqwords

# TF-IDF, inverse document frequency
tfidf.decade <- eo.words %>%
  left_join(data[, c('decade', 'eo_number')], on = 'eo_number') %>% 
  count(decade, word, sort = TRUE) %>%
  bind_tf_idf(word, decade, n) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(decade) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())

tfidf.eonumber <- eo.words %>%
  left_join(data[, c('decade', 'eo_number')], on = 'eo_number') %>% 
  count(eo_number, word, sort = TRUE) %>%
  bind_tf_idf(word, eo_number, n) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(eo_number) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>%
  arrange(eo_number, tf_idf) %>%
  mutate(row = row_number())

plot <- function(df, title) {
  ggplot(df, aes(x = row, y=tf_idf, fill = decade)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  labs(title = 'Important Words using TF-IDF', 
       subtitle = title) +
  theme_custom() +  
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
plot.tfidf <- plot(tfidf.decade, '(Grouped by decades)')
plot.tfidf

# Save  ----
#===================#
dir.create('./plots')

ggsave('plot.decades.png', path = './plots/', plot = plot.decades, device = 'png')
ggsave('plot.tfidf.png', path = './plots/', plot = plot.tfidf, device = 'png')
ggsave('plot.president.png', path = './plots/', plot = plot.president, device = 'png', width = 7.5, height = 5)




