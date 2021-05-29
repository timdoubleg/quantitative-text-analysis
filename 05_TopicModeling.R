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
library(here)
library(readtext)
library(stringr)
library(newsmap)
library(sentimentr)
library(tidyr)
library(maps)
library(countrycode)

rm(list=ls())

# set wd to where file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_cleaned.csv')


# Topic Modeling wtih Lexicoder ----
#===================#

# # download dictionary 
# download.file('http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip', 
#               destfile = './data/policy_agendas_english.zip')

# set up dictionary
lexicoder <- dictionary(file = './data/LTDjun2013/policy_agendas_english.lcd')
lengths(lexicoder)

eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')

# remove our own words
undesirable_words <- c('president', 'united', 'states', 'of', 'america', 'american', 
                       'executive', 'order', 'presidency', 'secretary', 'section', 'act')
presidents <- tolower(unique(data$president))
presidents <- unlist(strsplit(presidents, " "))
alphabet <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'g', 'h', 'i', 'j', 'k' , 'l' , 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
lists <- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')
month <- c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
USA <- c("United","America","American","Americans","Washington")
start <- tolower(c('About Search', 'By the authority',  'vested in me as President by the Constitution', 'the laws of the United States of America', 'it is hereby ordered as follows', 'by virtue of the authority vested in me'))
end <-  tolower(c('The American Presidency Project', 'The American Presidency ProjectJohn Woolley and Gerhard PetersContact, Twitter Facebook, Copyright', 'The American Presidency ProjectTerms of Service'))
undesirable_words <- tolower(append(undesirable_words, c(presidents, alphabet, lists, month, day, USA, start, end))) 

eo.dfm <- eo.corpus %>%
  dfm(., 
      tolower = TRUE, 
      remove = c(stopwords('english'), undesirable_words),
      remove_punct = TRUE, 
      remove_numbers = TRUE, 
      remove_symbols = TRUE, 
      remove_url = TRUE, 
      remove_separators = TRUE, 
      include_docvars = TRUE,
      dictionary = lexicoder)

head(eo.dfm)

# plot the most used topics
topics.df <- convert(eo.dfm, to = 'data.frame')
topics.df <- data.frame(colSums(topics.df[,-1], dims = 1))
topics.df$topic <- rownames(topics.df)
colnames(topics.df) <- c('sum' , 'topic')
topics.df

# filter all topics for which we have more than x
topics.df <- topics.df[topics.df$sum >250, ]
topics.df$perc <- topics.df$sum / sum(topics.df$sum)
topics.df <- topics.df %>% arrange(desc(sum))
topics.highest <- topics.df[1:10, 'topic']

# bar chart
plot.topics <- ggplot(topics.df, aes(x = reorder(topic, perc), y= perc)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() + 
  labs(title = 'Classified Topics with Lexicoder', 
       y = 'percentage',
       x = '',
       subtitle = paste0('n = ', nrow(data)))
plot.topics

# compute the most relevant per document
topics.lexicoder <- convert(eo.dfm, to = 'data.frame')
length(unique(topics.lexicoder$doc_id))

# select only EOs for which we have enough topic classifications, else we omit those 
# as they may be too ambiguous
topics.df <- subset(topics.lexicoder, select = -c(doc_id))
topics.df$rowSums <- rowSums(topics.df)
topics.df$eo_number <- as.numeric(topics.lexicoder$doc_id)
topics.df <- topics.df[topics.df$rowSums > 5, ]
colnames <- colnames(topics.df)[max.col(topics.df[,-c(29,30)],ties.method="first")]
for (i in 1:nrow(topics.df)) {
  topics.df$topic[i] <- colnames[i]
}

# merge with original data 
data$year <- year(data$date) 
topics.df <- left_join(topics.df, data, on = 'eo_number') %>% 
  select(c(eo_number, topic, year, president, party))

# conver to long 
topics.long <- topics.df %>% 
  filter(topic %in% topics.highest) %>% 
  group_by(year, topic) %>% 
  count(year, topic)

# plot top topics over time
plot.topics.time <- ggplot(topics.long, aes(x=year, y=n, color = factor(topic))) + 
  geom_line() +
  facet_grid(rows = vars(reorder(topic, -n)), scales = 'fixed', shrink = FALSE) +
  labs(title = 'Top 10 Topics Counts over time (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(topics.df)), 
       strip.text.y = element_text(
         size = 9, color = "red", face = "bold.italic")
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  theme(legend.position = "none") 
plot.topics.time

# Per Party and President ----
#===================#
top.10.topics.df <- topics.df %>% filter(topic %in% topics.highest)

eo.top10.party <- top.10.topics.df %>% count(year, topic, party)
eo.top10.president <- top.10.topics.df %>% count(year, topic, president)

# plot with party
plot.top10.party.topics <- ggplot(eo.top10.party, aes(x=year, y=n, color = factor(party))) + 
  geom_point() +
  facet_grid(rows = vars(reorder(topic, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts per topic and party (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(top.10.topics.df))) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  scale_color_manual(values=c("#0000FF", "#FF0000"))
plot.top10.party.topics

# plot with president
# first we need to reorder the factors
eo.top10.president$president <- factor(eo.top10.president$president, 
                                       levels = c("Harry S. Truman", "Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", 
                                                  'Richard Nixon', 'Gerald R. Ford', 'Jimmy Carter', 'Ronald Reagan', 'George Bush', 
                                                  'William J. Clinton', 'George W. Bush', 'Barack Obama', 'Donald J. Trump', 'Joseph R. Biden'))
plot.top10.president.topics <- ggplot(eo.top10.president, aes(x=year, y=n)) + 
  geom_point(aes(color = president)) +
  facet_grid(rows = vars(reorder(topic, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts per topic and president (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(top.10.topics.df))) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw()
plot.top10.president.topics


# Frequency of Topics by Party
plot.top10.party.frequency <-   ggplot(eo.top10.party, aes(x = n, y = reorder(topic, n), fill = party)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(title = 'Top 10 Frequency of Topics (1950 -2021)', 
       y = '',
       x = 'number of EOs',
       subtitle = paste0('n = ', nrow(top.10.topics.df))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))
plot.top10.party.frequency


# Save  ----
#===================#¨

# save plots
dir.create('./plots')

ggsave('plot.topics.png', path = './plots/', plot = plot.topics, device = 'png')
ggsave('plot.topics.time.png', path = './plots/', plot = plot.topics.time, device = 'png')
ggsave('plot.top10.party.topics.png', path = './plots/', plot = plot.top10.party.topics, device = 'png')
ggsave('plot.top10.president.topics.png', path = './plots/', plot = plot.top10.president.topics, device = 'png')
ggsave('plot.top10.party.frequency.png', path = './plots/', plot = plot.top10.party.frequency, device = 'png')
