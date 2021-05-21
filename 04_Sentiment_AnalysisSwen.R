# Quantitative Text Analysis 2021 
# Group paper                     
#                                 
# by                              
# Ella Stanisch                   
# David Klug                       
# Swen Hartlieb                   
# Tim Graf                        


# 1 Loading libraries ----
#===================#

library(here)
library(readtext)
library(quanteda)
library(stringr)
library(dplyr)
library(newsmap)
library(sentimentr)
library(data.table)
library(tidyr)
library(maps)
library(countrycode)
library(ggplot2)

# Setup----
#===================#
#rm(list=ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_cleaned.csv')




# Sentiment Analysis with Syuzhet

library(syuzhet)
library(tidyverse)
library(lubridate)
library(plotly)
library(tibble)
library(dplyr)

# The code calculates the sentiment in each EO
# method can easily be changed to afinn, bing or nrc. When using nrc, also add: lang="english"

data[,"sentiment_EO"]<-NA
data$sentiment_EO <- get_sentiment(data$text, method="afinn")

fig_sentiment <- ggplot(data %>% filter(date > "1950-04-11"),aes(x=date,y=sentiment_EO, color = sentiment_EO)) + geom_point() +
  geom_smooth(aes(x=date,y=sentiment_EO),method=lm, se=FALSE)
fig_sentiment

fig_sentiment_China <- ggplot(data %>% filter(country == "China"),aes(x=date,y=sentiment_EO, color = sentiment_EO)) + geom_point() +
  geom_smooth(aes(x=date,y=sentiment_EO),method=lm, se=FALSE)
fig_sentiment_China  

fig_Obama <- ggplot(data %>% filter(president == "Barack Obama"),aes(x=date,y=sentiment_EO, color = sentiment_EO)) + geom_point() +
  geom_smooth(aes(x=date,y=sentiment_EO),method=lm, se=FALSE)
fig_Obama







# frequency of EO by president
presidents <- table(data$president)
presidents_df <- as.data.frame(presidents)
names(presidents_df)[1]="president"
presidents_df







# Taking valence shifters into consideration
library(RSentiment)

sentiment_df<-sentiment(text.var = data$text,
                        polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        n.before = 5,
                        n.after = 2)

sentiment_mean<-aggregate(sentiment_df[,4],list(sentiment_df$element_id),mean)
data <- cbind(data,sentiment_mean$sentiment)
data <-data %>% rename(sentiment_valence = V2)

fig_3 <- ggplot(data %>% filter(president == "Barack Obama"),aes(x=date,y=sentiment_valence, color = sentiment_valence)) + geom_point() +
  geom_smooth(aes(x=date,y=sentiment_valence),method=lm, se=FALSE)
fig_3






# # Sentiment Analysis with simple AFINN according to method with cleaned corpus.

library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.dictionaries)
library(quanteda.corpora)
library(quanteda.tidy)

sentiment_corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')
head(summary(sentiment_corpus))

sentiment_corpus_tokens <- tokens(sentiment_corpus, 
                           remove_punct = TRUE,
                           remove_numbers = TRUE,
                           remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) 

collocations <- sentiment_corpus_tokens %>%
  textstat_collocations(min_count = 250,
                        size = 2) %>%
  filter(count >250)

sentiment_corpus_tokens <- tokens_compound(sentiment_corpus_tokens,
                                    phrase(collocations$collocation))

sentiment_corpus_dfm<-dfm(sentiment_corpus_tokens)

sentiment_corpus_dfm_AFINN <- sentiment_corpus_dfm %>%
  dfm(.,
      dictionary = data_dictionary_AFINN)

emotion <- convert(sentiment_corpus_dfm_AFINN, to = "data.frame")
net_emotion_AFINN <- emotion$positive-emotion$negative
data <- cbind(data,net_emotion_AFINN)



format(as.Date(df1$Date, format="%d/%m/%Y"),"%Y")

test_data <- data %>% group_by(country) %>% summarise(n = n(), min_rank(n)) %>% filter(min_rank(n) < 10)

ggplot(data, aes(x=date, y=sentiment_EO)) + 
  geom_line() +
  facet_grid(rows = vars(country))
  
  facet_grid(rows = vars(reorder(country, -sentiment_EO)), scales = 'fixed')

plot.top10.sentiment <- ggplot(data, aes(x=year, y=sentiment_EO, color = factor(country))) + 
  geom_line() +
  facet_grid(rows = vars(reorder(country, -sentiment_EO)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts over time (1950 -2021)', 
       y = 'Sentiment',
       x = 'Years',
       subtitle = paste0('sentiment_EO = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  theme(legend.position = "none") 
plot.top10.sentiment
