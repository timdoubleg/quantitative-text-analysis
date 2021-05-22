# Quantitative Text Analysis 2021 
# Group paper                     
#                                 
# by                              
# Ella Stanisch                   
# David Klug                       
# Swen Hartlieb                   
# Tim Graf                        


# Loading libraries ----
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

library(syuzhet)
library(tidyverse)
library(lubridate)
library(plotly)
library(tibble)

# Setup----
#===================#
rm(list=ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_withcountry.csv')


# Taking valence shifters into consideration with sentimentr ----
#===================#

sentiment_df<-sentiment_by(text.var = data$text,
                        polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        n.before = 5,
                        n.after = 2)

glimpse(sentiment_df)
summary(sentiment_df$ave_sentiment)

data <- cbind(data,sentiment_df$ave_sentiment)
data <-data %>% rename(sentiment_valence = V2)

plot.sentiment_by(sentiment_df)

rm(sentiment_df)



# Sentiment Analysis with simple AFINN according to method with cleaned corpus. ----
# Allows for stopwords removal and better cleaning and considers collocations
#===================#

library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.dictionaries)
library(quanteda.corpora)
library(quanteda.tidy)

#corpus creation
sentiment_corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')

glimpse(head(summary(sentiment_corpus)))

#tokenization
sentiment_corpus_tokens <- tokens(sentiment_corpus, 
                           remove_punct = TRUE,
                           remove_numbers = TRUE,
                           remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) 

#accounting for collocations
collocations <- sentiment_corpus_tokens %>%
  textstat_collocations(min_count = 250,
                        size = 2) %>%
  filter(count >250)

sentiment_corpus_tokens <- tokens_compound(sentiment_corpus_tokens,
                                    phrase(collocations$collocation))

#creating dfm with cleaned tokens
sentiment_corpus_dfm <- dfm(sentiment_corpus_tokens)

#creating dfm with afinn dictionary
sentiment_corpus_dfm_AFINN <- sentiment_corpus_dfm %>%
  dfm(., dictionary = data_dictionary_AFINN)

#creating df with doc_id, number of negative, positive, total number of tokens and total number of cleaned tokens
sentiment_corpus_combined <- data.frame(c(
   convert(sentiment_corpus_dfm_AFINN, to = "data.frame")),
   data.frame(ntoken(sentiment_corpus)),
   data.frame(ntoken(sentiment_corpus_dfm)),
   row.names = FALSE)
colnames(sentiment_corpus_combined)[4:5] <- c("n_tokens", "n_tokens_cleaned")

#creating four afinn score indices
#two of these mirror the formula used by sentimentr by taking the squareroot of n before dividing
sentiment_scores_afinn <- transmute(sentiment_corpus_combined,
  sent_afinn =              (positive-negative)/(n_tokens),
  sent_afinn.sqrt =         (positive-negative)/sqrt(n_tokens),
  sent_afinn_cleaned =      (positive-negative)/(n_tokens_cleaned),
  sent_afinn_cleaned.sqrt = (positive-negative)/sqrt(n_tokens_cleaned),
  )


#checking ranges and extremes
apply(sentiment_scores_afinn, 2, function(x){max(x)-min(x)})
{function(x)max(x)-min(x)} (data$sentiment_valence)
apply(sentiment_scores_afinn, 2, range)
range(data$sentiment_valence)

#none of the four afinn indices is comparable to sentimentr results
#for the final analysis, the clean.sqrt index will be used
#to provide somewhat better comparability, its range will be normalized, i.e. divided by the difference in range compared to sentimentr

#merging with "data"
data <- sentiment_scores_afinn %>%
  transmute(
   sent_afinn_weighted = sent_afinn_cleaned.sqrt /
     {function(x,y) (max(x)-min(x))/(max(y)-min(y)) } (sentiment_scores_afinn$sent_afinn_cleaned.sqrt, data$sentiment_valence)
  ) %>%
  cbind(data)


rm(sentiment_corpus_dfm, 
   sentiment_corpus_dfm_AFINN, 
   sentiment_corpus_tokens, 
   net_emotion_AFINN, 
   sentiment_corpus, 
   sentiment_corpus_combined,
   emotion, 
   collocations)

# Plots ----
#===================#

## defining plots ----
fig_index_comparison1 <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(value)) +
  geom_histogram(binwidth = .01) +
  facet_grid(rows = vars(type_of_index)) +
  xlim(-0.8,0.8) +
  labs(title = "Comparing the density of sentiment values by afinn and sentimentr")

fig_index_comparison2 <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=date,y=value)) +
  geom_point(aes(color=president),alpha = 0.2) +
  geom_smooth(aes(x=date,y=value),method=lm, se=FALSE) +
  facet_grid(cols = vars(type_of_index)) +
  ylim(-0.8,0.8) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr over time")


## plot output----
fig_index_comparison1
fig_index_comparison2