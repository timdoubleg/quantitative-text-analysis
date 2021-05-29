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


# Sentiment analysis: sentimentr ----
# This takes into account valence shifters
#===================#

sentiment_df<-sentiment_by(text.var = data$text,
                        polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        n.before = 5,
                        n.after = 2)

glimpse(sentiment_df)
summary(sentiment_df$ave_sentiment)

data <- cbind(data,sentiment_df$ave_sentiment)
data <-data %>% rename(sentiment_valence = V2)


# Sentiment analysis: AFINN ----
# Allows for stopwords removal and better cleaning and considers collocations
#===================#

library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.dictionaries)
library(quanteda.corpora)
library(quanteda.tidy)

## Corpus creation, tokenizatinon, cleaning, dfm creation ----

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


##  Creating sentiment indices, comparing indices, merging with data set ----

#creating df with doc_id, number of negative, positive, total number of tokens and total number of cleaned tokens
sentiment_corpus_combined <- data.frame(c(
   convert(sentiment_corpus_dfm_AFINN, to = "data.frame")),
   data.frame(ntoken(sentiment_corpus)),
   data.frame(ntoken(sentiment_corpus_dfm)))
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

#merging with "data" while normalizing the range of clean.sqrt
data <- sentiment_scores_afinn %>%
  transmute(
   sent_afinn_weighted = sent_afinn_cleaned.sqrt /
     {function(x,y) (max(x)-min(x))/(max(y)-min(y)) } (sentiment_scores_afinn$sent_afinn_cleaned.sqrt, data$sentiment_valence)
  ) %>%
  cbind(data, select(sentiment_corpus_combined, c(doc_id, n_tokens_cleaned)))


#writing data to new CSV
fwrite(data, './data/executive_orders_withcountry_withsentiments.csv')
data <- fread('./data/executive_orders_withsentiments.csv')



# Gathering outliers ----
#====================#

#first the data is selected and gathered, i.e. transformed into a longer format
gathered_data <- data %>%
  select(c(eo_number, sent_afinn_weighted, sentiment_valence)) %>%
  gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence))

#then the top and bottom 5 values for both measures are saved
top5_afinn <- gathered_data %>%
  filter(type_of_index == "sent_afinn_weighted") %>%
  slice_max(value, n = 5)

bottom5_afinn <- gathered_data %>%
  filter(type_of_index == "sent_afinn_weighted") %>%
  slice_min(value, n = 5)

top5_valence <- gathered_data %>%
  filter(type_of_index == "sentiment_valence") %>%
  slice_max(value, n = 5)

bottom5_valence <- gathered_data %>%
  filter(type_of_index == "sentiment_valence") %>%
  slice_min(value, n = 5)

#the result is saved in a data frame
sentiment_outliers <- rbind(top5_afinn,
                            top5_valence,
                            bottom5_afinn,
                            bottom5_valence)

#adding two columns that may be used for manual validation
sentiment_outliers$accuracy <- NA
sentiment_outliers$description <- NA

#adding number of words. will be used for validation and plotting below
sentiment_outliers <- left_join(sentiment_outliers, data[, c("eo_number", "n_tokens_cleaned", "title")], by = "eo_number")

#saving the data as a CSV
fwrite(sentiment_outliers, './data/executive_orders_sentiment_outlier_validation.csv')

#comparing with distribution in data set
summary(data$n_tokens_cleaned)

# Plots ----
#===================#

## Defining plots ----

#histogram of the two sentiment measures
fig_sent_comparison <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(value)) +
  geom_histogram(binwidth = .01) +
  facet_grid(rows = vars(type_of_index)) +
  xlim(-0.5,0.7) +
  labs(title = "Comparing the density of sentiment values by afinn and sentimentr",
       subtitle = "n = 3918") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 15))

#sorting presidents chronologically
data$president <- factor(data$president, 
                         levels = c("Harry S. Truman", "Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", 
                                    'Richard Nixon', 'Gerald R. Ford', 'Jimmy Carter', 'Ronald Reagan', 'George Bush', 
                                    'William J. Clinton', 'George W. Bush', 'Barack Obama', 'Donald J. Trump', 'Joseph R. Biden'))


#temporal perspective on sentiment measures
fig_sent_time <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=date,y=value)) +
  geom_point(aes(color=president),alpha = 0.2) +
  geom_smooth(aes(x=date,y=value),method=lm, se=FALSE) +
  facet_grid(cols = vars(type_of_index)) +
  ylim(-0.5,0.7) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr over time",
       subtitle = "n = 3918") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(strip.text = element_text(size = 15),
      axis.title = element_text(size = 15))

#boxplots for each president's average sentiment measures
fig_sent_presidents <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=president,y=value)) +
  geom_boxplot(aes(color=type_of_index)) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr between presidents",
       subtitle = "n = 3918") +
  coord_flip()

#boxplots for the two parties' average sentiment measures
fig_sent_party <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=party,y=value)) +
  geom_boxplot(aes(color=type_of_index)) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr between parties",
       subtitle = "n = 3918") +
  coord_flip()

#point plot highlighting the top and bottom five sentiment scores. mapped against word count
fig_sent_outliers <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=value,y=n_tokens_cleaned)) +
  geom_point(alpha = .3) +
  geom_point(data = sentiment_outliers, aes(x=value, y = n_tokens_cleaned), color = "red", size = 4) +
  facet_grid(cols = vars(type_of_index)) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr to word count",
       subtitle = "n = 3918")

#the above plot, but zoomed in
fig_sent_outliers_zoom1 <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=value,y=n_tokens_cleaned)) +
  geom_point(alpha = .3) +
  geom_point(data = sentiment_outliers, aes(x=value, y = n_tokens_cleaned), color = "red", size = 4) +
  facet_grid(cols = vars(type_of_index)) +
  ylim(0, 1900) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr to word count",
       subtitle = "n = 3918, subset with word count < 1900")

#the above plot, but even more zoomed in
fig_sent_outliers_zoom2 <- ggplot(data %>% gather(key = "type_of_index", value = "value", c(sent_afinn_weighted, sentiment_valence)), aes(x=value,y=n_tokens_cleaned)) +
  geom_point(alpha = .3) +
  geom_point(data = sentiment_outliers, aes(x=value, y = n_tokens_cleaned), color = "red", size = 4) +
  facet_grid(cols = vars(type_of_index)) +
  ylim(0, 900) +
  theme(legend.position = "top") +
  labs(title = "Comparing sentiment values by afinn and sentimentr to word count",
       subtitle = "n = 3918, subset with word count < 900")


## Plot output----
fig_sent_comparison
fig_sent_time
fig_sent_presidents
fig_sent_party
fig_sent_outliers
fig_sent_outliers_zoom1
fig_sent_outliers_zoom2

