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

library(readtext)
library(quanteda)
library("quanteda.textplots")
library(stringr)
library(dplyr)
library(sentimentr)
library(data.table)
library(tidyr)
library(ggplot2)
library(topicmodels)
library(tm)
library(stm)

rm(list=ls())

# set wd to where file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_withcountry.csv')


# 1 Simple Word Frequency Analysis ----
#===================#

# download dictionary 
download.file('http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip', 
              destfile = './data/policy_agendas_english.zip')

# set up dictionary
lexicoder <- dictionary(file = './data/LTDjun2013/policy_agendas_english.lcd')
lengths(lexicoder)

# analyze for china
target_countries <- c('China')
eo.china <- filter(data, country %in% target_countries)

eo.china.corpus <- corpus(eo.china, 
                    docid_field =  "eo_number", 
                    text_field = 'text')

# another approach
eo.dfm <- eo.china.corpus %>% 
  dfm(., 
      tolower = TRUE, 
      remove = stopwords('english'), 
      remove_punct = TRUE, 
      remove_numbers = TRUE, 
      remove_symbols = TRUE, 
      remove_url = TRUE, 
      remove_separators = TRUE, 
      include_docvars = TRUE)

# what are most frequent used words
topfeatures(eo.dfm, 20)

# remove our own words
words <- c('united', 'states', 'president', 'america', 'presidency')
alphabet <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'g', 'h')
lists <- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')

# try again
eo.dfm <- eo.china.corpus %>% 
  dfm(., 
      tolower = TRUE, 
      remove = c(stopwords('english'), words, alphabet, lists),
      remove_punct = TRUE, 
      remove_numbers = TRUE, 
      remove_symbols = TRUE, 
      remove_url = TRUE, 
      remove_separators = TRUE, 
      include_docvars = TRUE)

topfeatures(eo.dfm, 20)

# plot a word_cloud
# textplot_wordcloud(eo.dfm, min_count = 6, random_order = FALSE, rotation = 0.25, 
#                    color = RColorBrewer::brewer.pal(8, "Dark2"))


# 2 Topic Modeling wtih Lexicoder ----
#===================#

# download dictionary 
download.file('http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip', 
              destfile = './data/policy_agendas_english.zip')

# set up dictionary
lexicoder <- dictionary(file = './data/LTDjun2013/policy_agendas_english.lcd')
lengths(lexicoder)

eo.china.dfm <- eo.china.corpus %>%
  dfm(., 
      tolower = TRUE, 
      remove = c(stopwords('english'), words, alphabet, lists),
      remove_punct = TRUE, 
      remove_numbers = TRUE, 
      remove_symbols = TRUE, 
      remove_url = TRUE, 
      remove_separators = TRUE, 
      include_docvars = TRUE,
      dictionary = lexicoder)

head(eo.china.dfm)

# plot the most used topics
topics.df <- convert(eo.china.dfm, to = 'data.frame')
topics.df <- data.frame(colSums(topics.df[,-1], dims = 1))
topics.df$topic <- rownames(topics.df)
colnames(topics.df) <- c('sum' , 'topic')
topics.df

# filter all topics for which we have more than 100
topics.df <- topics.df[topics.df$sum >100, ]

# pie chart
ggplot(topics.df, aes(x="", y = sum, fill = topic)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle('Top Topics with Lexicoder for China')

# # compute the most relevant per document
# topics.lexicoder <- convert(eo.china.dfm, to = 'data.frame')
# topics.lexicoder <- topics.lexicoder %>% gather(key = 'topic', value = 'count', -doc_id)
# length(unique(topics.lexicoder$doc_id))
# 
# topics.lexicoder2 <- topics.lexicoder %>% group_by(doc_id) %>% top_n(1, count) 
# nrow(topics.lexicoder2) # we have some documents, for which we have equal counts
# length(unique(topics.lexicoder2$doc_id))



# 3 Topic Modeling with Quanteda ----
#===================#

eo.china.dfm <- tokens(eo.china.corpus, 
                 remove_punct =  TRUE, 
                 remove_numbers = TRUE, 
                 remove_separators = TRUE, 
                 remove_symbols = TRUE, 
                 remove_url = TRUE) %>% 
  tokens_remove(c(stopwords("en"), words)) %>% 
  dfm()

eo.china.dfm <- dfm_trim(eo.china.dfm, min_termfreq = 4, max_termfreq = 10)

if (require("stm")) {
  my_lda_fit20 <- stm(eo.china.dfm, K = 20, 
                      verbose = TRUE)
  plot(my_lda_fit20)
}

# 4 Topic Modeling with LDA ----
#===================#

lda <- LDA(data, k = 2, control = list(seed = 1234))

data("AssociatedPress")


myCorpus <- Corpus(VectorSource(data$text))
tdm <- TermDocumentMatrix(myCorpus)

Terms(tdm)

