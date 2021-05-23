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

# 2 Topic Modeling wtih Lexicoder ----
#===================#

# download dictionary 
download.file('http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip', 
              destfile = './data/policy_agendas_english.zip')

# set up dictionary
lexicoder <- dictionary(file = './data/LTDjun2013/policy_agendas_english.lcd')
lengths(lexicoder)

eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')

# remove our own words
words <- c('united', 'states', 'president', 'america', 'presidency')
alphabet <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'g', 'h')
lists <- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')

eo.dfm <- eo.corpus %>%
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

head(eo.dfm)

# plot the most used topics
topics.df <- convert(eo.dfm, to = 'data.frame')
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
  ggtitle('Top Topics with Lexicoder')

# # compute the most relevant per document
topics.lexicoder <- convert(eo.dfm, to = 'data.frame')
topics.lexicoder <- topics.lexicoder %>% gather(key = 'topic', value = 'count', -doc_id)
length(unique(topics.lexicoder$doc_id))


data<-cbind(data,topics.lexicoder)



# 
topics.lexicoder2 <- topics.lexicoder %>% group_by(doc_id) %>% top_n(1, count) 
nrow(topics.lexicoder2) # we have some documents, for which we have equal counts
length(unique(topics.lexicoder2$doc_id))
