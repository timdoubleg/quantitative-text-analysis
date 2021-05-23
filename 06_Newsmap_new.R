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
length(unique(topics.lexicoder$doc_id))

highest_topic_df <- subset(topics.lexicoder, select = -c(doc_id) )
highest_topic <- colnames(highest_topic_df)[max.col(highest_topic_df,ties.method="first")]

data<-cbind(data,highest_topic)



# Newsmap
target_topics <- c('macroeconomics', 'foreign_trade','intl_affairs','defence','culture','sstc','finance','civil_rights')
data <- filter(data, highest_topic %in% target_topics)

eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')
head(summary(eo.corpus))

# set some dictionaries for later
month <- c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
USA <- c("United","America","American","Americans","Washington")

# create tokens
eo.tokens <- tokens(eo.corpus, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE,
                    remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"),
                  month,
                  day,
                  USA)) 

# create labels
eo.label <- tokens_lookup(eo.tokens, 
                          dictionary = data_dictionary_newsmap_en, 
                          levels = 3) # level 3 stands for countries

# Document-feature matrix
dfmat_label <- dfm(eo.label, tolower = FALSE)
dfmat_feat <- dfm(eo.tokens, tolower = FALSE)

# select 
dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z0-9]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

# train the newsmap textmodel
tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label) 

# check which coefficients are associated to the individual countries
coef(tmod_nm,n=15)[c("US","CN","IQ", "IN")] 

# predict and cluster country labels on our documents
pred_nm <- predict(tmod_nm)
count <-table(pred_nm) 
count

# plot the newsmap
dat_country <- as.data.frame(count, stringsAsFactors = FALSE)
dat_country <- dat_country[order(-dat_country$Freq),]
dat_country$country <- countrycode(dat_country$pred_nm, origin = 'iso2c', destination = 'country.name')
colnames(dat_country) <- c("id", "frequency","country")
world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region) # convert country name to ISO code

plot.map <- ggplot(dat_country, aes(map_id = id)) +
  geom_map(aes(fill = frequency), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_continuous(name = "Frequency") +
  theme_void() +
  coord_fixed() +
  scale_fill_gradientn(colors=c("#56B1F7","green","yellow","orange","#ff0000"), values = scales::rescale(c(5, 25, 100, 200, 400))) +
  labs(title = 'Frequency of countries (1950 -2021)', 
       subtitle = paste0('n = ', nrow(data)))
plot.map

# check for counts of EU
target <- c('Germany', 'France', 'Italy', 'United Kingdom', 'Belgium', 'Poland', 'Sweden')
EU <- filter(dat_country, country %in% target)
sum(EU$frequency)

# check Top 10 countries and convert ISO
top10 <- dat_country[1:10, ]
top10$country <- countrycode(top10$id, origin = 'iso2c', destination = 'country.name')
top10 <- top10[order(-top10$frequency),]
rownames(top10) <- NULL

# add country to dataframe
data$iso <- pred_nm
data$country <- countrycode(pred_nm, origin = 'iso2c', destination = 'country.name')

# get EOs only for top 10 countries
target <- top10$country
eo.top10 <- filter(data, country %in% target)
nrow(eo.top10)/nrow(data) # account for the majority

# plot frequency of top 10 countries
plot.top10 <- ggplot(top10, aes(x = frequency, y = reorder(country, frequency))) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Top 10 Frequency of countries (1950 -2021)', 
       y = '',
       x = 'number of EOs',
       subtitle = paste0('n = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black"))
plot.top10


# plot counts over times
eo.top10$year <- year(eo.top10$date)

# count EOs per year and country
country.long <- eo.top10 %>% count(year, country)

# plot top 10 over time
plot.top10.time <- ggplot(country.long, aes(x=year, y=n, color = factor(country))) + 
  geom_line() +
  facet_grid(rows = vars(reorder(country, -n)), scales = 'free') +
  labs(title = 'Top 10 EOs counts over time (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  theme(legend.position = "none") 
plot.top10.time

# with fixed
plot.top10.time <- ggplot(country.long, aes(x=year, y=n, color = factor(country))) + 
  geom_line() +
  facet_grid(rows = vars(reorder(country, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts over time (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  theme(legend.position = "none") 
plot.top10.time
