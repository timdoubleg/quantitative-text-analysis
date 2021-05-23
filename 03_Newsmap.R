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

highest_topic_df <- subset(topics.lexicoder, select = -c(doc_id) )
highest_topic <- colnames(highest_topic_df)[max.col(highest_topic_df,ties.method="first")]

# bind with original df
data<-cbind(data,highest_topic)


# Geographical Classification ----
#===================#

# filter topics for which we are certain, won't concern foreign policy
undesired_topics <- c('agriculture', 'education', 'forestry', 'social_welfare', 'housing', 'civil_rights')
data <- filter(data, !highest_topic %in% undesired_topics)

eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')
head(summary(eo.corpus))

# create tokens
eo.tokens <- tokens(eo.corpus, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE,
                    remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"),
                  undesirable_words)) 

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
coef <- coef(tmod_nm,n=15)[c("US","CN","IQ")] 
df <- data.frame(unlist(coef)) 
df$word <- rownames(df)
rownames(df) <- NULL
df <- df %>% separate(word, c('ISO', 'Word'))
colnames(df) <- c('weight', 'ISO', 'word')

plot.coef <- ggplot(df, aes(x = reorder(word, weight), y=weight, fill = ISO)) +
  geom_col(show.legend = NULL)  +
  facet_wrap(~ISO, 
             ncol = 3, nrow = 1, 
             scales = "free") +
  theme(plot.subtitle=element_text(size=9, hjust=0.5, face="italic", color="black")) +
  coord_flip() + 
  labs(title = 'Highest weighted words of Newsmap for given countries', 
       y = '',
       x = '')
plot.coef
  
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

# check Top 10 countries and convert ISO
top20 <- dat_country[1:20, ]
top20$country <- countrycode(top20$id, origin = 'iso2c', destination = 'country.name')
top20 <- top20[order(-top20$frequency),]
rownames(top20) <- NULL

# add country to dataframe
data_sub$iso <- pred_nm
data_sub$country <- countrycode(pred_nm, origin = 'iso2c', destination = 'country.name')

# get a random sample of n=30 to manually check accuracy
set.seed(1234)
checking_accuracy <- sample_n(data_sub, 30)
# get a xlsx version for easier checking
# write_xlsx(checking_accuracy,"C:\\Users\\User_name\\Desktop\\data_frame.xlsx")"

# get EOs only for top 10 countries
top10 <- top20[1:10,]
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


# plot frequency of top 10 countries without US Territories
us_territories <- c('Samoa', 'Puerto Rico', 'United States', 'Northern Mariana Islands', 'British Virgin Islands', 'Guam') # British Virigin Island is wrongly classified from the American Virign Islands
top10_noUSterr <- filter(top20, !country %in% us_territories) 
top10_noUSterr <- top10_noUSterr[1:10,]
target <- top10_noUSterr$country
eo.top10 <- filter(data, country %in% target)
  
plot.top10.noUSterr <-   ggplot(top10_noUSterr, aes(x = frequency, y = reorder(country, frequency))) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Top 10 Frequency of countries without US Territories (1950 -2021)', 
       y = '',
       x = 'number of EOs',
       subtitle = paste0('n = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black"))
plot.top10.noUSterr


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



# Save  ----
#===================#¨

# save plots
dir.create('./plots')
ggsave('plot_topics.png', path = './plots/', plot = plot.topics, device = 'png')
ggsave('plot.top10.png', path = './plots/', plot = plot.top10, device = 'png')
ggsave('plot.coef.png', path = './plots/', plot = plot.coef, device = 'png')
ggsave('plot.map.png', path = './plots/', plot = plot.map, device = 'png')
ggsave('plot.top10.time.png', path = './plots/', plot = plot.top10.time, device = 'png')
ggsave('plot.top10.noUSterr.png', path = './plots/', plot = plot.top10.noUSterr, device = 'png')


#Save new dataframe
fwrite(data, './data/executive_orders_withcountry.csv')
