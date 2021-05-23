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
rm(list=ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- fread('./data/executive_orders_cleaned.csv')

# EO analysis----
#===================#
head(data) 
data$year <- year(data$date)
president.long <- as.data.frame(data %>% count(year, president,))
head(president.long)
plot.eo.president <- ggplot(president.long, aes(year, n, fill = (president))) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  labs(title = 'EOs per president (1950 -2021)', 
       y = 'EO count',
       x = 'Years',
       subtitle = paste0('n = ', nrow(data)))

plot.eo.president

# Geographical Classification ----
#===================#

# make all lowercase
data$text <- sapply(data$text, tolower)

# remove special characters
removeSpecialChars <- function(x) gsub("[^a-z ]", " ", x)
data$text <- sapply(data$text, removeSpecialChars)

# combine title and text 
data$text <- with(data, paste0(title, text))
 
# make a corpus
eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')
head(summary(eo.corpus))

# set some dictionaries for later
month <- (c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))
day <- (c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
USA <- (c("States", "Sec", "sec", "United","Act","Secretary","Council","State","Department","General","Section", "section", "management","America","committee","American","Americans","Washington", 
                 'Executive', 'President'))

# create tokens
eo.tokens <- tokens(eo.corpus, 
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_symbols = TRUE) %>%
            tokens_remove(c(stopwords("english"),
                              month,
                              day,
                              USA, 
                            valuetype = 'regex')) # regular expression 

# create labels
eo.label <- tokens_lookup(eo.tokens, 
                            dictionary = data_dictionary_newsmap_en, 
                            levels = 3, # level 3 stands for countries
                          nested_scope = 'dictionary') # reduce ambiguity in dictionary lookup

# Document-feature matrix
dfmat_label <- dfm(eo.label, tolower = FALSE)
dfmat_feat <- dfm(eo.tokens, tolower = FALSE)

# select 
dfmat_feat_select <- dfm_select(dfmat_feat, 
                                selection = 'keep', # keep features
                                pattern = "^[A-Z][A-Za-z]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

# train the newsmap textmodel
tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label) 

# check which coefficients are associated to the individual countries
coef(tmod_nm,n=15)[c("US","CN","IQ", "JP", "IN")] 

# predict and cluster country labels on our documents
pred_nm <- predict(tmod_nm)
count <-table(pred_nm) 
count

# plot the newsmap
dat_country <- as.data.frame(count, stringsAsFactors = FALSE)
dat_country <- dat_country[order(-dat_country$Freq),]
dat_country$country <- countrycode(dat_country$pred_nm, origin = 'iso2c', destination = 'country.name')
colnames(dat_country) <- c("id", "frequency")
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
sum(EU$Freq)

# check Top 10 countries and convert ISO
top10 <- dat_country[1:10, ]
top10$country <- countrycode(top10$id, origin = 'iso2c', destination = 'country.name')
top10 <- top10[order(-top10$frequency),]
rownames(top10) <- NULL

# add country to dataframe
data$iso <- pred_nm
data$country <- countrycode(pred_nm, origin = 'iso2c', destination = 'country.name')


# Plots----
#===================#

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


# Save  ----
#===================#¨

# save plots
dir.create('./plots')
ggsave('plot_president.png', path = './plots/', plot = plot.eo.president, device = 'png')

#Save new dataframe
fwrite(data, './data/executive_orders_withcountry.csv')
