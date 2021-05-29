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

# Set words to be cleaned ----
#===================#

# create a list of words we don't want to include
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
undesirable_words <- (append(undesirable_words, c(presidents, alphabet, romanNumber, month, day, USA, start, end))) 


# Training and Predicting Geographical Classification ----
#===================#

# create a corpus
eo.corpus <- corpus(data, 
                    docid_field =  "eo_number", 
                    text_field = 'text')
head(summary(eo.corpus))

# create tokens and remove unwanted words
eo.tokens <- tokens(eo.corpus, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE,
                    remove_symbols = TRUE, 
                    remove_url = TRUE) %>%
  tokens_remove(c(stopwords("english"),
                  undesirable_words)) 

# create labels
eo.label <- tokens_lookup(eo.tokens, 
                          dictionary = data_dictionary_newsmap_en, 
                          levels = 3) # level 3 stands for countries

# manual label filtering
# eo_number <- data$eo_number
# list.value = {}
# list.eonumber = {}
# n = 0
# for (i in eo_number) {
#   string <- toString(i)
#   if (length(eo.label[[string]]) != 0) {
#     n = n+1
#     list.eonumber[[n]] <- i
#     list.value[[n]] <- eo.label[[string]]
#   }
# }
# 
# labels.df <- data.frame(matrix(unlist(list.value), nrow=length(list.value), byrow=TRUE))
# labels.df$eo_number <- unlist(list.eonumber)
#
#eo.label[['14014']] <- NULL


# Document-feature matrix
dfmat_label <- dfm(eo.label, tolower = FALSE)
dfmat_feat <- dfm(eo.tokens, tolower = FALSE)

# select 
dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

# train the Newsmap textmodel
tmod_nm <- textmodel_newsmap(x = dfmat_feat_select, y = dfmat_label) 
  
# predict and cluster country labels on our documents
pred_nm <- predict(tmod_nm)
count <-table(pred_nm) 
count

# add country to dataframe
data$iso <- pred_nm
data$country <- countrycode(pred_nm, origin = 'iso2c', destination = 'country.name')

# Analysis Geographical Classification ----
#===================#

# check which coefficients are associated to the individual countries
coef <- coef(tmod_nm,n=10)[c("US","CN","IQ", "JP")] 
word.weight <- data.frame(unlist(coef)) 
word.weight$word <- rownames(word.weight)
rownames(word.weight) <- NULL
word.weight <- word.weight %>% separate(word, c('ISO', 'Word'))
colnames(word.weight) <- c('weight', 'ISO', 'word')

# reorder
word.weight <- data.table(word.weight)
word.weight[, ord := sprintf("%02i", frank(word.weight, weight, ties.method = "first"))]

# plot top 10 associated words
plot.coef <- ggplot(word.weight, aes(x = ord, y = weight, fill = ISO)) +
  geom_col(show.legend = NULL) +
  facet_wrap(~ ISO, ncol = 4, nrow = 1, scales = "free", drop = TRUE) +
  # use named character vector to replace x-axis labels
  scale_x_discrete(labels = word.weight[, setNames(as.character(word), ord)]) + 
  xlab(NULL) +
  theme(plot.subtitle=element_text(size=9, hjust=0.5, face="italic", color="black")) +
  labs(title = 'Highest weighted words of Newsmap for given countries', 
       y = '',
       x = '') +
  coord_flip()
plot.coef

# plot the aggregated worldmap 
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

# get EOs only for top 10 countries
top10 <- top20[1:10,]
target <- top10$country
eo.top10 <- filter(data, country %in% target)
nrow(eo.top10)/nrow(data) # account for the majority

# plot frequency of top 10 countries
plot.top10 <- ggplot(top10, aes(x = frequency, y = reorder(country, frequency))) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Top 10 Frequency of Countries (1950 -2021)', 
       y = '',
       x = 'number of EOs',
       subtitle = paste0('n = ', nrow(eo.top10))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black"))
plot.top10

# plot counts over times
# count EOs per year and country
us_territories <- c('Samoa', 'Puerto Rico', 'United States', 'Northern Mariana Islands', 'British Virgin Islands', 'Guam') # British Virigin Island is wrongly classified from the American Virign Islands
count.eo.noUSterr <- filter(data, !country %in% us_territories) 
top10_noUSterr <- filter(top20, !country %in% us_territories) 
top10_noUSterr <- top10_noUSterr[1:10,]
target <- top10_noUSterr$country
eo.top10 <- filter(data, country %in% target)

eo.top10$year <- year(eo.top10$date)
eo.top10$text <- NULL
eo.top10.all <- eo.top10 %>% count(year, country)
eo.top10.party <- eo.top10 %>% count(year, country, party)
eo.top10.president <- eo.top10 %>% count(year, country, president)

# plot all without US territories
plot.top10.all <- ggplot(eo.top10.all, aes(x=year, y=n, color = factor(country))) + 
  geom_line() +
  facet_grid(rows = vars(reorder(country, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts per country (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(eo.top10))) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  theme(legend.position = "none")
plot.top10.all


# Per Party and President ----
#===================#

# plot frequency of top 10 countries without US Territories
top10_noUSterr <- filter(data, country %in% target) %>% 
  count(country, party) %>% 
  arrange(desc(n))

plot.top10.noUSterr <-   ggplot(top10_noUSterr, aes(x = n, y = reorder(country, n), fill = party)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(title = 'Top 10 Frequency of Countries without US Territories (1950 -2021)', 
       y = '',
       x = 'number of EOs',
       subtitle = paste0('n = ', sum(top10_noUSterr$n))
  ) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  scale_fill_manual(values=c("#0000FF", "#FF0000"))
plot.top10.noUSterr


# plot with party
plot.top10.party <- ggplot(eo.top10.party, aes(x=year, y=n, color = factor(party))) + 
  geom_point() +
  facet_grid(rows = vars(reorder(country, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts per country and party (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(eo.top10))) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw() + 
  scale_color_manual(values=c("#0000FF", "#FF0000"))
plot.top10.party

# plot with president
# first we need to reorder the factors
eo.top10.president$president <- factor(eo.top10.president$president, 
                                       levels = c("Harry S. Truman", "Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", 
                                                  'Richard Nixon', 'Gerald R. Ford', 'Jimmy Carter', 'Ronald Reagan', 'George Bush', 
                                                  'William J. Clinton', 'George W. Bush', 'Barack Obama', 'Donald J. Trump', 'Joseph R. Biden'))
plot.top10.president <- ggplot(eo.top10.president, aes(x=year, y=n)) + 
  geom_point(aes(color = president)) +
  facet_grid(rows = vars(reorder(country, -n)), scales = 'fixed') +
  labs(title = 'Top 10 EOs counts per country and president (1950 -2021)', 
       y = '',
       x = 'Years',
       subtitle = paste0('n = ', nrow(eo.top10))) +
  theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
  theme_bw()
plot.top10.president

# plot the worldmap of Republican and Democrat
country.republican <- filter(data, party == 'Republican')
country.democrat <- filter(data, party == 'Democrat')

# create a counted dataframe
order_country <- function (party_affiliation) {
  data %>% 
    filter(party == party_affiliation) %>%
    filter(!country %in% us_territories) %>%
    group_by(country, iso) %>%
    summarise(frequency = n()) %>% 
    arrange(desc(frequency))
}

# create dataframes
count.republican <- order_country("Republican")
count.democrat <- order_country("Democrat")

# # aggregated plot
# plot_map <- function(df, n_rows, party) {
#   ggplot(df, aes(map_id = df$iso)) +
#     geom_map(aes(fill = frequency), map = world_map) +
#     expand_limits(x = world_map$long, y = world_map$lat) +
#     scale_fill_continuous(name = "Frequency") +
#     theme_void() +
#     coord_fixed() +
#     scale_fill_gradientn(colors=c("#56B1F7","green","yellow","orange","#ff0000"), values = scales::rescale(c(5, 25, 100, 200, 400))) +
#     labs(title = paste0(party, ': Frequency of countries (1950 -2021)'), 
#          subtitle = paste0('n = ', n_rows))
# }
# plot_map(count.republican, nrow(country.republican), "Republican")
# plot_map(count.democrat, nrow(country.democrat), "Democrat")

# get the top 10 per party over time
get_top10 <- function(df_count, df_country) {
  top10 <- df_count[1:10,]
  target <- top10$country
  df <- filter(df_country, country %in% target)
  df$year <- year(df$date)
  df %>% count(year, country)
}
top10.republican <- get_top10(count.republican, country.republican)
top10.democrat <- get_top10(count.democrat, country.democrat)

# plot top 10 over time
plot_top10_time <- function(df, party, n_nrow) {
  ggplot(df, aes(x=year, y=n, color = factor(country))) + 
    geom_line() +
    facet_grid(rows = vars(reorder(country, -n)), scales = 'fixed') +
    labs(title = paste0(party, ': Top 10 EOs counts over time (1950 -2021)'), 
         y = '',
         x = 'Years',
         subtitle = paste0('n = ', n_nrow)
    ) +
    theme(plot.subtitle=element_text(size=9, hjust=0, face="italic", color="black")) +
    theme_bw() + 
    theme(legend.position = "none") 
}
plot_top10_time(top10.republican, "Republican", sum(top10.republican$n))
plot_top10_time(top10.democrat, "Democrat", sum(top10.democrat$n))


# Manual Analysis ----
#===================#
eo.iraq <- filter(eo.top10, country == 'Iraq')
eo.china <- filter(eo.top10, country == 'China')
nrow(filter(data, iso == 'US'))/nrow(data)


# Testing Accurarcy ----
#===================#

# get a random sample of n=30 to manually check accuracy
set.seed(1234)
checking_accuracy <- sample_n(data, 30)
# get a xlsx version for easier checking
write_xlsx(checking_accuracy,"./check_accuracy.xlsx")


# Save  ----
#===================#¨

# save plots
dir.create('./plots')
ggsave('plot.top10.png', path = './plots/', plot = plot.top10, device = 'png')
ggsave('plot.coef.png', path = './plots/', plot = plot.coef, device = 'png', width = 7.5, height = 5)
ggsave('plot.map.png', path = './plots/', plot = plot.map, device = 'png')
ggsave('plot.top10.noUSterr.png', path = './plots/', plot = plot.top10.noUSterr, device = 'png', width = 7.5, height = 5)
ggsave('plot.top10.party.png', path = './plots/', plot = plot.top10.party, device = 'png')
ggsave('plot.top10.president.png', path = './plots/', plot = plot.top10.president, device = 'png')
ggsave('plot.top10.all.png', path = './plots/', plot = plot.top10.all, device = 'png')


# Save new dataframe
fwrite(data, './data/executive_orders_withcountry.csv')
