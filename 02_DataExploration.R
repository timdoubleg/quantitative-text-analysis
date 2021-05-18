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

rm(list=ls())

Sys.setenv(lang = "ENG")
Sys.setlocale("LC_ALL", "English") #not setting this to English will break as.Date()

# 2 Data import ----
#===================#

# as these pdf files are corrupt we delete them 
file.remove('./data/executive_orders/00-31252.pdf')
file.remove('./data/executive_orders/2018-04860.pdf')

# function to read in data
read_pdfs <- function (folder_dir){
  readtext(paste0('./data/', folder_dir),
           docvarsfrom = "filenames", 
           docvarnames = 'document_number',
           ignore_missing_files = TRUE, 
           verbosity = 3) 
}

# read in all data
executive.orders <- read_pdfs('executive_orders/')
presidential.orders <- read_pdfs('presidential_orders/')
memorandums <- read_pdfs('memorandums/')
proclamations <- read_pdfs('proclamations/')
notices <- read_pdfs('notices/')

# merge with existing dfs to get date
executive.orders.df <- fread('./data/dataframes/executive_orders.csv')
presidential.orders.df <- fread('./data/dataframes/presidential_orders.csv')
memorandums.df <- fread('./data/dataframes/memorandums.csv')
proclamations.df <- fread('./data/dataframes/proclamations.csv')
notices.df <- fread('./data/dataframes/notices.csv')

# merge dataframes based on their document_number
executive.orders.df <- left_join(executive.orders.df, executive.orders, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts))
presidential.orders.df <- left_join(presidential.orders.df, presidential.orders, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts))
memorandums.df <- left_join(memorandums.df, memorandums, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts))
proclamations.df <- left_join(proclamations.df, proclamations, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts))
notices.df <- left_join(notices.df, notices, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts))

# remove unnecessary values
rm(executive.orders, memorandums, notices, presidential.orders, proclamations)



#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.

# Some PDFs like 08-62.pdf contain extensive parts with scanned text.

#The code below extracts the pattern "Executive Order <nr> of <month> <nr>, <year>" from the texts
#It then extracts the EO number and the date of issuance and adds these as variables to a new dataframe df1


# 3 Data cleaning ----
#===================#

## Extracting dates of issuance and executive order number ----
find_EO_dates <- function(data, 
                          regex_pattern = "Executive\\s{1}Order\\s{1}\\d{4,6}\\s{1}of\\s{1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") {
  mutate(data,
         EO_nr = 
           str_extract(text, regex_pattern) %>%
           str_extract("\\d{4,6}") %>% 
           as.numeric(),
         date = 
           str_extract(text, regex_pattern) %>% 
           str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") %>% 
           as.Date(format = "%B %d, %Y"))
}

df1 <- df1 %>% find_EO_dates()


## Inspecting the result ----
nrow(df1) #there are 1108 documents
range(nchar(df1$EO_nr), na.rm = TRUE) #all EO_nr have the same length, as they should
sum(is.na(df1$EO_nr)) #there are 25 missing EO_nr values
sum(is.na(df1$date)) #there are 25 missing date values
sum(is.na(df1$EO_nr) | is.na(df1$date)) #25 indicates that the documents missing EO_nr are also the ones missing date values

df1 <- filter(df1, !is.na(date)) #keeping only data with no missing values


#adding presidents by checking the EO number. reference: https://www.federalregister.gov/presidential-documents/executive-orders
df1 <- df1 %>% 
  mutate(president = case_when(
      EO_nr >= 12945 & EO_nr <= 13197 ~ "Clinton",
      EO_nr >= 13198 & EO_nr <= 13488 ~ "W. Bush",
      EO_nr >= 13489 & EO_nr <= 13764 ~ "Obama",
      EO_nr >= 13765 & EO_nr <= 13984 ~ "Trump",
      EO_nr >= 13985 ~ "Biden"))



# 4 Data exploration ----
#===================#

# The following code creates the already cleaned main corpus for our analysis.
corp_main <- corpus(df1)


corp_main <- tokens(corp_main, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE,
                    remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english")) 

# By adding the number of tokens to our dataframe df1, we get a fealing of the length of each EO.

number_corp_main<-ntoken(corp_main)
df1<-cbind(df1,number_corp_main)

# For our analysis, we need to know the country, each EO is addressing.
# Given our large amount of data, going manually through every document
# seems to be too time-consuming. Furthermore, since we know that some
# documents are for example specifically designed to address China or
# Chinese companies without ever mentioning the word China, a dictionary
# approach might fall short in classifying all documents according to the
# country the EO is addressing. As such, we opted for a semisupervised model
# for geographical document classification, called Newsmap (https://tutorials.quanteda.io/machine-learning/newsmap/).

# For the geographical classification, dates do not hold much value. As such we will define the following custom stopwords.
# Since there are many USA specific tokens, due to the origin of EOs being in the USA, some USA specific stopwords will also be removed to avoid an overrepresentation of the USA.

corp1 <- corpus(df1)

month <- c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
USA <- c("States", "Sec", "United","Act","Secretary","Council","State","Department","General","Section","Management","America","Committee","American","Americans","Washington")

tokens_corp1 <- tokens(corp1, 
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"),
                  month,
                  day,
                  USA)) 

# The seed dictionary below, data_dictionary_newsmap_en, is a geographical dictionary developed by Newsmap. It only contains the names of countries and capital cities.
# The levels argument will determine the keys being recorded in our toks-label object.
# Level 1 for example would stand for the continent level. But since we are interested in
# countries, we have to use level 3.

toks_label <- tokens_lookup(tokens_corp1, 
                            dictionary = data_dictionary_newsmap_en, 
                            levels = 3) # level 3 stands for countries

# Since the dictionary only contains country and capital names, the following model, textmodel_newsmap, is going to associate country specific features with ISO 3166-1 country codes. The codes for USA and China for example are "US" & "CN"
# The model uses two document-feature matrices. One being based on our country lables above, the second on the features in our corpus, from which we eliminated the USA-specific stopwords.
# The second dfm will be reduced in size by pattern matching as well as by only considering values that appear at least 10 times in the dfm. 

dfmat_label <- dfm(toks_label, tolower = FALSE)
dfmat_feat <- dfm(tokens_corp1, tolower = FALSE)

dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z0-9]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)


# Next, we will train the Newsmap model in a semi-supervised document classification approach, using the two document-feature matrices.
tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label)

coef(tmod_nm,n=15)[c("US","CN","IN")] 

#The code above extracts the model coefficients and thus gives us the strength with which the model associates certain words with a country based on our data.

# The following code predicts the most strongly associated country for each EO.
pred_nm <- predict(tmod_nm)
head(pred_nm,1000)

# Here we get the frequency of countries in the Executive Orders
prediction_country<-table(pred_nm)
prediction_country


# The following code will join df1 with our predicted country labels.
df1<-cbind(df1,pred_nm)


# Junk code (will delete this at some point) ----


## Trying to deal with missing values ----
#df1 %>% filter(is.na(date)) %>% data.frame() %>%  select(doc_id) #displays a df that contains all the documents with missing values

#manual inspection reveals that some EOs only contain "Order" but not "Executive Order <nr>"


### Adapting the regex ----
#new_regex <- "Order\\s{1}of\\s{1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}"

#df2 <- df1 %>% ## I don't understand why this code is not working! all the dates remain NA ----
#filter(is.na(date)) %>%
# find_EO_dates(regex_pattern = new_regex)

#sum(is.na(df1$date)) # there are still 25 missing date values, meaning the code above failed


#df1 %>% ## meanwhile, picking a single document like this somehow works, the date is correctly extracted. I don't know why  ----
# filter(is.na(date)) %>%
#   filter(doc_id == "2016-29494.pdf") %>%
#   select(text) %>%
#   str_extract(new_regex) %>% 
#   str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") %>% 
#   as.Date(format = "%B %d, %Y")
# right_join(df1, df2, by = "text") can't join the two df together before this issue is resolved



# Research Ideen ----


# Positiv-negagiv-Skala 
#   >>> sentiment analysis, pro Land, ?ber Zeit 
#   >>> evtl. m?ssen Dictionaries verwendet werden bei L?ndern mit wenig Daten
#   >>> unsupervised ML?

# Relevanz von L?ndern ?ber Zeit
#   >>> evtl mit Textl?nge gewichten

# Interaktive time series Visualisierung

# Weitere Daten benutzen mit API? wie begr?nden?

# Pr?sident

# Topic analysis für ein Land mit vielen EO

# Was sind FP preference von US Präsidenten?

#Kontrollvariablen und UV
#Tenure
#Majority size in congress
#Reelection ambition
#New president or reelected?
#Election year?
#Change in popularity?

                     









