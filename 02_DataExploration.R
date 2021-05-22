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
library(quanteda.dictionaries)
library(quanteda.corpora)
library(quanteda.tidy)
library(stringr)
library(dplyr)
library(newsmap)
library(sentimentr)
library(data.table)


rm(list=ls())

Sys.setenv(lang = "ENG")
Sys.setlocale("LC_ALL", "English") #not setting this to English will break as.Date()

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

test <- read_pdfs("/executive_orders/E9-30020.pdf")
glimpse(test)

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
executive.orders.df <- left_join(executive.orders.df, executive.orders, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts, type))
presidential.orders.df <- left_join(presidential.orders.df, presidential.orders, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts, type))
memorandums.df <- left_join(memorandums.df, memorandums, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts, type))
proclamations.df <- left_join(proclamations.df, proclamations, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts, type))
notices.df <- left_join(notices.df, notices, on = 'document_number') %>% select(-c(doc_id, abstract, excerpts, type))

# add variable 
executive.orders.df$document_type <- 'executive.order'
presidential.orders.df$document_type <- 'presidential.order'
memorandums.df$document_type <- 'memorandum'
proclamations.df$document_type <- 'proclamation'
notices.df$document_type <- 'notice'

# merge all dataframes together
documents <- rbind(executive.orders.df, presidential.orders.df, memorandums.df, proclamations.df, notices.df)
documents <- documents[order(publication_date),]

# count how many NAs we have
sum(is.na(documents$text))

# remove texts with NAs
documents <- documents[!is.na(documents$text), ]

# remove unnecessary values
rm(executive.orders, memorandums, notices, presidential.orders, proclamations)

# check how many unique documents we have
length(unique(documents$document_number))


#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.

# Some PDFs like 08-62.pdf contain extensive parts with scanned text.

#The code below extracts the pattern "Executive Order <nr> of <month> <nr>, <year>" from the texts
#It then extracts the EO number and the date of issuance and adds these as variables to a new dataframe documents


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

documents <- documents %>% find_EO_dates()


## Inspecting the result ----
nrow(documents)
range(nchar(documents$EO_nr), na.rm = TRUE)
sum(is.na(documents$EO_nr))
sum(is.na(documents$date))
sum(is.na(documents$EO_nr) | is.na(documents$date))

#keeping only data with no missing values
documents <- filter(documents, !is.na(date)) 

#importing a list of presidential documents sorted by president. contains the var "document_type"
list_of_documents_by_clinton <- read.csv("https://www.federalregister.gov/documents/search.csv?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=william-j-clinton&conditions%5Bpresidential_document_type%5D%5B%5D=determination&conditions%5Bpresidential_document_type%5D%5B%5D=memorandum&conditions%5Bpresidential_document_type%5D%5B%5D=notice&conditions%5Bpresidential_document_type%5D%5B%5D=presidential_order&conditions%5Bsigning_date%5D%5Byear%5D=&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&order=document_number&per_page=1000")
list_of_documents_by_w.bush <- read.csv("https://www.federalregister.gov/documents/search.csv?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=george-w-bush&conditions%5Bpresidential_document_type%5D%5B%5D=determination&conditions%5Bpresidential_document_type%5D%5B%5D=memorandum&conditions%5Bpresidential_document_type%5D%5B%5D=notice&conditions%5Bpresidential_document_type%5D%5B%5D=presidential_order&conditions%5Bsigning_date%5D%5Byear%5D=&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&order=document_number&per_page=1000")
list_of_documents_by_obama <- read.csv("https://www.federalregister.gov/documents/search.csv?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=barack-obama&conditions%5Bpresidential_document_type%5D%5B%5D=determination&conditions%5Bpresidential_document_type%5D%5B%5D=memorandum&conditions%5Bpresidential_document_type%5D%5B%5D=notice&conditions%5Bpresidential_document_type%5D%5B%5D=presidential_order&conditions%5Bsigning_date%5D%5Byear%5D=&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&order=document_number&per_page=1000")
list_of_documents_by_trump <- read.csv("https://www.federalregister.gov/documents/search.csv?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=donald-trump&conditions%5Bpresidential_document_type%5D%5B%5D=determination&conditions%5Bpresidential_document_type%5D%5B%5D=memorandum&conditions%5Bpresidential_document_type%5D%5B%5D=notice&conditions%5Bpresidential_document_type%5D%5B%5D=presidential_order&conditions%5Bsigning_date%5D%5Byear%5D=&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&order=document_number&per_page=1000")
list_of_documents_by_biden <- read.csv("https://www.federalregister.gov/documents/search.csv?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=joe-biden&conditions%5Bpresidential_document_type%5D%5B%5D=determination&conditions%5Bpresidential_document_type%5D%5B%5D=memorandum&conditions%5Bpresidential_document_type%5D%5B%5D=notice&conditions%5Bpresidential_document_type%5D%5B%5D=presidential_order&conditions%5Bsigning_date%5D%5Byear%5D=&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&order=document_number&per_page=1000")

#adding a new var "president" to the "documents" object by comparing "document_number"
documents <- documents %>% 
  mutate(president = as.factor(case_when(
    (EO_nr >= 12945 & EO_nr <= 13197) | document_number %in% list_of_documents_by_clinton$document_number ~ "Clinton",
    (EO_nr >= 13198 & EO_nr <= 13488) | document_number %in% list_of_documents_by_w.bush$document_number ~ "W. Bush",
    (EO_nr >= 13489 & EO_nr <= 13764) | document_number %in% list_of_documents_by_obama$document_number ~ "Obama",
    (EO_nr >= 13765 & EO_nr <= 13984) | document_number %in% list_of_documents_by_trump$document_number ~ "Trump",
    (EO_nr >= 13985) | document_number %in% list_of_documents_by_biden$document_number ~ "Biden",
  )))

#remove lists to unclutter the environment
rm(
  list_of_documents_by_clinton,
  list_of_documents_by_w.bush,
  list_of_documents_by_obama,
  list_of_documents_by_trump,
  list_of_documents_by_biden
)



# 4 Data exploration ----
#===================#

# Using Newsmap 
corp1 <- corpus(documents) # corpus for Newsmap

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

toks_label <- tokens_lookup(tokens_corp1, 
                            dictionary = data_dictionary_newsmap_en, 
                            levels = 3) # level 3 stands for countries

dfmat_label <- dfm(toks_label, tolower = FALSE)
dfmat_feat <- dfm(tokens_corp1, tolower = FALSE)

dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z0-9]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label) # Training the Newsmap model

coef(tmod_nm,n=15)[c("US","CN","IQ")] # Extraction of model coefficients

pred_nm <- predict(tmod_nm) # Prediction of country labels on our documents

prediction_country<-table(pred_nm) # Frequency of countries in EO
prediction_country

documents<-cbind(documents,pred_nm) # joining documents with predicted country labels

#remove Data and values to unclutter the environment
rm(dfmat_feat,dfmat_feat_select,dfmat_label,tokens_corp1,toks_label,collocations,corp_main_tokens,
   corp_main,tmod_nm,day,month,corp1,number_corp_main,pred_nm,prediction_country,USA,number_corp_main)


# Junk code (will delete this at some point) ----


## Trying to deal with missing values ----
#documents %>% filter(is.na(date)) %>% data.frame() %>%  select(doc_id) #displays a df that contains all the documents with missing values

#manual inspection reveals that some EOs only contain "Order" but not "Executive Order <nr>"


### Adapting the regex ----
#new_regex <- "Order\\s{1}of\\s{1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}"

#df2 <- documents %>% ## I don't understand why this code is not working! all the dates remain NA ----
#filter(is.na(date)) %>%
# find_EO_dates(regex_pattern = new_regex)

#sum(is.na(documents$date)) # there are still 25 missing date values, meaning the code above failed


#documents %>% ## meanwhile, picking a single document like this somehow works, the date is correctly extracted. I don't know why  ----
# filter(is.na(date)) %>%
#   filter(doc_id == "2016-29494.pdf") %>%
#   select(text) %>%
#   str_extract(new_regex) %>% 
#   str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") %>% 
#   as.Date(format = "%B %d, %Y")
# right_join(documents, df2, by = "text") can't join the two df together before this issue is resolved



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

# Topic analysis f?r ein Land mit vielen EO

# Was sind FP preference von US Pr?sidenten?

#Kontrollvariablen und UV
#Tenure
#Majority size in congress
#Reelection ambition
#New president or reelected?
#Election year?
#Change in popularity?

                     









