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

rm(list=ls())

Sys.setenv(lang = "ENG")
Sys.setlocale("LC_ALL", "English") #not setting this to English will break as.Date()

# 2 Data import ----
#===================#

df1 <- readtext(here("data"),
         docvarsfrom = "filenames") 
#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.

# Some PDFs like 08-62.pdf contain extensive parts with scanned text.

#The code below extractsthe pattern "Executive Order <nr> of <month> <nr>, <year>" from the texts
#It then extracts the EO number and the date of issuance and adds these as variables to a new dataframe df1


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

nrow(df1) #there are 1108 documents
range(nchar(df1$EO_nr), na.rm = TRUE) #all EO_nr have the same length, as they should
sum(is.na(df1$EO_nr)) #there are 25 missing EO_nr values
sum(is.na(df1$date)) #there are 25 missing date values
sum(is.na(df1$EO_nr) | is.na(df1$date)) #25 indicates that the documents missing EO_nr are the same missing date values

df1 %>% filter(is.na(date)) %>% data.frame() %>%  select(doc_id) #displays a df that contains all the documents with missing values

#manual inspection reveals that some EOs only contain "Order" but not "Executive Order <nr>"



## adapting the regex ----

new_regex <- "Order\\s{1}of\\s{1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}"

df2 <- df1 %>% ## I don't understand why this code is not working! all the dates remain NA ----
  filter(is.na(date)) %>%
  find_EO_dates(regex = new_regex)

sum(is.na(df1$date)) # there are still 25 missing date values, meaning the code above failed



df1 %>% ## meanwhile, picking a single document like this somehow works, the date is correctly extracted. I don't know why  ----
  filter(is.na(date)) %>%
  filter(doc_id == "2016-29494.pdf") %>%
  select(text) %>%
  str_extract(new_regex) %>% 
  str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") %>% 
  as.Date(format = "%B %d, %Y")

# right_join(df1, df2, by = "text") can't join the two df together before this issue is resolved

df1<- filter(df1,!is.na(date))

#The following code assigns the number of words/length of an Executive order.
corp_main <- corpus(df1)
number_corp_main<-ntoken(corp_main)
df1<-cbind(df1,number_corp_main)

# For the geographical classification, dates do not hold much value. As such we will define the following custom stopwords.
# Since there are many USA specific tokens, due to the origin of the USA, some USA specific stopwords will also be removed to avoid an overrepresentation of the USA.

corp1 <- corpus(df1)

month <- c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
USA <- c("States", "Sec", "United","Act","Secretary","Council","State","Department","General","Section","Management","America","Committee","American","Americans","Washington")

tokens_corp1 <- tokens(corp1, remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"),
                  month,
                  day,
                  USA)) 

# The model associates country specific features with ISO 3166-1 country codes. The codes for USA and China for example are "US" & "CN"

toks_label <- tokens_lookup(tokens_corp1, dictionary = data_dictionary_newsmap_en, 
                            levels = 3) # level 3 stands for countries
dfmat_label <- dfm(toks_label, tolower = FALSE)

dfmat_feat <- dfm(tokens_corp1, tolower = FALSE)
dfmat_feat_select <- dfm_select(dfmat_feat, pattern = "^[A-Z][A-Za-z0-9]+", 
                                valuetype = "regex", case_insensitive = FALSE) %>% 
  dfm_trim(min_termfreq = 10)

tmod_nm <- textmodel_newsmap(dfmat_feat_select, y = dfmat_label)

coef(tmod_nm,n=15)[c("US","CN")]

pred_nm <- predict(tmod_nm)
head(pred_nm,1000)

# getting the frequency of countries in Executive Orders
prediction_country<-table(pred_nm)
prediction_country


# the following code will join df1 (or the main df, if new one)
# df_geography<-cbind(df1,pred_nm)





# 3 Data cleaning ----
#===================#



# 4 Data exploration ----
#===================#


