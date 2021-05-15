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
                          regex = "Executive\\s{1}Order\\s{1}\\d{4,6}\\s{1}of\\s{1}(January|February|March|April|May|June|July|August|September|October|November|December)\\s{1}\\d{1,2},\\s{1}\\d{4}") {
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

corp1 <- corpus(df1)

# 3 Data cleaning ----
#===================#



# 4 Data exploration ----
#===================#


