library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(downloader)


rm(list=ls())

# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### FETCH DATA -----------------------------------
# https://www.federalregister.gov/developers/documentation/api/v1

# set the base url for the REST API
base = 'https://www.federalregister.gov/api/v1/'

# fetch all kinds of  documents
res = GET(paste0(base, 'documents.json'), 
          query = list(per_page = 1000))
data = fromJSON(rawToChar(res$content))
df <- data.frame(data)
unique(df$results.type) # there are four different types of documents
range(df$results.publication_date) # data only goes back 10 days


# get a single executive order
res = GET('https://www.federalregister.gov/api/v1/documents/2021-10139.json')
data2 = fromJSON(rawToChar(res$content))

# get all executive orders
res = GET('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Btype%5D%5B%5D=PRESDOCU&conditions%5Bpresidential_document_type%5D%5B%5D=executive_order')
data3 = fromJSON(rawToChar((res$content)))
df3 <- data.frame(data3$results)
range(df3$publication_date) # data goes from 1997 to 2021

# as we can get maximum 1000 but there are more executive orders we make an additional request
res = GET('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpublication_date%5D%5Blte%5D=2000-01-01&conditions%5Bpresidential_document_type%5D%5B%5D=executive_order')
data4 = fromJSON((rawToChar(res$content)))
df4 <- data.frame(data4$results)

#merge both dataframes
df <- full_join(df3, df4)
range(df$publication_date) # data goes from 1994 to 2021

# check if the merge made sense
min_date <- min(range(df3$publication_date)) # data goes from 1997 to 2021
missing_df <- df[which(df$publication_date < as.Date(min_date)), ]# it does

# download all pdfs
dir.create('./data')
for (i in 1:nrow(df)){
  name = df$document_number[i]
  tryCatch(download(df$pdf_url[i], destfile = paste0('./data/', name, '.pdf'),timeout = 1000, mode="wb"), 
           error = function(e) print(paste(name, e)))
}

# der geht lange 
# tryCatch(download('https://www.govinfo.gov/content/pkg/FR-2018-03-08/pdf/2018-04860.pdf', destfile = paste0('./data2/', name, '.pdf'), timeout = 360), 
#          error = function(e) print(paste(name, e)))


