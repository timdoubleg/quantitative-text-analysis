library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(downloader)
library(data.table)


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
          query = list(per_page = 20))
data = fromJSON(rawToChar(res$content))
df <- data.frame(data)
unique(df$results.type) # there are four different types of documents
range(df$results.publication_date) # data only goes back 10 days

# get a single executive order (for testing purposes)
res = GET('https://www.federalregister.gov/api/v1/documents/2021-10139.json')
doc.types = fromJSON(rawToChar(res$content))

# get all executive orders
res = GET('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Btype%5D%5B%5D=PRESDOCU&conditions%5Bpresidential_document_type%5D%5B%5D=executive_order')
data = fromJSON(rawToChar((res$content)))
exec.order1 <- data.frame(data$results)
range(exec.order1$publication_date) # data goes from 1997 to 2021

# as we can get maximum 1000 but there are more executive orders we make an additional request
res = GET('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpublication_date%5D%5Blte%5D=2000-01-01&conditions%5Bpresidential_document_type%5D%5B%5D=executive_order')
data = fromJSON((rawToChar(res$content)))
exec.order2 <- data.frame(data$results)

# get all presidential orders
res = GET(paste0('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpresidential_document_type%5D%5B%5D=', 'presidential_order'))
data = fromJSON((rawToChar(res$content)))
presidential.orders <- data.frame(data$results)

# get all memorandums 
res = GET(paste0('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpresidential_document_type%5D%5B%5D=', 'memorandum'))
data = fromJSON((rawToChar(res$content)))
memorandums <- data.frame(data$results)

# get all notices
res = GET(paste0('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpresidential_document_type%5D%5B%5D=', 'memorandum'))
data = fromJSON((rawToChar(res$content)))
notices <- data.frame(data$results)

# get all proclamations
res = GET(paste0('https://www.federalregister.gov/api/v1/documents.json?per_page=1000&conditions%5Bpresidential_document_type%5D%5B%5D=', 'memorandum'))
data = fromJSON((rawToChar(res$content)))
proclamations <- data.frame(data$results)


### MERGE DATA -----------------------------------

#merge the dataframes for executive orders
exec.orders <- full_join(exec.order1, exec.order2)
range(exec.orders$publication_date) # data goes from 1994 to 2021

# check if the merge made sense
min_date <- min(range(exec.order1$publication_date)) # data goes from 1997 to 2021
missing_df <- df[which(exec.orders$publication_date < as.Date(min_date)), ]# it does

# drop column "agencies"
drop_column <- function(df, column) {
  df %>% select(-column)
}

exec.orders <- drop_column(exec.orders, 'agencies')
presidential.orders <- drop_column(presidential.orders, 'agencies')
memorandums <- drop_column(memorandums, 'agencies')
notices <- drop_column(notices, 'agencies')
proclamations <- drop_column(proclamations, 'agencies')


### SAVE DATA -----------------------------------

# download all pdfs
dir.create('./data/executive_orders')
dir.create('./data/presidential_orders')
dir.create('./data/memorandums')
dir.create('./data/notices')
dir.create('./data/proclamations')

# NOTE: CHANGE MODE = 'WB' FOR WINDOWS AND LEAVE IT OUT FOR MAC
download_pdfs <- function(dataframe, folder_name) {
  for (i in 1:nrow(dataframe)){
    name = dataframe$document_number[i]
    tryCatch(download(dataframe$pdf_url[i], destfile = paste0('./data/', folder_name, '/', name, '.pdf'), timeout = 1000, mode = "wb"),
             error = function(e) print(paste(name, e)))
  }
}

# download all PDFs
# this will take a while
download_pdfs(exec.orders, 'executive_orders')
download_pdfs(presidential.orders, 'presidential_orders')
download_pdfs(memorandums, 'memorandums')
download_pdfs(notices, 'notices')
download_pdfs(proclamations, 'proclamations')

# save dfs to merge later
dir.create('./data/dataframes')
fwrite(exec.orders, './data/dataframes/executive_orders.csv' )
fwrite(presidential.orders, './data/dataframes/presidential_orders.csv' )
fwrite(memorandums, './data/dataframes/memorandums.csv' )
fwrite(notices, './data/dataframes/notices.csv' )
fwrite(proclamations, './data/dataframes/proclamations.csv' )

