library(stringr)
library(gsubfn)
library(data.table)
library(tidyverse)
library(dplyr)


# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list=ls())

data <- fread('./data/executive_orders.csv')

# inspect
head(data$H1)
head(data$H2)
head(data$H3)
head(data$date)


### FEATURE ENGINEERING --------------------------

# get document type
data$document_type <- substr(data$H1, start = 1, stop = 15)
unique(data$document_type)

# get executive order
data$eo_number <- strtoi(substr(data$H1, start = 16, stop = 21))
data$eo_number 

# extract title
data$title <- str_sub(data$H1, start = 23)
head(data$title)
tail(data$title)

# get presidents
data$president <- sub(",.*", "", data$H3)
unique(data$president)

# extract date
data$date <- str_sub(data$date, start = 6, end = -3)
data$date <- as.Date(data$date, format = "%B %d, %Y")
head(data$date)

