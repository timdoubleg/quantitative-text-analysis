library(stringr)
library(gsubfn)
library(data.table)
library(tidyverse)
library(dplyr)


# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#rm(list=ls())

data <- fread('./data/executive_orders.csv')

# inspect
head(data$H1)
head(data$H2)
head(data$H3)
head(data$date)


### FEATURE ENGINEERING & CLEANING--------------------------

# extract document type
data$document_type <- substr(data$H1, start = 1, stop = 15)
unique(data$document_type)

# extract executive order number
data$eo_number <- strtoi(substr(data$H1, start = 16, stop = 21))
length(unique(data$eo_number))

# extract title
data$title <- str_sub(data$H1, start = 23)
head(data$title)
tail(data$title)

# extract presidents
data$president <- sub(",.*", "", data$H3)
unique(data$president)

# extract date
data$date <- str_sub(data$date, start = 6, end = -3)
data$date <- as.Date(data$date, format = "%B %d, %Y")
head(data$date)

# filter all dates after 1950
data <- data[data$date > '1950-01-01',]

# drop duplicate EOs
data <- data[!duplicated(data$eo_number),]

# drop not needed columns 
data <- subset(data, select = -c(URL, H1, H2, H3))

# show new ranges
summary(data)

# Save dataframe ------------------
fwrite(data, './data/executive_orders_cleaned.csv')


