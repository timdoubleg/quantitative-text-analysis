library(stringr)
library(gsubfn)
library(data.table)
library(tidyverse)
library(dplyr)


# set wd to where the source is
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

# extract date
data$date <- str_sub(data$date, start = 6, end = -3)
data$date <- as.Date(data$date, format = "%B %d, %Y")
head(data$date)

# filter all dates after 1950
data <- data[data$date > '1950-01-01',]

# drop duplicate EOs
data <- data[!duplicated(data$eo_number),]

# extract presidents
data$president <- sub(",.*", "", data$H3)
unique(data$president)

# drop not needed columns 
data <- subset(data, select = -c(URL, H1, H2, H3))

# add party affiliation by president
data <- data %>%
  mutate(
    party = case_when(
      str_detect(president, "Truman")         ~ "Democrat",
      str_detect(president, "Eisenhower")     ~ "Republican",
      str_detect(president, "Kennedy")        ~ "Democrat",
      str_detect(president, "Johnson")        ~ "Democrat",
      str_detect(president, "Nixon")          ~ "Republican",
      str_detect(president, "Ford")           ~ "Republican",
      str_detect(president, "Carter")         ~ "Democrat",
      str_detect(president, "Reagan")         ~ "Republican",
      str_detect(president, "George Bush")    ~ "Republican",
      str_detect(president, "Clinton")        ~ "Democrat",
      str_detect(president, "George W. Bush") ~ "Republican",
      str_detect(president, "Obama")          ~ "Democrat",
      str_detect(president, "Trump")          ~ "Republican",
      str_detect(president, "Biden")          ~ "Democrat",
    )
  )

# show new ranges
summary(data)

# rename column to text
data <- data %>% rename(text = body)

# Save dataframe ------------------
fwrite(data, './data/executive_orders_cleaned.csv')


