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

# 2 Data import ----
#===================#

df1 <- readtext(here("data"),
         docvarsfrom = "filenames") 
#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.

# Some PDFs like 08-62.pdf contain extensive parts with scanned text.

corp1 <- corpus(df1)

# 3 Data cleaning ----
#===================#

## Finding the date of issuance through regex ----



# 4 Data exploration ----
#===================#





