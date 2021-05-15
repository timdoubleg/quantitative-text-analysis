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
library(dplyr)
Sys.setenv(lang = "ENG")

# 2 Data import ----
#===================#

df1 <- readtext(here("data"),
         docvarsfrom = "filenames") 
#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.
# Some PDFs like 08-62.pdf contain extensive parts of scanned text. This may have to be taken into account during the analysis or interpretation of the results.
#Two files had to be manually deleted.



# 3 Data cleaning ----
#===================#


# 4 Data exploration ----
#===================#





