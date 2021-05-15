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
Sys.setenv(lang = "ENG")

# 2 Data import ----
#===================#

#the code below should help finding corrupt files by looping through all PDFs in the list
df_test <- data.frame() #creating an empty dataframe to be filled 
for(file in list.files(here("data"))) {
  loaded_text <- readtext(here("data", file), docvarsfrom = "filenames")
  df_test <- rbind(df_test, loaded_text)
} 

df1 <- readtext(here("data"),
         docvarsfrom = "filenames") 
#The filename correspond with the federal register doc id. They do not provide additional information, thus no further docvars are specified.



## 2.1 Data import: notes ----
#===================#

# Some PDFs like 08-62.pdf contain extensive parts with scanned text.

# 3 Data cleaning ----
#===================#


# 4 Data exploration ----
#===================#





