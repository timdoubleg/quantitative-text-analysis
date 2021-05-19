library(httr)
library(rvest)
library(Rcrawler)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)


rm(list=ls())


# set wd to where the source file is
# make sure you have the datafiles in a /data/ folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# initiate empty list to keep track of crawler history (visited pages)
crawl_hist <- matrix(ncol = 2, nrow = 0) 
colnames(crawl_hist) <- c("visited_URL", "found_URLs")
# initiate control variable for frontier 
frontier_not_empty <- TRUE
# initiate counter variable
counter <- 1
frontier = {}

# set n low to test the code 
# else set it to the maximum amount of pages: 609
n = 609

for (i in 1:n){
  # DEQUEUEING AND FETCHING OF WEBPAGE ----- # dequeue URL from frontier
  current_URL <- paste0('https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?page=', i)
  # fetch page via HTTP
  resp <- GET(current_URL)
  # parse HTML
  html_doc <- read_html(resp)
  
  # LINK EXTRACTION -----
  # extract all URLs
  links_nodes <- html_nodes(html_doc, xpath = ".//a")
  URLs <- html_attr(links_nodes, name = "href")
  # clean URLs
  # remove anchors
  URLs <- URLs[!grepl("#", URLs, fixed = TRUE)]
  # remove NAs
  URLs <- na.omit(URLs)
  # canonicalize URLs, only keep unique entries
  URLs <- unique(LinkNormalization(links = URLs, current = current_URL)) # add to tail of frontier (first-in-first-out -> breadth first) 
  frontier[[i]] <- URLs
  
  # DATA PART -----
  # create log entry
  log_df <- data.frame(visited_URL = current_URL, URLs_found = URLs) # add to crawler history
  crawl_hist <- rbind(crawl_hist, as.matrix(log_df))
  
  # CONTROL PART ----
  # update control variables
  frontier_not_empty <- length(frontier) != 0
  # display status on screen:
  cat(paste0("\n", "Page no. ", counter, " visited!\n", current_URL))
  # update counter
  counter <- counter + 1
  
}

# transform to dataframe
df <- data.frame(unlist(frontier))
colnames(df) <- "urls"
filtered.urls <- df %>%filter(str_detect(urls, 'https://www.presidency.ucsb.edu/documents/executive-order'))
filtered.urls <- data.frame(unique((filtered.urls)))


# set up lists for extraction
url.list = {}
body.list = {}
h1.list = {}
h2.list = {}
h3.list = {}
date.list = {}


# web scrapting of texts 
for (i in 1:nrow(filtered.urls)){
  
  tryCatch({
    # read html 
    url = filtered.urls$urls[i]
    html <- read_html(url)
    
    # add url 
    url.list[[i]] = url
    
    # read text body
    body.list[[i]] <- html %>%
      html_nodes("p") %>%
      html_text() %>% 
      toString()
    
    # get titles
    h1.list[[i]] <- html %>% html_nodes("h1") %>% html_text() %>% toString
    h2.list[[i]] <- html %>% html_nodes("h2") %>% html_text() %>% toString
    h3.list[[i]] <- html %>% html_nodes("h3") %>% html_text() %>% toString
    
    # get date
    date.list[[i]] <- toString(html %>% html_nodes("div.field-docs-start-date-time") %>% html_text())
    
    
    # print progress
    print(paste0('success link nr. ', i))
  },
  error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
    }
  )
}

# merge all lists into a dataframe
df <- do.call(rbind, Map(data.frame, 
                         URL = url.list,
                         H1= h1.list,
                         H2 = h2.list,
                         H3 = h3.list,
                         body = body.list, 
                         date = date.list))

# save dataframe
dir.create('./data/')
fwrite(df, './data/executive_orders.csv')


