# test API

# 1. To install packages if they don't exist ----
#to_install <- c("httr")
#install.packages(setdiff(to_install, 
#                         rownames(installed.packages()))) 


# 2. Load the libraries----
library(tidyverse)
library(lubridate)
library(rorcid)
library(purrr)
library(roadoi)
library(httr)
library(jsonlite)

load("big_table_orcids.RData")

mydata <- big_table_test_five_orcids %>% 
  filter(is.na(external_id_normalized_value )) %>% 
  #filter(publication_date_year_value > 2015) %>% 
  filter(publication_date_year_value > 2020) %>% 
  select(title_title_value)
  
# format to search
#https://api.unpaywall.org/v2/search/?query=A%20toolkit%20for%20studying%20Varroa%20genomics%20and%20transcriptomics:%20Preservation,%20extraction,%20and%20sequencing%20library%20preparation&email=unpaywall_01@example.com

# 3. get data ----
base_url <- "https://api.unpaywall.org/v2/"
search_q <- "search/?query="
email_q <- "&email="
myemail <- "paula.martinez@ardc.edu.au"

#paste0(base_url, search_q, email_q, myemail)


mycalls <- 
paste0(base_url, search_q, "\"" , mydata$title_title_value , email_q, myemail)


# test1 <- mydata %>%
#   mutate(mycall = paste0(
#     base_url, search_q, "\"" , title_title_value, "\"" , 
#     email_q, myemail)) 


getting_dois <- httr::GET(url = URLencode(URL = mycalls[1]), 
                          verbose())


