# get DOIs from titles
# Description use: https://docs.ropensci.org/roadoi/
# rOpenSci: The roadoi package
# roadoi interacts with the Unpaywall REST API, an openly 
# available web-interface which returns metadata about open 
# access versions of scholarly works.
# # This client supports the most recent API Version 2.
# # API Documentation: https://unpaywall.org/products/api
# Date: 2021-08-18
# Author: Paula Andrea Martinez


# 1. To install the development version ----
# use the devtools package
# uncomment if installation is needed
#devtools::install_github("ropensci/roadoi")

# 2. Load the libraries
library(tidyverse)
library(lubridate)
library(rorcid)
library(purrr)
library(roadoi)

# 3. read data ----
load("RData/big_table_orcids.RData")

big_table_orcid_doi <- big_table_test_five_orcids %>% 
  select(path, doi = external_id_normalized_value) %>% 
  filter(!is.na(doi)) %>% 
  distinct(doi, .keep_all = TRUE) %>%
  mutate(path = str_remove(path, "(/)")) %>% 
  separate(col = path, into = c("orcid", "works"), sep = "/work/") %>% 
  select(-works)

#clean
rm(big_table_test_five_orcids)

# Example ----
# nested_dois_example <- tibble(
#   orcid = c("0000-0003-3685-174X",
#             "0000-0002-8630-1458"),
#   data =  list(c("10.1130/0016-7606(1997)109<1515:fositc>2.3.co",
#                  "10.1130/0016-7606(1997)109<1515:fositc>2.3.co;2",
#                  "10.1175/1087-3562(1999)003<0002:tdmcwc>2.0.co;2"),
#                c("10.1111/faf.12287",
#                  "10.1038/nature25504"))
# )

# Nest DOIs as tibble ----
nested_dois <-big_table_orcid_doi %>%
  group_by(orcid) %>% 
  summarise(doi = list(doi))

#check tibble
nested_dois
#View(nested_dois)

# testing
test <- nested_dois[12:13, ] %>%
  mutate(doi_data = purrr::pmap(list(
    dois = doi,
    email = "paula.martinez@ardc.edu.au",  ### PLEASE add your email here
    .flatten = TRUE),
    .f = purrr::possibly(## the saving function
      roadoi::oadoi_fetch, 
      otherwise = NA))) %>% 
  select(-doi) %>% 
  unnest(doi_data)

View(test)
# test seems to be working


# let's try all
all_dois_data <- nested_dois %>%
  mutate(doi_data = purrr::pmap(list(
    dois = doi,
    email = "paula.martinez@ardc.edu.au",  ### PLEASE add your email here
    .flatten = TRUE),
    .f = purrr::possibly(## the saving function
      roadoi::oadoi_fetch, 
      otherwise = NA))) %>% 
  select(-doi) %>% 
  unnest(doi_data)

View(all_dois_data)
# started at 16:04
# showed errors at 16:07
# still running at 16:30
# still running at 16:48
# running for more than one hour
# 17:12
# 17:27
# 17:31
# done at 17:41
#126 warnings
options(nwarnings = 10000)
saved_my_warnings <- summary(warnings())

save(all_dois_data,
     file = paste0("raw_data/","all_dois_data_", lubridate::today(),
                   ".RData"))

capture.output(saved_my_warnings,
     file = paste0("raw_data/","saved_my_warnings_", lubridate::today(),
                   ".txt"))

first_check <-
all_dois_data %>% 
  filter(is_best == TRUE)

savemyobjectRDS(first_check, "raw_data", "first_check")
#readRDS("raw_data/first_check_2021-08-26.RData")

articles_since_2012 <-
  all_dois_data %>% 
  filter(year > 2012) %>% 
  filter(is_best == TRUE)

savemyobjectRDS(articles_since_2012, "raw_data", "articles_since_2012")

articles_since_2012_oa <-
  articles_since_2012 %>% 
  filter(is_oa == TRUE) 

savemyobjectRDS(articles_since_2012_oa, "raw_data", "articles_since_2012_oa")

# function wrapper to get info about DOIs
# get_data_from_doi <- function(tibble_dois){
#   test <- tibble_dois %>%
#     mutate(doi_data = purrr::pmap(.l = list(
#       dois = doi,
#       email = "paula.martinez@ardc.edu.au", ## Your email here
#       .flatten = TRUE),
#       .f = purrr::possibly(roadoi::oadoi_fetch, ## the saving function
#                            otherwise = NA)))    ## returns NA if no data is found
#   
#   # message("Rows where error:")
#   # print(
#   #   filter(test, map_lgl(doi_data, is.na))
#   # )
#   # message("-----")
#   return(test)
# }
# 
# # testing:
# test <- nested_dois[12:13, ] %>% 
#   mutate(doi_data1 = 
#            purrr::map(.x = data,
#                       .f = get_data_from_doi)
#   )
# 
# View(test)

# one row, 
# two rows 
# test1 <- nested_dois[1:2, ] %>% 
#   mutate(doi_data1 = purrr::map(.x = data,
#     .f = get_data_from_doi))

# a few rows with causes crashes have been identified
# save
# 10, 12, 13, 15, 18, 24, 30
# I stopped the tests after that



# test again with the new get_data_from_doi
# function which is now using possibly as safeguard! :)
# and it works like a charm :)
# test <- nested_dois[12:13, ] %>% 
#               mutate(doi_data1 = 
#                        purrr::map(.x = data,
#                                   .f = get_data_from_doi)
#                )


#test_all <- tibble()
#test_all <- rbind(test_all, test)

# print("done")
#print(dim(test_all))



# Chop DOIs as list of characters ----
# nested_dois_c <-big_table_orcid_doi %>% 
#   group_by(orcid) %>% 
#   chop(doi) %>% 
#   ungroup()
# 
# nested_dois_c

# testing ---

# View(test)
# rm(test)

filter_oa_best <- function(mydata) {
    #try next
  doi_data_t <- tibble()
  if(!is.null(mydata)) {
    doi_data_t <- mydata %>% 
      select(doi, url_for_pdf, url_for_landing_page,
             license, evidence,
             version, is_best, is_oa,
             journal_name, publisher, title,
             updated_resource) %>%
      filter(is_best == TRUE)
  }
  return(doi_data_t)
}

new_test <- ((test[[3]][[1]])[[2]][[46]]) # a random selection
result <- filter_oa_best(new_test)

#test with 1 orcid 89 dois ----
# ~ 1.636 minutes
# system.time({
#   test1 <- big_table_orcid_doi[1:89, ] %>%
#     mutate(doi_data = purrr::pmap(.l = list(
#       dois = doi,
#       email = "paula.martinez@ardc.edu.au",
#       .flatten = TRUE),
#       .f = roadoi::oadoi_fetch
#     ))
# 
# })
# test1
# View(test1)
# 
# doi_data_t <- filter_oa_best(test1)
# View(doi_data_t)
# rm(doi_data_t)

# rm(test1)



# test with 2 orcid 126 dois ----
# in ~ 2.304 minutes
# system.time({
#   test2 <- big_table_orcid_doi[90:215, ] %>% 
#     mutate(doi_data = purrr::pmap(.l = list(
#       dois = doi,
#       email = "paula.martinez@ardc.edu.au",
#       .flatten = TRUE),
#       .f = roadoi::oadoi_fetch
#     ))
# })
# test2
# View(test2)
# rm(test2)


## all results will run in about 
## ~ minutes
# system.time({
#   all_oa_dois <- big_table_orcid_doi %>% 
#     mutate(doi_data = purrr::pmap(.l = list(
#       dois = doi,
#       email = "paula.martinez@ardc.edu.au",
#       .flatten = TRUE),
#       .f = roadoi::oadoi_fetch
#     ))
# })
# all_oa_dois
# dim(all_oa_dois)
# str(all_oa_dois)

# 5. save results
# save(all_oa_dois, 
#      file = paste0("all_oa_dois_", lubridate::today(),
#                    ".RData"))

# doi_data_t <- filter_oa_best(all_oa_dois)
# dim(doi_data_t)
# str(doi_data_t)

# 5. save results
# save(doi_data_t, 
#      file = paste0("doi_data_t_", lubridate::today(),
#                    ".RData"))







# 6. filter those DOIs that are Open Access
# oa_table_doi_info <- big_table_doi_info %>% 
#   filter(is_oa == TRUE)