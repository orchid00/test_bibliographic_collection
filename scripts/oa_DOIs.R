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

# # Nest DOIs as tibble ----
nested_dois <-big_table_orcid_doi %>%
  group_by(orcid) %>%
  nest(data = c(doi)) %>%
  ungroup()

nested_dois

get_data_from_doi <- function(tibble_dois){
  test <- tibble_dois %>%
    mutate(doi_data = purrr::pmap(.l = list(
      dois = doi,
      email = "paula.martinez@ardc.edu.au",
      .flatten = TRUE),
      .f = roadoi::oadoi_fetch))
  
    return(test)
}

# testing:
# one row, 
# two rows 
test1 <- nested_dois[1:2, ] %>% 
  mutate(doi_data1 = purrr::map(.x = data,
    .f = get_data_from_doi))

#test1to9 <- test_all
# save
#save(test1to9, file = "test1to9_oa_lists.RData")
# will skip 10 
#test11 <- test_all
#save(test11, file = "test11_oa_lists.RData")
# will skip 12, 13
#test14 <- test_all
#save(test14, file = "test14_oa_lists.RData")
#skip 15
#test16_17 <- test_all
#save(test16_17, file = "test16_17_oa_lists.RData")
#skip 18
#test19_23 <- test_all
#save(test19_23, file = "test19_23_oa_lists.RData")
#skip 24
#test25_29 <- test_all
#save(test25_29, file = "test25_29_oa_lists.RData")
#skip 30

test_all <- tibble()


test <- nested_dois[10, ] %>% 
              mutate(doi_data1 = purrr::possibly(
                                  purrr::map(.x = data,
                                .f = get_data_from_doi),
                            otherwise = NA)
              )

test_all <- rbind(test_all, test)

  print("done")
  print(dim(test_all))



# Chop DOIs as list of characters ----
# nested_dois_c <-big_table_orcid_doi %>% 
#   group_by(orcid) %>% 
#   chop(doi) %>% 
#   ungroup()
# 
# nested_dois_c

# testing ---

#test with 1 orcid 4 dois ----
system.time({
  test <- big_table_orcid_doi[212:215, ] %>%
    mutate(doi_data = purrr::pmap(.l = list(
                      dois = doi,
                      email = "paula.martinez@ardc.edu.au",
                      .flatten = TRUE),
                      .f = roadoi::oadoi_fetch
                      ))
})
test
# View(test)
# rm(test)

doi_data_t <- nested_dois %>% 
  mutate(doi_data1 = purrr::map(.x = data,
                                .f = get_data_from_doi))


  # try next 
#     select(doi, url_for_pdf, url_for_landing_page,
#            license, evidence,
#            version, is_best, is_oa,
#            journal_name, publisher, title,
#            updated_resource) %>%
#     filter(is_best == TRUE)
# 
#   return(doi_data_t)
# }
doi_data_t <- filter_oa_best(test)
View(doi_data_t)
rm(doi_data_t)

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
system.time({
  all_oa_dois <- big_table_orcid_doi %>% 
    mutate(doi_data = purrr::pmap(.l = list(
      dois = doi,
      email = "paula.martinez@ardc.edu.au",
      .flatten = TRUE),
      .f = roadoi::oadoi_fetch
    ))
})
all_oa_dois
dim(all_oa_dois)
str(all_oa_dois)

# 5. save results
save(all_oa_dois, 
     file = paste0("all_oa_dois_", lubridate::today(),
                   ".RData"))

doi_data_t <- filter_oa_best(all_oa_dois)
dim(doi_data_t)
str(doi_data_t)

# 5. save results
save(doi_data_t, 
     file = paste0("doi_data_t_", lubridate::today(),
                   ".RData"))







# 6. filter those DOIs that are Open Access
oa_table_doi_info <- big_table_doi_info %>% 
  filter(is_oa == TRUE)