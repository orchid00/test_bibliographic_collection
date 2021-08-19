## From : https://ciakovx.github.io/rorcid.html

library(rorcid)
library(data.table)
library(tidyverse)
library(lubridate)

names <- fread("head -n -1 clean_data/NCMAS_2021-08-09.csv", 
               select = c("lead_ci", "institution")) %>% 
  as_tibble() %>% 
  separate(lead_ci, into = c("given_name", "last_name"), extra = "merge") %>% 
  mutate(
    institution = case_when(
      str_detect(institution, "NSW") ~ "University of New South Wales",
      TRUE ~ institution  ))

#Alexander
# Mikheyev
#rorcid::orcid_search(given_name = "Alexander",
#                      family_name = "Mikheyev")
# 
# rorcid::orcid_search(given_name = "David",
#                      family_name = "Edwards",
#                      affiliation_org = "University of Western Australia", 
#                      rows = 1)


#View(names[2:4, ])

orcid_recovered <- names %>% 
  mutate(orcid_recovered = purrr::pmap(.f = rorcid::orcid_search,
                                       .l = list(given_name = given_name, 
                                                 family_name = last_name, 
                                                 text = "Australia",
                                                rows = 1)
                                      )) %>% 
    unnest_wider(orcid_recovered)


View(orcid_recovered)

write_csv(orcid_recovered, 
          here::here(paste0("raw_data/orcids_NCMAS_", 
                            today(), ".csv")))


orcid_recovered_second <- orcid_recovered %>% 
  filter(is.na(orcid)) %>%  
  select(given_name, last_name, institution) %>% 
  mutate(orcid_second = purrr::pmap(.f = rorcid::orcid_search,
                                       .l = list(given_name = given_name,
                                                 family_name = last_name,
                                                 current_inst = institution,
                                                 rows = 1)))%>% 
  unnest_wider(col = orcid_second)

View(orcid_recovered_second)

write_csv(orcid_recovered_second, 
          here::here(paste0("raw_data/orcids_NCMAS_second_", 
                            today(), ".csv")))


orcid_recovered_third <- orcid_recovered_second %>% 
  filter(is.na(orcid)) %>%  
  select(given_name, last_name, institution) %>% 
  mutate(orcid_third = purrr::pmap(.f = rorcid::orcid_search,
                                    .l = list(given_name = given_name,
                                              family_name = last_name,
                                              text = institution,
                                              rows = 1)))%>% 
  unnest_wider(col = orcid_third)

View(orcid_recovered_third)

write_csv(orcid_recovered_third, 
          here::here(paste0("raw_data/orcids_NCMAS_third_", 
                            today(), ".csv")))

all_orcids_found <-
  rbind(orcid_recovered, orcid_recovered_second, orcid_recovered_third) %>% 
  filter(!is.na(orcid))

View(all_orcids_found)


write_csv(orcid_recovered_third, 
          here::here(paste0("clean_data/orcids_NCMAS_", 
                            today(), ".csv")))
