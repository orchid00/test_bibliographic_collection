library(tidyverse)
library(lubridate)
library(rorcid)
library(purrr)

load("big_table_orcids.RData")

big_table <- big_table_test_five_orcids %>% 
  filter(!is.na(external_id_normalized_value)) %>% 
  distinct(external_id_normalized_value, .keep_all = TRUE) %>%
  mutate(path = str_remove(path, "(/)")) %>% 
  separate(col = path, into = c("orcid", "works"), sep = "/work/")


View(big_table)

counts_works <- big_table %>% 
  group_by(orcid) %>% 
  count() %>% 
  arrange(desc(n)) 

counts_works$n %>% 
  as.numeric() %>% 
  summary()
