# Merge results
library("tidyverse")
library("lubridate")


data1 <- read_csv("raw_data/orcids_NCMAS_2021-08-09.csv")
data2 <- read_csv("raw_data/orcids_NCMAS_second_2021-08-09.csv")
data3 <- read_csv("raw_data/orcids_NCMAS_third_2021-08-09.csv")

total_data <- read_csv("clean_data/NCMAS_2021-08-09.csv")

# #Are there duplicates in total?
# test_duplicated <-
# total_data %>% select(`Lead CI`) %>% 
#   group_by(`Lead CI`) %>% 
#   filter(n()>1)

all_orcids <- bind_rows(data1, data2, data3) %>% 
  filter(!is.na(orcid)) 

write_csv(all_orcids, 
          here::here(paste0("clean_data/all_orcids_NCMAS_", 
                            today(), ".csv")))

all_orcids_lead <- all_orcids %>% 
  unite(col = "lead_ci", given_name:last_name, sep = " ")

all_data <- total_data %>% 
  select(project_code:project_title) %>%
  filter(project_code != "TOTALS (KSU)") %>% 
  left_join(all_orcids_lead, by = ("lead_ci" = "lead_ci")) %>% 
  mutate(
    institution = case_when(
      str_detect(institution.x, "NSW") ~ "University of New South Wales",
      is.na(institution.y) ~institution.x,
      TRUE ~ institution.x  )) %>% 
  select(-institution.x, - institution.y) %>% 
  arrange(institution, lead_ci) %>% 
  select(orcid, lead_ci, first, last, institution,
          project_code, project_title)

write_csv(all_data, 
          here::here(paste0("clean_data/all_orcids_projects_NCMAS_", 
                            today(), ".csv")))
