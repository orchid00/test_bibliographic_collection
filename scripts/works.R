#works
library(tidyverse)
library(lubridate)
library(rorcid)
library(purrr)

get_works_from_orcid <- function(orcid_digits) {
    #first collect data
    #orcid_digits <- "0000-0002-2189-7780"
    full_works <- rorcid::works(orcid_digits) %>%
    as_tibble() %>%
    janitor::clean_names()
    #check
    print(paste("orcid: ", orcid_digits))
    print(paste("dimensions: ", dim(full_works)))
 
    if(nrow(full_works)> 1){
     
      one_person_works <- full_works  %>% 
      filter(type == "journal-article") %>% 
      #path = orcid, "/work/", put_code
      select(put_code, path, source_source_name_value,
             title_title_value, publication_date_year_value, 
             journal_title_value, external_ids_external_id) %>% 
      distinct(title_title_value, publication_date_year_value, .keep_all = TRUE) %>% 
      tidyr::unnest(external_ids_external_id, keep_empty = TRUE) %>%
      janitor::clean_names()
      
      if("external_id_type" %in% names(one_person_works)){
        one_person_works <- one_person_works %>% 
        filter(external_id_type %in% c("doi", NA)) %>% 
        select(put_code, path,                        
               source_source_name_value, title_title_value,           
               publication_date_year_value, 
               journal_title_value, external_id_type,           
               external_id_value,
               external_id_normalized_value)
      }
      else
      {
        one_person_works <- one_person_works %>% 
              select(-external_ids_external_id) %>% 
              mutate(external_id_type= NA,            
          external_id_value= NA, 
          external_id_normalized_value = NA)
      }
    }
    else
    {
      one_person_works <- tibble(put_code = NA, 
             path = paste0("/", orcid_digits, "/work/"),                        
                    source_source_name_value= NA, 
                    title_title_value= NA,
                    publication_date_year_value= NA,  
                    journal_title_value= NA,  external_id_type= NA,            
                    external_id_value= NA, 
                    external_id_normalized_value = NA)
    }
    print(paste("dimensions clean: ", dim(one_person_works)))
    return(one_person_works)
}

# test <- get_works_from_orcid("0000-0002-2189-7780")
# test2 <- get_works_from_orcid("0000-0002-9403-9779")
# 
# names(test)
# names(test2)

# five_orcids <- c("0000-0002-2189-7780",
#                  "0000-0002-6508-7480",
#                  "0000-0003-4369-1019",
#                  "0000-0001-8318-9408",
#                  "0000-0002-9403-9779")
# 
# test_fiveorcids <- purrr::map_dfr(.x = five_orcids, 
#                                   .f =  get_works_from_orcid)
  
  
all_data <- read_csv("clean_data/all_orcids_projects_NCMAS_2021-08-16.csv")

orcids_all <- tibble(orcid = all_data$orcid) %>% 
  filter(!is.na(orcid)) 



big_table_orcids <- purrr::map_dfr(.x = orcids_all$orcid, 
                                     .f =  get_works_from_orcid)

save(big_table_orcids, file = "big_table_orcids.RData")
#compressed save
con <- pipe("pigz -p8 > big_table_orcids.gz", "wb")
save(big_table_orcids, file = con)
close(con)
