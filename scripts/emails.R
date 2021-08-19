#emails

library(tidyverse)
library(lubridate)
library(rorcid)
library(purrr)

# source https://ciakovx.github.io/rorcid.html#Getting_data_on_multiple_people_with_orcid_person()

orcid_digits <- "0000-0001-6978-9765"

get_emails_from_orcid <- function(orcid_digits){
  
  test1 <- orcid_email(orcid_digits)
  if(is.list(test1)) {
    if(length(test1[[1]][[2]]$email) > 0 ) {
      test1_2 <- tibble(orcid = names(test1),
                      x = test1[[1]][[2]]$email)
      test1_3 <- test1_2 %>% 
        mutate(email = paste0("email_", row_number())) %>% 
        pivot_wider(names_from = email,
                    values_from = x)
      return(tibble(test1_3))
    }
  }
  else{
    tibble(test1)
  }
}


the_email <- get_emails_from_orcid("0000-0001-6978-9765")
the_email <- get_emails_from_orcid("0000-0002-2189-7780")
the_email <- get_emails_from_orcid("0000-0002-9403-9779")
 
five_orcids <- c("0000-0002-2189-7780",
                 "0000-0002-6508-7480",
                 "0000-0001-6978-9765",
                 "0000-0001-9792-4143",
                 "0000-0002-9403-9779")

test_five_emails <- purrr::map_dfr(.x = five_orcids,
                                  .f =  get_emails_from_orcid)

all_data <- read_csv("clean_data/all_orcids_projects_NCMAS_2021-08-16.csv")
 
orcids_all <- tibble(orcid = all_data$orcid) %>% 
   filter(!is.na(orcid)) 

orcids_email <- purrr::map_dfr(.x = orcids_all$orcid,
                               .f =  get_emails_from_orcid)

            
# get_emails_from_orcid <- function(orcid_digits) {
#   #first collect data
#   orcid_digits <- "0000-0002-2189-7780"
#   my_orcid_person <- rorcid::orcid_person(orcid_digits)
#   
#   my_orcid_person_data <- my_orcid_person %>% {
#     dplyr::tibble(
#       created_date = purrr::map_dbl(., purrr::pluck, 
#                                     "name", "created-date", "value", 
#                                     .default = NA_integer_),
#       given_name = purrr::map_chr(., purrr::pluck, 
#                                   "name", "given-names", "value", 
#                                   .default = NA_character_),
#       family_name = purrr::map_chr(., purrr::pluck, 
#                                    "name", "family-name", "value", 
#                                    .default = NA_character_),
#       credit_name = purrr::map_chr(., purrr::pluck, 
#                                    "name", "credit-name", "value", 
#                                    .default = NA_character_),
#       orcid_identifier_path = purrr::map_chr(., purrr::pluck, 
#                                    "name", "path", .default = NA_character_),
#       biography = purrr::map_chr(., purrr::pluck, 
#                                  "biography", "content", .default=NA_character_),
#       researcher_urls = purrr::map(., purrr::pluck, 
#                                    "researcher-urls", "researcher-url", 
#                                    .default = NA_character_),
#       emails = purrr::map(., purrr::pluck, 
#                           "emails", "email", "email", 
#                           .default = NA_character_),
#       keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", 
#                             .default = NA_character_)
#     )} %>%
#     dplyr::mutate(created_date = anytime::anydate(created_date/1000))
#   
#   #check
#   print(paste("orcid: ", orcid_digits))
#   
#   if(nrow(my_orcid_person_data)< 1){
#     my_orcid_person_data <- tibble(created_date,         
#                                    given_name,           
#                                    family_name,          
#                                    credit_name,          
#                                    other_names,          
#                                    orcid_identifier_path,
#                                    biography,            
#                                    researcher_urls,      
#                                    emails,               
#                                    external_ids)
#     }
#   print(paste("dimensions: ", dim(my_orcid_person_data)))
#   return(my_orcid_person_data)
#   
# }
# 
# # test <- get_emails_from_orcid("0000-0002-2189-7780")
# # test2 <- get_emails_from_orcid("0000-0002-9403-9779")
# # names(test)
# # names(test2)
# # 
# # five_orcids <- c("0000-0002-2189-7780",
# #                  "0000-0002-6508-7480",
# #                  "0000-0003-4369-1019",
# #                  "0000-0001-8318-9408",
# #                  "0000-0002-9403-9779")
# # 
# # test_fiveorcids <- get_emails_from_orcid(five_orcids)
# 
# all_data <- read_csv("clean_data/all_orcids_projects_NCMAS_2021-08-16.csv")
# 
# orcids_all <- tibble(orcid = all_data$orcid) %>% 
#   filter(!is.na(orcid)) 
# 
# 
# 
# orcids_email <- get_emails_from_orcid(orcids_all$orcid)
# 
# orcids_email_only <- orcids_email %>% 
#   filter(!is.na(emails)) %>% 
#   select(contains("date"), contains("name"), emails) %>% 
#   mutate(emails = str_remove(emails, "[c(]"))
#   # extract(emails, c("A", "B"),
#   #         regex = "([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))
#   #                  ([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))"))
# 
# # save(big_table_orcids, file = "big_table_orcids.RData")
# # #compressed save
# # con <- pipe("pigz -p8 > big_table_orcids.gz", "wb")
# # save(big_table_orcids, file = con)
# # close(con)