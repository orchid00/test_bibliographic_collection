library(rvest)
library(tidyverse)
library(janitor)
library(lubridate)
#' NCMAS
#'
#' @inheritParams 
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' get_NCMAS_summary()
get_NCMAS_summary <- function() {
  url <- sprintf("https://ncmas.nci.org.au/2020/outcomes")
  resp <- httr::GET(url)
  
  httr::stop_for_status(resp)
  
  my_table <- read_html(url) %>%
    html_element("tbody") %>%
    html_table(header = TRUE)
  
  write_csv(my_table, 
            here::here(paste0("raw_data/NCMAS_", 
                                  today(), ".csv")))
  
  clean_table <- my_table %>% 
    janitor::clean_names()
  
  write_csv(clean_table, 
            here::here(paste0("clean_data/NCMAS_", 
                              today(), ".csv")))
  
  #View(clean_table)  
  
  # KSU per institution
  clean_table %>% 
    group_by(institution) %>% 
    summarise(allocation_institution = sum(total_allocation_ksu)) %>% 
    arrange(desc(allocation_institution)) %>% 
    print(n = Inf)
  
  # leads per institution
  clean_table %>% 
    group_by(institution, lead_ci) %>% 
    count(institution, sort = TRUE) %>% 
    print(n = Inf)
  
}

