# Open Access PDFs were collected using Zotero.
# I've downloaded 13 Gb of PDFs all open access



# PDFs are stored at ~/Zotero/storage
# shell
# find -iname '*.pdf' > list_PDFs.txt
# copy this list into the directory PDFnames

# Date: 2021-09-28
# Author: Paula Andrea Martinez


# 1. Load the libraries ----
library(tidyverse)
library(lubridate)
library(janitor)

# 2. read data ----
raw_names  <- read_table("PDFnames/list_PDFs.txt", 
                         col_names = FALSE) %>% 
  separate(col = X1, into = c("home", "dir_name", "full_name"),
           sep = "([/])") %>% 
  separate(col = full_name, into = c("author", "year", 
                                     "title"),
           sep = "( \\- )") %>% 
  filter(!is.na(title)) %>% 
  filter(year >= 2010) %>% 
  mutate( title = str_remove(title, "(^)*\\.pdf")) %>% 
  arrange(year, author) %>% 
  mutate(title = str_to_title(title))

write_csv(raw_names, file = paste0(
  "PDFnames/PDF_names_", today(), ".csv"))
