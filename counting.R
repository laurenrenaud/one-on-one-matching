#' App expects the following environmental variables
#' base_id - from airtable
#' table_name - from airtable
#' AIRTABLE_API_KEY - from airtable
#' SMTP_PASSWORD -- of outgoing mail (gmail)
#' EMAIL_USER -- email address of outgoing mail

library(dplyr)
library(magrittr)
# https://rtable.john-coene.com/articles/classic.html
library(rtable)
source("setup.R")
source("helper_functions.R")

# pull in from airtable -----

# get 1:1 notes
setup(api_key = Sys.getenv("AIRTABLE_API_KEY"), 
      base = Sys.getenv("base_id"),
      table = "1:1 Notes")

get_setup()

notes <- records_to_tibble(list_records(filter = "{Status}='Done'")) %>%
  dplyr::select(record_id, Date, notes_taker = `Note Taker`, notes_with = `Notes With`) 

# get unique conversations ------
unique.pairs <- notes %>% 
  dplyr::select(notes_taker, notes_with) %>%
  dplyr::mutate_all(as.character) %>%
  uniqueConversationPairs()

print(paste("There were", nrow(unique.pairs), "one-on-ones"))

# if you want names -----

# get member list
reset_setup(base = FALSE, table = TRUE)
setup(api_key = Sys.getenv("AIRTABLE_API_KEY"), 
      base = Sys.getenv("base_id"),
      table = "Member List")

get_setup()

member.list <- records_to_tibble(list_records()) %>%
  dplyr::filter(record_id %in% unique.pairs$notes_taker |
                  record_id %in% unique.pairs$notes_with)

pairs.names <- unique.pairs %>%
  dplyr::left_join(select(member.list, notes_taker = record_id, f_name = `F Name`, l_name = `Last Name`),
                   by = "notes_taker") %>%
  dplyr::mutate(notetaker_name = paste(f_name, l_name)) %>%
  dplyr::select(-f_name, -l_name) %>%
  dplyr::left_join(select(member.list, notes_with = record_id, f_name = `F Name`, l_name = `Last Name`),
                   by = "notes_with") %>%
  dplyr::mutate(noteswith_name = paste(f_name, l_name)) %>%
  dplyr::select(notetaker_name, noteswith_name)
  