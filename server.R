#' App expects the following environmental variables
#' base_id - from airtable
#' AIRTABLE_API_KEY - from airtable
#' SMTP_PASSWORD -- of outgoing mail (gmail)

library(dplyr)
library(magrittr)
# library(airtabler)
source("airtabler.R")
library(lubridate)
library(glue)
library(blastula)
library(splitstackshape)
library(shiny)
library(httr)
library(jsonlite)
source("helper_functions.R")

test_run <- FALSE

if (test_run) {
  source("setup.R")
}

shinyServer(function(input, output, session) {
  
  # run matching ----
  observeEvent(input$run_matching, {
    
    # connect to airtable
    form_base <- airtable(
      base = Sys.getenv("base_id"), 
      tables = c("1:1 Sign Up")
    )
    print("connected to airtable")
    
    # pull up sign up list
    # only those who have not yet had an email sent
    signup.list <- form_base$`1:1 Sign Up`$select(filterByFormula = "{match_email_sent}=BLANK()") %>%
      dplyr::select(-id, -createdTime)
    
    print("pulled sign up list")
    
    # check if testing
    if (test_run) { # only include test data
      signup.list %<>% dplyr::filter(grepl("fake", name, ignore.case = T))
    } else {
      # remove fake one
      signup.list %<>% dplyr::filter(!grepl("fake", name, ignore.case = T))
    }
    
    # clean incoming list
    signup.list %<>%
      dplyr::group_by(name) %>%
      # if someone has signed up more than once, keep most recent
      #' *TODO**
      #' If we remove someone need to update Airtable
      #' so they aren't pulled in next batch
      dplyr::filter(submit_date == max(submit_date)) %>%
      # simplify from airtable verbose version
      # and order for use in creating pairs
      dplyr::mutate(preferance = case_when(grepl("newbie", preferance) ~ "newbie",
                                           grepl("around", preferance) ~ "scoobie",
                                           # if selected 'either' or missing
                                           TRUE ~ "either"),
                    engagement = case_when(grepl("newer", engagement) ~ "newbie",
                                           grepl("around", engagement) ~ "scoobie",
                                           # if missing, mark newbie
                                           TRUE ~ "newbie"),
                    preferance = ordered(preferance, c("scoobie", "either", "newbie")),
                    engagement = ordered(engagement, c("newbie", "scoobie", "either")),
                    num_convos = ifelse(is.na(num_convos), 1, as.numeric(num_convos)))
    
    # check there are any sign ups since last emails were sent
    if (nrow(signup.list) == 0 | is.null(signup.list)) {
      output$no_new_entries <- renderText({
        paste0("<p><h3>No new sign ups since the last round of matches</h3></p>")})
      
    } else {
      
      pairs <- runAndCheckPairs(signup_list = signup.list)
      
    }
    
    # check that pairs exist
    if (nrow(pairs) > 0) {
      print("checked pairs before rendering table")
      
      output$display_pairs <- renderTable({
        pairs
      }, striped = TRUE)
    }
    
  })
  
  # send emails on button push ----
  observeEvent(input$send_emails, {
    
    print("send emails button clicked")
    
    sendMatchEmails(pairs = pairs, signup_list = signup.list)
    
    # generate download buttons
    output$download_pairs <- downloadHandler(
      filename = function() {paste0("pairs_", Sys.Date(), '.csv')},
      content = function(con) {
        write.csv(pairs, row.names = F, con)
      }
    )
    
    output$emails_sent <- renderText({
      paste0("<p><h2>", nrow(pairs), " match emails sent!</h4></p>")})
    
  })
  
  # inputs ------
  
  output$run_matching <- renderUI({
    actionButton("run_matching", "Run Matches")
  })
  
  output$send_emails <- renderUI({
    actionButton("send_emails", "Send Match Emails")
  })
  
})

#' #### *TO DO* #######
#' 
#' Add option to Shiny app to use different URL (and/or csv?)
#' 
#' Is there a way to remove an object -- remove the match table on button send email click and
#' replace with updated list with a check box in SENT column
#' 
#' Possible interest matching:
#' 1. Fuzzy string match OR
#' 2. First get count per response (split, tidy data, count)
#' 2.1 Sort by least frequent response
#' 2.2 Sort by that within newbie group_by... ? how do I match on it then