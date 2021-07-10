#' App expects the following environmental variables
#' base_id - from airtable
#' AIRTABLE_API_KEY - from airtable
#' SMTP_PASSWORD -- of outgoing mail (gmail)

library(dplyr)
library(magrittr)
library(airtabler)
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
      dplyr::select(-id, -`Created Date`)
    
    print("pulled sign up list")
    
    # check if testing
    if (test_run) { # only include test data
      signup.list %<>% dplyr::filter(grepl("fake", name, ignore.case = T))
    } else {
      # remove fake one
      signup.list %<>% dplyr::filter(!grepl("fake", name, ignore.case = T))
    }
    
    # add to a few 
    signup.list$num_convos[signup.list$name=="Jen Myhre"] <- 4
    
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
      
      pairs %<>%
        # replace record_ids with names from sign up list
        dplyr::left_join(select(signup.list, `Partner #1` = name, partner1 = record_id),
                         by = "partner1")  %>%
        dplyr::left_join(select(signup.list, `Partner #2` = name, partner2 = record_id),
                         by = "partner2") %>%
        dplyr::select(-partner1, -partner2)
      
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
#' Odd number of conversations   
#'
#' don't match with same partner more than once
#' 
#' don't forget about any scoobies who request to talk to a scoobie
#' 
#' Add step to review pairs on screen
#' before sending emails
#' 
#' Add option to Shiny app to use different URL (and/or csv?)
#' 
#' PRINT EMAIL CONFIRMATION TO SCREEN 
#' ---- could do this by printing updated table after sending
#' 
#' SOME WAY TO PREVENT MULTIPE SENDS ---- update records after sending and filter before sending a batch
#' 
#' Add "re-shuffle" button (after match, before sending)
#'  --- which will also involve setting a seed
#'  --- and creating a random column for scoobies / partner #2
#'  --- and sorting on that random column each time
#' 
#' Is there a way to remove an object -- remove the match table on button send email click and
#' replace with updated list with a check box in SENT column
#' 
#' Possible interest matching:
#' 1. Fuzzy string match OR
#' 2. First get count per response (split, tidy data, count)
#' 2.1 Sort by least frequent response
#' 2.2 Sort by that within newbie group_by... ? how do I match on it then