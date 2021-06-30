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

test_run <- TRUE

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
      dplyr::select(-id, -createdTime, -`Created Date`)
    
    print("pulled sign up list")
    
    # check if testing
    if (test_run) { # only include test data
      signup.list %<>% dplyr::filter(grepl("fake", name, ignore.case = T))
    } else {
      # remove fake one
      signup.list %<>% dplyr::filter(!grepl("fake", name, ignore.case = T))
    }
    
    signup.list %<>%
      # record to be easier to use
      dplyr::mutate(preferance = case_when(grepl("newbie", preferance) ~ "newbie",
                                           grepl("around", preferance) ~ "scoobie",
                                           # if selected 'either' or missing
                                           TRUE ~ "either"),
                    engagement = case_when(grepl("newer", engagement) ~ "newbie",
                                           grepl("around", engagement) ~ "scoobie",
                                           # if missing, mark newbie
                                           TRUE ~ "newbie"),
                    preferance = ordered(preferance, c("newbie", "scoobie", "either")),
                    engagement = ordered(engagement, c("newbie", "scoobie")))
    
    # record for each convo requested
    # https://github.com/mrdwab/splitstackshape
    convo.list <- expandRows(signup.list, "num_convos", drop = F)
    
    # Split scoobies and newbies
    newbies <- dplyr::filter(convo.list, engagement == "newbie")
    
    others <- convo.list %>%
      dplyr::filter(engagement != "newbie",
                    preferance != "scoobie") %>%
      dplyr::select(name, record_id, preferance, engagement, num_convos) %>%
      # create a row number and sort on it to "shuffle" this list
      dplyr::group_by(name) %>%
      dplyr::mutate(convocount = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(convocount, name)
    
    # Check newbie / scoobie balance 
    newbie_scoobie_balance <- nrow(newbies) - nrow(others)
    print("check pair balance")
    
    # check there are sign ups
    if (nrow(signup.list) == 0 | is.null(signup.list)) {
      output$need_scoobies <- renderText({
        paste0("<p><h3>No new sign up since the last round of matches</h3></p>")})
      
    } else if (newbie_scoobie_balance == 0) {
      # if perfect balance of newbies and scoobies, 
      # match scoobies with newbies
      
      pairs <- tibble(partner1 = newbies$record_id,
                      partner2 = others$record_id) %>%
        dplyr::mutate(match_id = row_number())
      
    } else if (newbie_scoobie_balance < 0) {
      # if more scoobies than newbies
      
      # first match newbies
      newbie.pairs <- tibble(partner1 = newbies$record_id,
                             partner2 = others$record_id[1:nrow(newbies)]) 
      
      # get remaining scoobies
      others.to.pair <- others[(nrow(newbies) + 1):nrow(others),] %>%
        # arrange by record_id to avoid matching to self
        dplyr::arrange(record_id) 
      
      other.pairs <- tibble(
        # now that the list is sorted by record_id
        # put first half of list in partner1 column
        partner1 = others.to.pair[1:(nrow(others.to.pair)/2),]$record_id,
        # second half of list in partner2 column
        partner2 = others.to.pair[((nrow(others.to.pair)/2)+1):nrow(others.to.pair),]$record_id)
      
      # combine lists
      pairs <- dplyr::bind_rows(newbie.pairs, other.pairs) %>%
        unique()
      
      rm(other.pairs)
      rm(others.to.pair)
      rm(newbie.pairs)
      
    } else {
      
      output$need_scoobies <- renderText({
        paste0("<p><h4>We need ", newbie_scoobie_balance,
               " more scoobie conversation partners.</h4></p>
               <p>Add some folks on<a href='https://airtable.com/shrAvctlUGyUVSVCu'>this form</a>
               and then re-run the matcher.</p>")})
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

#### *TO DO* #######
#' what to do if too many newbies, not enough scoobies
#' could send messsage to get more scoobies!
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