createPairs <- function(signup_list) {
  
  # record for each convo requested
  # https://github.com/mrdwab/splitstackshape
  convo.list <- expandRows(signup_list, count = "num_convos", drop = F)
  
  pairing.list <- convo.list %>%
    dplyr::group_by(record_id) %>%
    dplyr::mutate(rand_set = runif(1)) %>% # used to randomize within newbie/scoobie groups
    dplyr::arrange(preferance, engagement, rand_set)
  
  partner1.list <- pairing.list[1:(floor(nrow(pairing.list)/2)),
                                c("name", "preferance", "engagement")] %>%
    dplyr::rename("Partner #1" = "name", "Partner 1 Eng" = "engagement", "Partner 1 Pref" = "preferance")
    # randomize the order of participants, but keep participants in a chunk together
    
  
  # other half of the list, but inverted
  partner2.list <- pairing.list[nrow(pairing.list):((floor(nrow(pairing.list)/2))+1), 
                                c("name", "preferance", "engagement")] %>%
    dplyr::rename("Partner #2" = "name", "Partner 2 Eng" = "engagement", "Partner 2 Pref" = "preferance") %>%
    # create a new randomized order
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rand_set = runif(1)) %>%
    dplyr::arrange(rand_set) %>%
    dplyr::select(-rand_set)
  
  # combine left and right sides of the pairs lists
  pairs <- bind_cols(partner1.list, partner2.list)
  
  # check pair requests are met
  pairs %<>%
    dplyr::mutate(partner_1_req = `Partner 1 Pref` == `Partner 2 Eng` | `Partner 1 Pref`== "either",
                  partner_2_req = `Partner 2 Pref` == `Partner 1 Eng` | `Partner 2 Pref`== "either")
  
  print(paste0("Partner 1 request misses: ", sum(!pairs$partner_1_req)))
  print(paste0("Partner 2 request misses: ", sum(!pairs$partner_2_req)))
  
  return(pairs)
}

runAndCheckPairs <- function(signup_list = signup.list) {
  
  # check number of conversations
  print(paste0("sum convo count = ", sum(signup_list$num_convos)))
  
  # if odd number of conversations, drop one convo from someone who has large # requests
  if (sum(signup_list$num_convos) %% 2 != 0) {
    
    signup_list %<>%
      dplyr::arrange(desc(num_convos))
    
    signup_list$num_convos[1] <- signup_list$num_convos[1] - 1
    
  }
  
  # get initial pairs
  pairs <- createPairs(signup_list = signup_list)
  
  # if there are duplicates, re-run
  i <- 0
  while (sum(duplicated(pairs)) > 0 & i < 20) {
    
    pairs <- createPairs(signup_list = signup_list)
    print(paste0("New duplicates: ", sum(duplicated(select(pairs, -match_id)))))
    i + 1
    
    # check interests
    # pairs %>%
    #   dplyr::left_join(select(signup_list, name, interests), by = c("Partner #1" = "name")) %>%
    #   dplyr::left_join(select(signup_list, name, interests), by = c("Partner #2" = "name")) %>%
    #   dplyr::select(`Partner #1`, `Partner #2`, interests.x, interests.y) %>% View()
  }
  
  return(pairs)
  
}

createMatchEmail <- function(partner1, partner2, signup_list) {
  
  
  partner1_name <- partner1
  partner2_name <- partner2
  
  partner1_email <- signup_list$email[signup_list$name==partner1]
  partner2_email <- signup_list$email[signup_list$name==partner2]
  
  partner1_phone <- signup_list$phone[signup_list$name==partner1]
  partner2_phone <- signup_list$phone[signup_list$name==partner2]
  
  ## TO DO ##
  # Add interest into body of email?
  
  email <- compose_email(
    body = md(glue::glue(
      "Hi! 
          
      You've been matched for our July of 1:1s!
      
      <b>{partner1_name}</b> and <b>{partner2_name}</b>
          
      You can reply directly in this email (feel free to drop me from the CC!) or call or text:<br>
      {partner1_name}: {partner1_phone}<br>
      {partner2_name}: {partner2_phone}
      
      Reach out in the new week or so and try to schedule your conversation some time in July.
      
      Here are some <a href='https://docs.google.com/document/d/1G4xco-Q0Qn5-uCvn1V84BveQcqDB-5aNqwof5HB4STg/edit?usp=sharing'>questions</a> to get you thinking.
      
      You can use <a href='https://airtable.com/shrjyI1W0sOjUI2Iz'>this form</a> to share back some highlights.
      
      Have fun!")),
    footer = md(glue::glue("Showing Up for Racial Justice @ Sacred Heart<br>www.surjatsacredheart.org"))
  )
  
}

sendMatchEmails <- function(pairs, signup_list = signup.list) {
  
  # create unique id for each pair
  pairs %<>%
    dplyr::mutate(match_id = row_number())
  print("add match_ids")
  
  # send email for each match
  for (match in pairs$match_id) {
    
    partner1 = pairs$`Partner #1`[pairs$match_id==match]
    partner2 = pairs$`Partner #2`[pairs$match_id==match]
    
    email <- createMatchEmail(partner1, partner2, signup_list)
    print(paste("match email #", match))
    
    # send email
    email %>%
      smtp_send(
        # to = "lauren.renaud+test@gmail.com",
        to = c(signup_list$email[signup_list$name==partner1],
               signup_list$email[signup_list$name==partner2]),
        from = c("Lauren at SURJ" = "lauren.renaud@gmail.com"),
        subject = "Match for SURJ July of 1:1s",
        credentials = creds_envvar(
          user = "lauren.renaud@gmail.com",
          pass_envvar = "SMTP_PASSWORD",
          provider = "gmail",
          host = "smtp.gmail.com",
          port = 465, use_ssl = FALSE
        )
      )
    
    print(paste0("Email sent to ", partner1, " and ", partner2))
    
    updateAirtableSent(partner1, signup_list$record_id[signup_list$name==partner1],
                       partner2, signup_list$record_id[signup_list$name==partner2])
    
  }
}

updateAirtableSent <- function(partner1, partner1_record, 
                               partner2, partner2_record) {
  
  # set up body of update -- just match date
  record_data <- air_prepare_record(list(match_email_sent = as.character(Sys.Date())))
  json_record_data <- jsonlite::toJSON(list(fields = record_data))
  
  # update record for first partner  
  res <- httr::PATCH(
    url = paste0(Sys.getenv("base_api_url"), partner1_record),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("AIRTABLE_API_KEY")),
      `Content-type` = "application/json"),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  
  update_response <- air_parse(res)
  print(paste0("Email sent to ", partner1, " (with ", partner2, ")"))
  
  # update record for second partner  
  res <- httr::PATCH(
    url = paste0(Sys.getenv("base_api_url"), partner2_record),
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  
  update_response <- air_parse(res)
  print(paste0("Email sent to ", partner2, " (with ", partner1, ")"))
  
}