# send match email to both at same time
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
    
    partner1 = pairs$partner1[pairs$match_id==match]
    partner2 = pairs$partner2[pairs$match_id==match]
    
    email <- createMatchEmail(partner1, partner2, signup_list)
    print(paste("match email #", match))
    
    # send email
    email %>%
      smtp_send(
        to = "lauren.renaud+test@gmail.com",
        # to = c(signup.list$email[signup.list$name==partner1],
        #        signup.list$email[signup.list$name==partner2]),
        from = c("Lauren at SURJ" = "lauren.renaud@gmail.com"),
        subject = "Match for SURJ July of 1:1s",
        # credentials = creds_file("gmail_creds")
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

updateAirtableSent <- function(partner1, parter1_record, 
                               partner2, parter2_record) {
  
  # set up body of update -- just match date
  record_data <- air_prepare_record(list(match_email_sent = as.character(Sys.Date())))
  json_record_data <- jsonlite::toJSON(list(fields = record_data))
  
  # update record for first partner  
  res <- httr::PATCH(
    url = paste0(Sys.getenv("base_api_url"), parter1_record),
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  
  update_response <- air_parse(res)
  print(paste0("Email sent to ", partner1, " (with ", partner2, ")"))
  
  # update record for second partner  
  res <- httr::PATCH(
    url = paste0(Sys.getenv("base_api_url"), parter2_record),
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"),
    body = json_record_data
  )
  
  air_validate(res)  # throws exception (stop) if error
  
  update_response <- air_parse(res)
  print(paste0("Email sent to ", partner2, " (with ", partner1, ")"))
  
}