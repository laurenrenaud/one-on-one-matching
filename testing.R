# https://rtable.john-coene.com/articles/classic.html

# set up creds
setup(api_key = Sys.getenv("RTABLE_API_KEY"), 
      base = Sys.getenv("base_id"),
      table = "1:1 Sign Up")

# confirm
get_setup()

# pull table
signup.list <- records_to_tibble(list_records(view = "Responses",
                                              filter = "{match_email_sent}=BLANK()")) %>%
  dplyr::select(-record_id_displayed, -record_created_time)

# update record


# https://github.com/bergant/airtabler

# new_hotel <- 
airtable$`1:1 Sign Up`$update(
  record_id = test_record_id, 
  record_data = list(
    match_email_sent = as.character(Sys.Date())
  )
)

new_date <- form_base$`1:1 Sign Up`$update(
  record_id = test_record_id, 
  record_data = list(
    match_email_sent = as.character(Sys.Date())
  )
)

airtable$`1:1 Sign Up`$update(
  record_id = pairs$partner1[pairs$match_id==match], 
  record_data = list(
    match_email_sent = as.character(Sys.Date())
  )
)

air_update(form_base, "1:1 Sign Up", 
           record_id = test_record_id, 
           record_data = list(
             record_id = test_record_id,
             match_email_sent = as.character(Sys.Date())
           ))

record_data = list(
  # record_id = "recdJuFUkvseuFn7P",
  match_email_sent = as.character(Sys.Date())
)
record_data <- air_prepare_record(record_data)
json_record_data <- jsonlite::toJSON(list(fields = record_data))

# request_url <- sprintf("%s/%s/%s/%s", air_url, form_base, "1:1 Sign Up", record_id = test_record_id)
request_url <- sprintf("%s/%s/%s/%s", air_url, base = Sys.getenv("base_id"), "1:1 Sign Up",
                       record_id = "recdJuFUkvseuFn7P")
                       # record_id = record_id)

# call service:
res <- httr::PATCH(
  # request_url,
  "https://api.airtable.com/v0/appcrRVE9ITTWI9dW/1%3A1%20Sign%20Up/recdJuFUkvseuFn7P",
  httr::add_headers(
    Authorization = paste("Bearer", air_api_key()),
    `Content-type` = "application/json"
  ),
  body = json_record_data
)

air_validate(res)  # throws exception (stop) if error
air_parse(res)     # returns R object

