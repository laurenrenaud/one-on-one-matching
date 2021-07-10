library(shiny)
# if using datatables, need to include library in ui.R too
library(DT)

# ui.R
htmlTemplate("template.html",
             # text
             no_new_entries = htmlOutput("no_new_entries"),
             emails_sent = htmlOutput("emails_sent"),
             
             # display tables
             display_pairs = tableOutput("display_pairs"),
             
             # buttons
             run_matching = uiOutput("run_matching"),
             download_pairs = downloadButton("download_pairs", "Download Pair List"),
             send_emails = uiOutput("send_emails")
)
