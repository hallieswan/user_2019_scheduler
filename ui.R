# user2019_scheduler - ui.R ####################################################
# Author: Hallie Swan
# Date: 2019.06.11
# This script contains the ui for the user2019_scheduler.

fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "user2019_scheduler.css")
    ),
    ui_create_header(),
    ui_create_controls(),
    ui_create_calendar()
)