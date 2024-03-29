# user2019_scheduler - Global.R ################################################
# Author: Hallie Swan
# Date: 2019.06.11
# This script loads packages and functions, scrapes and processes data, and then
# prepares constants for the user2019_scheduler.

# TODO:
# - handle timezone - need to explicitly set to CEST in download
#    --> issue: google calendar event csv strips timezone off
#    --> so no longer has timezone
#    --> either need to conver times to user's local
#    --> or create ics file that includes timezone
# - document functions
# - move google calendar formatting to end
# - add icons for session, calendar, etc.
# - add bookmarking
# - add filter/add all events from a given session

# 0. load packages #############################################################
library(rvest)
library(tidyverse)
library(writexl)
library(lubridate)
library(shinyWidgets)
library(shiny)

# 1. source functions ##########################################################
source("functions.R", local = TRUE)

# 2. scrape and process data ###################################################
talks_df <- scrape_user_talks("http://www.user2019.fr/talk_schedule/",
                              paste0("201907", 10:12)) %>%
    prep_data_for_plot() %>%
    add_google_calendar_cols()

# 3. define constants ##########################################################
# conference days
conf_days <- unique(talks_df$day)
conf_days <- setNames(conf_days, strftime(conf_days, "%A, %B %d"))
# session colors
cats <- unique(talks_df$session_category)
bg_cols <- colorRampPalette(RColorBrewer::brewer.pal(12,"Set3"))(length(cats))
bg_cols <- setNames(c("black", bg_cols), c("! Your Calendar !", cats))
# hour breaks
hour_breaks <- seq(floor(min(talks_df$start_hour)), ceiling(max(talks_df$end_hour)), by = 1)
# columns for google calendar csv
cal_cols <- c("Subject", "Start Date", "Start Time",
              "End Date", "End Time", "All Day Event",
              "Description", "Location", "Private")
