# user2019_scheduler - functions.R #############################################
# Author: Hallie Swan
# Date: 2019.06.11
# This script contains helper functions for the user2019_scheduler.

# scrape_process_data.R ########################################################

scrape_user_talks <- function(url, days) {
    talks_page <- html(url)
    lapply(days, function(day) {
        talks_page %>%
            html_node(paste0("#schedule", day)) %>%
            html_table() %>%
            rename(description = "") %>%
            rename_all(tolower) %>%
            mutate(description = lead(description)) %>%
            filter(rep(c(TRUE, FALSE), times = nrow(.)/2)) %>%
            mutate(start_time = time,
                   day = as.POSIXct(day, format = "%Y%m%d", tz = "Europe/Paris")) %>%
            arrange(day, room, start_time) %>%
            group_by(day, room) %>%
            mutate(end_time = lead(start_time),
                   end_time = case_when(
                       !is.na(end_time) ~ end_time,
                       is.na(end_time) & session == "Keynote" ~ "17:00",
                       is.na(end_time) ~ "15:30"
                   )) %>%
            mutate_at(vars(ends_with("_time")), 
                      list(~as.POSIXct(paste0(day, .), 
                                       format = "%Y-%m-%d%H:%M", 
                                       tz = "Europe/Paris"))) %>%
            mutate(
                # give some time for a passing period
                # time needs to be in seconds
                end_time = end_time - 0.5*60,
                duration = difftime(end_time, start_time, "mins")) %>%
            select(day, start_time, end_time, duration, title, description, everything()) %>%
            arrange(day, time) %>% 
            # remove blank events
            filter(title != "")
    }) %>% 
        bind_rows() 
}

prep_data_for_plot <- function(df) {
    df %>%
        rowwise() %>%
        mutate(
            nchar_print = 25 * as.numeric(duration) / 10,
            nchar_title = nchar(title),
            nchar = ifelse(nchar_title <= nchar_print, nchar_title, nchar_print),
            label = str_trunc(title, nchar), 
            start_hour = hour(start_time) + minute(start_time)/60,
            end_hour = hour(end_time) + minute(end_time)/60) %>%
        ungroup() %>%
        mutate(event_id = 1:n(),
               session_category = trimws(gsub("[[:digit:]]+", "", session)))
}

bold <- function(val) paste0("<b>", val, "</b>")

# https://support.google.com/calendar/answer/37118?hl=en
# cols: 
#   Subject: name of the event, required
#   Start Date: first day of the event, required
#   Start Time: time the event begins
#   End Date: last day of the event
#   End Time: time the event ends
#   All Day Event: True, False
#   Description: description or notes about the event
#   Location: the location for the event
#   Private: True, False
# how to handle timezone???
add_google_calendar_cols <- function(df) {
    df %>% 
        ungroup() %>%
        mutate(Subject = title,
               `Start Date` = day,
               `Start Time` = substr(start_time, 12, 16),
               `End Date` = day,
               `End Time` = substr(end_time, 12, 16),
               `All Day Event` = "False",
               Description = paste0(
                   bold("Session: "), session, "<br>",
                   bold("Speaker: "), speaker, "<br>",
                   bold("Chair: "), chair, "<br>",
                   bold("Slides: "), slides, "<br>",
                   description, "<br>"
               ),
               Description = gsub("\n", "<br><br>", Description),
               Location = paste0(room, ", Centre de Congrès Pierre Baudis, Toulouse, France"),
               Private = "False") %>%
        filter(!is.na(Subject) & Subject != "", !is.na(`Start Date`))
}

# server.R #####################################################################

# calendar formatting from: https://stackoverflow.com/a/52487803/8099834
create_calendar_plot <- function(df) {
    df <- df %>%
        mutate(session_category = ifelse(user_cal, "! Your Calendar !", session_category),
               text_color = ifelse(user_cal, "white", "black"))
    df %>%
        # create plot
        ggplot(aes(y = start_hour, yend = end_hour, x = 0, xend = 0,
                   text = Description)) +
        facet_grid(.~room, drop = FALSE) +
        geom_segment(size = 65, aes(color = session_category)) +
        geom_text(aes(label = str_wrap(label, 25), 
                      y = (end_hour - start_hour)/2 + start_hour),
                  color = df$text_color,
                  size = 4) +
        theme_minimal() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 15),
              axis.title = element_text(size = 15),
              legend.text = element_text(size = 13),
              legend.title = element_blank(),
              legend.position = "bottom",
              strip.text = element_text(size = 15),
              panel.grid = element_blank(),
              panel.grid.major.y = element_line(color = "black")) +
        labs(x = NULL, y = "Hour CEST") +
        guides(color = guide_legend(override.aes = list(size=5),
                                    nrow = 2, byrow = TRUE)) +
        scale_y_reverse(breaks = hour_breaks,
                        labels = paste0(hour_breaks, ":00"),
                        limits = c(max(hour_breaks), min(hour_breaks))) +
        scale_color_manual(values = bg_cols)
}

create_event_modal <- function(click_df) {
    
    # get constants
    search_url <- paste0("https://maps.google.com/?q=", gsub(" ", "+", click_df$Location))
    event_toggle <- ifelse(!click_df$user_cal, "check-circle", "times-circle")
    event_toggle_text <- ifelse(!click_df$user_cal, "add event to", "remove event from")
    
    # build modal
    modalDialog(
        div(class = "right-align", 
            actionButton("close_modal", "", icon = icon("times"))),
        h3(click_df$title),
        div(icon("calendar"),
            strftime(click_df$start_time, "%A, %B %d %H:%M"), " - ", 
            strftime(click_df$end_time, "%H:%M"), " CEST", br()),
        div(icon("map-marker-alt"), 
            a(click_df$Location, href = search_url, target = "_blank")), 
        hr(),
        HTML(click_df$Description),
        div(align = "center", 
            actionButton("change_event", "", icon = icon(event_toggle, "fa-5x")),
            helpText("Click to ", event_toggle_text, " your calendar.")),
        easyClose = TRUE,
        footer = NULL)
}

#' Get Alert Values
#' 
#' This function will return a list of values that make up the content of a 
#' sweet alert. 
#'
#' @param add_rem boolean, if TRUE, will create values indicating successful 
#' addition of event to calendar. if FALSE, will create values indicating 
#' successful removal of event from calendar.
#'
#' @return list
#' @export
#'
#' @examples
#' get_alert_vals(TRUE)
#' get_alert_vals(FALSE)
get_alert_vals <- function(add_rem) {
    alert_vals <- list()
    alert_vals$title <- ifelse(!add_rem, "Saved!", "Removed!")
    alert_vals$text <- ifelse(!add_rem, "Event saved to your calendar.", "Event removed from your calendar.")
    alert_vals$type <- ifelse(!add_rem, "success", "error")
    alert_vals
}

# ui.R #########################################################################
ui_create_header <- function() {
    fluidRow(
        column(width = 12, 
               h1("Build Calendar for useR! 2019 - Toulouse"),
               helpText(
                   icon("github"),
                   "Built by: ", 
                   a("Hallie Swan", 
                     href = "https://github.com/hallieswan/", target = "_blank"), 
                   HTML("&emsp;&bull;&emsp;"),
                   icon("code-branch"),
                   a("Fork me", 
                     href = "https://github.com/hallieswan/?tab=repositories",
                     target = "_blank"),
                   HTML("&emsp;&bull;&emsp;"),
                   icon("table"), 
                   "See original data: ", 
                   a("http://www.user2019.fr/talk_schedule/", 
                     href = "http://www.user2019.fr/talk_schedule/",
                     target = "_blank")
               )
               )
    )
}
ui_create_controls <- function() {
    fluidRow(
        column(width = 4, 
               helpText("Toggle between all events and events you have added ", 
                        "to your calendar. Add events by clicking on the event, ",
                        "and then clicking the check in the upper-right corner of the modal."),
               switchInput("calendar_toggle", value = TRUE, 
                           onLabel = "View all events", offLabel = "View my events",
                           width = 400)),
        column(width = 4, 
               helpText("Select a conference date for which you would like to view events."),
               selectInput("date", "Conference Date", choices = conf_days)),
        column(width = 4,
               helpText("Add events to Google Calendar by downloading a csv of events", 
                        "and then following instructions here:",
                        a("https://support.google.com/calendar/answer/37118",
                          href = "https://support.google.com/calendar/answer/37118", target = "_blank")),
               downloadButton("dl_your_cal", "Your Events"),
               downloadButton("dl_full_cal", "All Events"))
    )
}
ui_create_calendar <- function() {
    fluidRow(
        column(width = 12,
               plotOutput("calendar", width = 1200, height = 1000, 
                          click = "calendar_click")
        )
    )
}