# user2019_scheduler - server.R ################################################
# Author: Hallie Swan
# Date: 2019.06.11
# This script contains the server for the user2019_scheduler.

function(input, output, session) {
    
    # user's calendar ##########################################################
    user_talks <- reactiveValues()
    user_talks$cal <- talks_df %>%
        mutate(user_cal = FALSE,
               room = as.factor(room))
    
    # calendar plot ############################################################
    output$calendar <- renderPlot({
        
        df <- user_talks$cal %>%
            filter(day == as.Date(input$date))
        
        if(!input$calendar_toggle) {
            df <- df %>%
                filter(user_cal)
        }
        
        validate(need(nrow(df) > 0, message = "No events added to your calendar on this day."))
        
        create_calendar_plot(df)
    })
    
    # clicked event modal ######################################################
    # ...and store event_id
    observeEvent(input$calendar_click, {
        if(!is.null(input$calendar_click)) {
            click <- input$calendar_click
            click_event <- user_talks$cal %>%
                filter(room == click$panelvar1 &
                           start_hour < click$y & end_hour > click$y &
                           day == as.Date(input$date))
            if(nrow(click_event) > 0) {
                # add clicked event to reactive vals
                user_talks$clicked_event_id <- click_event$event_id
                
                # build modal
                showModal(create_event_modal(click_event))
            }
        }
    })
    
    # close modal ##############################################################
    observeEvent(input$close_modal, {
        if(!is.null(input$close_modal) & input$close_modal > 0) {
            removeModal()
        }
    })
    
    # add/remove event to/from user's calendar #################################
    # ..and show success modal
    observeEvent(input$change_event, {
        if(!is.null(input$change_event) && input$change_event > 0) {
            
            user_cal <- user_talks$cal %>%
                filter(event_id == user_talks$clicked_event_id) %>%
                pull(user_cal)
            
            user_talks$cal <- user_talks$cal %>%
                mutate(user_cal = ifelse(event_id == user_talks$clicked_event_id, !user_cal, user_cal))
            
            alert_vals <- get_alert_vals(user_cal)
            removeModal()
            sendSweetAlert(
                session = session,
                title = alert_vals$title,
                text = alert_vals$text,
                type = alert_vals$type
            )
        }
    })
    
    # download calendars #######################################################
    # ...your calendar
    output$dl_your_cal <- downloadHandler(
        filename = function() {
            "user2019_full_calendar.csv"
        },
        content = function(file) {
            df <- user_talks$cal %>%
                filter(user_cal) %>%
                select(cal_cols)
            write_csv(df, file) 
        }
    )
    # ...full calendar
    output$dl_full_cal <- downloadHandler(
        filename = function() {
            "user2019_your_calendar.csv"
        },
        content = function(file) {
            df <- select(user_talks$cal, cal_cols)
            write_csv(df, file) 
        }
    )
    
}