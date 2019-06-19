# useR! 2019 scheduleR

The useR! 2019 conference is scheduled for July 9-12 in Toulouse, France. A schedule of talks was recently released on the [conference website](http://www.user2019.fr/talk_schedule/) in a table format. The goal of this app is to make selecting talks and constructing a reasonable schedule easier.  

The user can add events to their calendar and then download a csv of the events. The user can then import the events to their Google Calendar.

# Open the scheduler online

The scheduler is hosted here: [https://hallietheswan.com/shiny/sample-apps/user_2019_scheduler/](https://hallietheswan.com/shiny/sample-apps/user_2019_scheduler/).
Note: this site is not set up to handle much traffic and may be down.

# Open the scheduler locally

1. Clone the repo.
2. Open RProject.
3. Enter `shiny::runApp()` in console.
4. App will open.

# Add Events to Your Google Calendar

1. Open the scheduler. Events are shown with various colors by session category (e.g. Shiny).
2. Click on an event -- a modal will open.
3. Click the green check at the bottom of the modal -- a success alert will display.
4. The event will now appear in black/white on the calendar.
5. Click download your calendar -- a csv containing events you added will be downloaded.
6. Open Google Calendar.
7. Click + next to "Other calendars", then select "Create new calendar".
8. Add the following information then click "Create calendar"
    - Name: useR! 2019 - Toulouse
    - Description: talks for useR! 2019
    - Time zone: (GMT+02:00) Central European Time - Paris
9. Click "Import & export". Under import:
    - Select file downloaded in step 5.
    - Change calendar to "useR! 2019 - Toulouse"
10. Click "Import" -- you should see an alert that says "x out of x events added."
11. Return to your calendar. You should see the events added. They will be displayed using your current timezone, but should be the correct time when you are in Toulouse.

# Suggestions/Improvements/Comments

Please don't hesitate to contact me!

