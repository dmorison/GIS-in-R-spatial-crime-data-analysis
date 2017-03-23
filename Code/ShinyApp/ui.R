library(shiny)

# monthDates <- c("2016-01","2016-02","2016-03","2016-04","2016-05","2016-06",
#                 "2016-07","2016-08","2016-09","2016-10","2016-11","2016-12")
# monthNames <- c("January","February","March","April","May","June",
#                 "July","August","September","October","November","December")

monthChoices <- list("January" = "2016-01", "February" = "2016-02", "March" = "2016-03",
                     "April" = "2016-04", "May" = "2016-05", "June" = "2016-06",
                     "July" = "2016-07", "August" = "2016-08", "September" = "2016-09",
                     "October" = "2016-10", "November" = "2016-11", "December" = "2016-12")

fluidPage(
  
  sidebarPanel(
    selectInput("month", "Select a month:",
                choices = monthChoices,
                selected = monthChoices[1]
    ),
    checkboxInput("Anti-social_behaviour", "Anti-social behaviour", FALSE),
    checkboxInput("Bicycle_theft", "Bicycle theft", TRUE),
    checkboxInput("Burglary", "Burglary", FALSE),
    checkboxInput("Criminal_damage_and_arson", "Criminal damage and arson", FALSE),
    checkboxInput("Drugs", "Drugs", FALSE),
    checkboxInput("Other_crime", "Other crime", FALSE),
    checkboxInput("Other_theft", "Other theft", FALSE),
    checkboxInput("Possession_of_weapons", "Possession of weapons", FALSE),
    checkboxInput("Public_order", "Public order", FALSE),
    checkboxInput("Robbery", "Robbery", FALSE),
    checkboxInput("Shoplifting", "Shoplifting", FALSE),
    checkboxInput("Theft_from_the_person", "Theft from the person", FALSE),
    checkboxInput("Vehicle_crime", "Vehicle crime", FALSE),
    checkboxInput("Violence_and_sexual_offences", "Violence and sexual offences", FALSE)
  ),
  
  mainPanel(
    plotOutput("crimeMap"),
    plotOutput("crimeTrend")
  )
  
)