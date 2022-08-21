library(shiny)
library(tidyverse)
library(lubridate)
library(tidyr)

bike <- read_csv('week10_biketown.csv')
bike[c('code', 'bike_name')] <- str_split_fixed(bike$BikeName, ' ', 2)
bike[c('BikeName2', 'info')] <- str_split_fixed(bike$bike_name, '-', 2)
bike[c('BikeName3', 'info')] <- str_split_fixed(bike$BikeName2, '\\(', 2)
bike$bikeName <- trimws(bike$BikeName3, which = c("both"))
bike <- bike %>% select(-BikeName, -bike_name, -BikeName2, -BikeName3)

bike <- bike %>%
  mutate(Duration = as.numeric(hms(bike$Duration))) %>%
  mutate(Duration = Duration/3600) %>%
  mutate(speed = Distance_Miles/Duration) %>%
  mutate(speed = round(speed, digits = 3))

bike <- bike %>%
  drop_na(StartLatitude, StartLongitude, StartHub) %>%
  mutate(StartLatitude = round(StartLatitude, digits = 2)) %>%
  mutate(StartLongitude = round(StartLongitude, digits = 2))

ui <- fluidPage(

    titlePanel("Biketown Bikeshare"),

    h3("1. How long are different bikes driven for?"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(multiple = TRUE,
                     helpText("Select a bike"),
                     selectInput("bikeName", h3("Bike Name"),
                                 choices = unique(bike$bikeName))
                     ),
        mainPanel(
          plotOutput("bikeDur")
        )

      )
    ),

    h3("2. Compare bike"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(h1("Bike Comparison"),
                     selectizeInput("bikes_compare1", "Select bike 1",
                                    choices = unique(bike$bikeName)),
                     selectizeInput("bikes_compare2", "Select bike 2",
                                    choices = unique(bike$bikeName))

        ),
        mainPanel(
          plotOutput("compare_bike")
        )
      )
    ),

    h3("3.Find Area's attributes"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(h3("Find your latitude and longitude"),
                     selectizeInput("find_latitude", "Choose your Area",
                                    choices = unique(bike$StartHub)),
                     numericInput("lat", "Enter your latitude", value = 45.52),
                     numericInput("long", "Enter your longitude", value = -122.65)

        ),

        mainPanel(
          textOutput("latitude"),
          tableOutput("paymentTable")
        )
      )
    ),

    h3("3.Time of Day"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(h3("Find your latitude and longitude"),
                     selectizeInput("find_latitude", "Choose your Area",
                                    choices = unique(bike$StartHub)),
                     numericInput("lat", "Enter your latitude", value = 45.52),
                     numericInput("long", "Enter your longitude", value = -122.65)

        ),

        mainPanel(
          textOutput("latitude"),
          tableOutput("paymentTable")
        )
      )
    ),


    fluidRow(
      column(10,
             div(class = "about",
                 uiOutput('about'))
      )
    ),
    includeCSS("styles.css")
)

server <- function(input, output, session) {

  observe({
    updateSelectInput(session, "bikes_compare2", choices = setdiff(unique(bike$bikeName), input$bikes_compare1))
  })

  output$bikeDur <- renderPlot({
    dplyr::filter(bike, bikeName == input$bikeName) %>%
      ggplot(aes(MultipleRental)) +
      geom_bar()
    })

    output$compare_bike <- renderPlot({
      bike %>% group_by(bikeName) %>%
        summarise(avg_speed = mean(speed, na.rm = TRUE)) %>%
        filter(bikeName == input$bikes_compare1 | bikeName == input$bikes_compare2) %>%
        ggplot(aes(bikeName, avg_speed, fill = bikeName)) + geom_col()
    })

    output$latitude <- renderText({
      lat <- unique(bike$StartLatitude[bike$StartHub== input$find_latitude])
      lat
      long <- unique(bike$StartLongitude[bike$StartHub== input$find_latitude])
      long
      paste("Latitude is", lat, "and longitude is", long)

    })

    output$paymentTable <- renderTable({
      bike1 <- bike %>%
        filter(StartLatitude == input$lat & StartLongitude == input$long) %>%
        count(PaymentPlan) %>% drop_na()
        bike1

    })


    output$about <- renderUI({
      knitr::knit("about.Rmd", quiet = TRUE) %>%
        markdown::markdownToHTML(fragment.only = TRUE) %>%
        HTML()
    })
}

shinyApp(ui = ui, server = server)
