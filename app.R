library(shiny)
library(tidyverse)
library(lubridate)
library(tidyr)
library(shinythemes)
library(glue)

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

bike$StartTime <- hms(bike$StartTime)
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")
bike$Time_of_day <- cut(x=hour(bike$StartTime), breaks = breaks, labels = labels, include.lowest=TRUE)

ui <- fluidPage(theme = shinytheme("darkly"),

    titlePanel("Biketown Bikeshare"),

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

    h3("What time of day do most people ride?"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
        selectizeInput("time", "Guess the time",
                     choices = unique(bike$Time_of_day)),
        actionButton("submit", "Submit")
        ),
        mainPanel(
          textOutput("answer"),
          uiOutput("Morning"),
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
      glue("Latitude is ", lat, " and longitude is ", long)

    })

    output$paymentTable <- renderTable({
      bike1 <- bike %>%
        filter(StartLatitude == input$lat & StartLongitude == input$long) %>%
        count(PaymentPlan, sort = TRUE) %>% drop_na()
      bike1

    })


    time_msg <- reactive({
      case_when(input$time == "Morning" ~ "This is not the correct answer. ",
      input$time == "Afternoon" ~ "This is the correct answer. ",
      input$time == "Evening" ~ "Unfortunately, you are wrong. ",
      input$time == "Night" ~ "Uhoh!. "
      )
    })


    output$answer <- renderText({
      bike2 <- bike %>% select(Time_of_day) %>%
        count(Time_of_day, sort = TRUE) %>%
        mutate(percentage = (n/sum(n))*100) %>%
        mutate(percentage = round(percentage, digits =2))


          if(input$submit) {
            glue(time_msg(), bike2$percentage[1], "% people ride in the afternoon.")
          }
    })

    output$Morning <- renderUI({
      if(input$submit) {
      tags$img(src = "https://www.pngkey.com/png/detail/54-543795_eyes-sun-cartoon-half-free-sad-rays-grumpy.png",
               height="50%", width="50%", align="centre")
      }
    }) %>%
      bindEvent(input$submit)


    output$about <- renderUI({
      knitr::knit("about.Rmd", quiet = TRUE) %>%
        markdown::markdownToHTML(fragment.only = TRUE) %>%
        HTML()
    })
}

shinyApp(ui = ui, server = server)
