library(shiny)
library(tidyverse)
library(lubridate)
library(tidyr)
library(shinythemes)
library(glue)

bike_unzip <- unzip('week10_biketown.zip')
bike <- read_csv(bike_unzip)
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

ui <- fluidPage(theme = shinytheme("journal"),

                titlePanel(div(column(width = 6, h2("Biketown Bikeshare Analysis")),
                               column(width = 6, tags$img(src = "https://i.pinimg.com/originals/24/ae/8d/24ae8def288851503cf68340df174963.gif",
                                                          height="50%", width="50%", align="centre"))),
                ),

                h3("This app gives you insight into speeds of various bikes, riders in your area and general time trends. Visit the different tabs for more information!"),

                tags$style(HTML("
      .tabbable > .nav > li > a {background-color: aqua;  color:black}
  ")),
                tabsetPanel(
                  tabPanel(h3("Speed and Area"),


                           h3("1. Compare the average speed of the bikes."),
                           fluidRow(
                             sidebarLayout(
                               sidebarPanel(h2("Bike Speed Comparison"),
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

                           h3("2. Find the bikers' general attributes 10km around your Area."),
                           fluidRow(
                             sidebarLayout(
                               sidebarPanel(h3("Find your latitude and longitude if you are unsure."),
                                            selectizeInput("find_latitude", "Choose your Area",
                                                           choices = unique(bike$StartHub)),
                                            numericInput("lat", "Enter your latitude", value = 45.52),
                                            numericInput("long", "Enter your longitude", value = -122.65),
                                            actionButton("submit", "Submit")

                               ),

                               mainPanel(
                                 textOutput("latitude"),
                                 br(),
                                 tableOutput("paymentTable"),
                                 br(),
                                 tableOutput("summary")
                               )
                             )
                           )),

                  tabPanel(h3("Time"),

                           h3("3. What time of day do most people ride? Take a guess!"),
                           fluidRow(
                             sidebarLayout(
                               sidebarPanel(
                                 selectizeInput("time", "Guess the time",
                                                choices = unique(bike$Time_of_day)),
                                 actionButton("guess", "Guess")
                               ),
                               mainPanel(
                                 textOutput("answer"),
                                 uiOutput("Morning"),
                               )

                             )
                           )),

                  tabPanel(h3("About the app"),

                           fluidRow(
                             column(10,
                                    div(class = "about",
                                        uiOutput('about'))
                             )
                           ),
                           includeCSS("styles.css")
                  )))

server <- function(input, output, session) {

  observe({
    updateSelectInput(session, "bikes_compare2", choices = setdiff(unique(bike$bikeName), input$bikes_compare1))
  })

  output$compare_bike <- renderPlot({
    bike %>%
      filter(bikeName == input$bikes_compare1 | bikeName == input$bikes_compare2) %>%
      ggplot(aes(bikeName, speed, fill = bikeName)) +
      geom_boxplot(show.legend = FALSE) +
      labs(title = "Average Speed in miles per hour") +
      ylab("Average Speed in miles/hour") +
      xlab("Bike Name") +
      theme_minimal() +
      theme(axis.text.x = element_text(color = "blue")) +
      theme(text = element_text(size = 20))

  })

  output$latitude <- renderText({
    lat <- unique(bike$StartLatitude[bike$StartHub== input$find_latitude])
    lat
    long <- unique(bike$StartLongitude[bike$StartHub== input$find_latitude])
    long
    glue("Latitude is ", lat, " and longitude is ", long)

  })

  output$paymentTable <- renderTable({
    if(input$submit) {
      bike1 <- bike %>%
        filter(StartLatitude == input$lat & StartLongitude == input$long) %>%
        count(PaymentPlan, sort = TRUE) %>% drop_na()
      bike1
    }

  })

  output$summary <- renderTable({
    if(input$submit) {
      bike2 <- bike %>%
        filter(StartLatitude == input$lat & StartLongitude == input$long) %>%
        summarise(average_speed_milesPerHour = mean(speed, na.rm = TRUE),
                  average_duration_seconds = mean((Duration*3600), na.rm = TRUE))
      bike2
    }

  })


  time_msg <- reactive({
    if(input$guess){
      case_when(input$time == "Morning" ~ "This is not the correct answer. ",
                input$time == "Afternoon" ~ "This is the correct answer. ",
                input$time == "Evening" ~ "Unfortunately, you are wrong. ",
                input$time == "Night" ~ "Uhoh!. "
      )}
  })


  output$answer <- renderText({
    bike2 <- bike %>% select(Time_of_day) %>%
      count(Time_of_day, sort = TRUE) %>%
      mutate(percentage = (n/sum(n))*100) %>%
      mutate(percentage = round(percentage, digits =2))


    if(input$guess) {
      glue(time_msg(), bike2$percentage[1], "% people ride in the afternoon.")
    }
  })

  output$Morning <- renderUI({
    if(input$guess) {
      if(input$time == "Morning"){
        tags$img(src = "https://hdclipartall.com/images/good-morning-animated-clip-art-good-morning-clip-art-free-2-image-3-morning-clipart-550_400.jpg",
                 height="50%", width="50%", align="centre")

      }
      else if(input$time == "Afternoon") {
        tags$img(src = "https://www.seekpng.com/png/detail/457-4571607_happy-sun-free-download-clip-art-on-clipart.png",
                 height="50%", width="50%", align="centre")
      }
      else if(input$time == "Evening") {
        tags$img(src = "https://img.freepik.com/premium-vector/sunset-sky-cartoon-summer-sunrise-with-pink-clouds-sunshine-evening-cloudy-heaven-panorama_461812-70.jpg?w=360",
                 height="50%", width="50%", align="centre")
      }
      else if(input$time == "Night") {
        tags$img(src = "https://thumbs.dreamstime.com/b/cute-moon-character-sad-expression-cute-moon-character-sad-expression-your-commercial-project-others-182038820.jpg",
                 height="50%", width="50%", align="centre")
      }
    }
  }) %>%
    bindEvent(input$guess)


  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)
