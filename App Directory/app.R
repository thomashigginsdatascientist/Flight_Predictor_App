
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

#Read in Packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggmap)
library(ggnetwork)
library(shinyTime)
library(lubridate)
library(shinycssloaders)
library(leaflet)

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

#Read in gloabl variables

path <- "C:/Users/thigg/Desktop/Career Code/Flight App/App Directory/"

Geo <- readRDS(paste0(path, "Geo Data.RDS"))

arrive <- readRDS(paste0(path, "Arrive Model.RDS"))
depart <- readRDS(paste0(path, "Depart Model.RDS"))
train <- readRDS(paste0(path, "train.RDS"))
training <- train[1,]
training <- training %>%
  select(sched_dep_time, sched_arr_time, carrier, origin, dest, distance, Num.Flights.That.Day, name)

Geo <- as.data.frame(Geo)

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# Define UI 

ui <- fluidPage(
  
  # Application title
  titlePanel("Predict If Your Flight Will Be Late Or On Time!"),
  
  sidebarPanel(
    
    fluidRow(
      column(5,
             selectInput(inputId = "origin", label = "Origin:", choices = unique(Geo$origin), selected = "PIT"),
             br(),
             br(),
             timeInput(input = "depart", label = "Scheduled Departure Time:", seconds = FALSE, minute.steps = 30),
             timeInput(input = "arrive", label = "Scheduled Arrival Time:", seconds = FALSE, minute.steps = 30)),
      column(5,
             selectInput(inputId = "destination", label = "Destination:", choices = unique(Geo$dest), selected = "BOS"),
             br(),
             br(),
             selectInput(inputId = "airline", label = "Airline:", choices = unique(train$name), multiple = FALSE),
             actionButton(inputId = "run", label = "Run Late/Early Analysis!"))
    )
    
  ),
  
  mainPanel(
    #plotOutput(outputId = "LocationMap"),
    withSpinner(uiOutput(outputId = "status"), type = 1),
    withSpinner(leafletOutput("mymap"), type = 1)
  )

)

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# Define server logic 
server <- function(input, output, session) {
  
  
  geo_update <- reactive({
    
    Geo %>%
      filter(origin == input$origin) %>%
      as.data.frame()
    
  })
  
  air_update <- reactive({
    
    train %>%
      filter(origin == input$origin) %>%
      filter(dest == input$destination) %>%
      as.data.frame()
    
  })
  
  map_update_origin <- reactive({
    Geo %>%
      filter(origin == input$origin) %>%
      distinct(origin, .keep_all = TRUE) %>%
      select(origin, lon, lat) %>%
      as.data.frame()
  })
  
  map_update_destination <- reactive({
    Geo %>%
      filter(dest == input$destination) %>%
      distinct(dest, .keep_all = TRUE) %>%
      select(dest, lon, lat) %>%
      rename("origin" = dest) %>%
      as.data.frame()
  })
  
  map_update <- reactive({
    rbind(map_update_origin(), map_update_destination())
  })
  
  
  observeEvent(input$origin, {
  updateSelectInput(session = session, inputId = "destination", choices = c(unique(geo_update()$dest)))
  })
  
  observeEvent(input$destination, {
    updateSelectInput(session = session, inputId = "airline", choices = c(unique(air_update()$name)))
  })
  
  
  observeEvent(input$run, {
    
    input_flight <- train %>%
      filter(origin==input$origin) %>%
      filter(dest==input$destination) %>%
      filter(name == input$airline) %>%
      mutate(sched_dep_time = as.character(input$depart)) %>%
      mutate(sched_dep_time = case_when(minute(sched_dep_time) == 0 ~ paste(as.character(hour(sched_dep_time)), as.character(minute(sched_dep_time)), "0", sep = ""),
                                        TRUE ~ paste(as.character(hour(sched_dep_time)), as.character(minute(sched_dep_time)), sep = ""))) %>%
      mutate(sched_arr_time = as.character(input$arrive)) %>%
      mutate(sched_arr_time = case_when(minute(sched_arr_time) == 0 ~ paste(as.character(hour(sched_arr_time)), as.character(minute(sched_arr_time)), "0", sep = ""),
                                        TRUE ~ paste(as.character(hour(sched_arr_time)), as.character(minute(sched_arr_time)), sep = ""))) %>%
      mutate(sched_dep_time = as.integer(sched_dep_time)) %>%
      mutate(sched_arr_time = as.integer(sched_arr_time)) %>%
      select(sched_dep_time, sched_arr_time, carrier, origin, dest, distance, Num.Flights.That.Day, name) %>%
      filter(row_number() == 1) %>%
      as.data.frame()
    
    input_flight <- rbind(training, input_flight)
    input_flight <- input_flight[-1,]
    
    input_flight$arrival_pred <- predict(arrive, newdata = input_flight)
    input_flight$depart_pred <- predict(depart, newdata = input_flight)
    
    output$status <- renderUI({
      h1(paste0("This flight from ", as.character(input_flight$name),  " will most likely depart ", as.character(input_flight$arrival_pred), " and 
                arrive ", as.character(input_flight$depart_pred), "!"))
    })
    
    output$depart_stats <- renderUI({
      DT::datatable(input_flight)
    })
    
  })
  
  

  observeEvent(input$run, {

    output$mymap <- renderLeaflet({
      
      leaflet(data = map_update()) %>%
        addTiles() %>%
        addMarkers(~lon, ~lat, popup = ~as.character(paste(origin, lon, lat, sep = "/")), label = ~as.character(origin)) %>%
        addPolylines(lng = ~lon, lat = ~lat, group = ~origin)
    
  })
    
  })

  
  #Save For Later if Needed
  # observeEvent(input$origin, {
  #   
  #   points <- Geo %>%
  #     as.data.frame()
  #   
  #   zoom <- calc_zoom(points, lon = lon, lat = lat)
  #   zoom <- ifelse(zoom > 18, 18, zoom)
  # 
  # output$LocationMap <- renderPlot({
  #   
  #   validate(need(points, 'Press the Run Button To Run the Analysis!'))
  #   
  #   qmplot(data = points, lon, lat, zoom = zoom) +
  #     geom_segment(data = points %>% filter(origin == input$origin) %>% filter(dest == input$destination),  aes(xend=lonend, yend=latend), colour = "blue", size = 1) +
  #     theme_minimal() +
  #     labs(
  #       title = "Flight Network Map",
  #       x = "Longitutde",
  #       y = "Latitude"
  #     ) +
  #     theme(
  #       plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  #       plot.subtitle = element_text(face = "bold", hjust = 0.5),
  #       axis.title.x = element_text(size = 10),
  #       axis.title.y = element_text( size = 10),
  #     )
  #   
  # })
  #   
  # })
  

}

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
