library(shiny)
library(tidyverse)
library(leaflet)
library(excelR)


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width =  6,
           leafletOutput(outputId = "plot",height = 700),
           h3(textOutput("compile_status"), align = "center"),
           actionButton("goButton", "Next"), p("Proof the next individual")
    ),
    column(width = 6,
           excelOutput("table", height = 400))
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### OK, lets intersect the location data with the shapefile for the AMWO 
  ### SGS coverage zones
  
  ## import datafile (this has already been run through the HMM, just sending
  ## as a csv to speed up your workflow)
  
  #amwoData.sm <- read.csv('HMMmale.csv')
  amwoData.sm <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
  
  amwoData.sm %>%
    mutate(animal_name = as.character(animal_name),
           x = long,
           y = lat,
           date = as.Date(time),
           point_state = primary_point_state,
           step_state = primary_step_state) ->
    amwoData.sm
  
  individual_stepper <- reactiveValues() #These values can be defined w/in a reactive expression and will be remembered in other reactive expressions
  individual_stepper$compiled <- 0
  individual_stepper$count <- 1
  individual_stepper$current_id <- unique(amwoData.sm$animal_name)[1]
  individual_stepper$amwoDataID <- amwoData.sm %>%
    filter(animal_name == unique(amwoData.sm$animal_name)[1])
  
  
  selected_columns <- filter(amwoData.sm, animal_name == unique(amwoData.sm$animal_name)[1]) %>% 
    dplyr::select("animal_name", "primary_point_state", "step_state", "time") 
  
  output$table <- excelTable(data = selected_columns, tableHeight = "800px") %>% 
    renderExcel()
  
  getColor <- function(state) {
    sapply(state$point_state, function(states) { # individual_stepper$amwoDataID$point_state
      if(states == "Stationary") {
        "blue"
      } else if(states == "Migratory (fall)") {
        "pink"
      } else if(states == "Migratory (spring)"){
        "green"
      } else{
        "orange"
      } })
  }
  
  individual_stepper$icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = as.character(getColor(filter(amwoData.sm, animal_name == unique(amwoData.sm$animal_name)[1])))
  )
  
  
  #Reactive plotting: anything within this expression reruns every time input is modified
  output$plot <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     minZoom = 1, maxZoom = 22,
                                     dragging = TRUE)) %>%
      addTiles() %>% # Default base mape
      addProviderTiles("Esri.WorldImagery") %>%  # ortho image
      addProviderTiles(providers$Stamen.TonerLines) %>% # state lines and roads.
      #addProviderTiles(providers$Stamen.TonerLabels) %>% # add location and road labels
      addScaleBar() %>%
      addAwesomeMarkers(lng=individual_stepper$amwoDataID$x, 
                        lat=individual_stepper$amwoDataID$y, 
                        icon=individual_stepper$icons,
                        popup = individual_stepper$amwoDataID$date) %>%  
      addPolylines(lng =individual_stepper$amwoDataID$x, 
                   lat = individual_stepper$amwoDataID$y, 
                   weight=3, color="red")
    
    
  })#end of reactive plotting
  
  #clean locations that we start with
  #  location_iterator <- reactive({
  #amwoData.sm %>%  filter(animal_name == individual_stepper$current_ID)
  #  }) 
  
  #updating when go button is pressed
  observeEvent(input$goButton, {
    # Check if we've reached the end. If so, compile. If not, advance
    if(individual_stepper$count != length(unique(amwoData.sm$animal_name))){
      individual_stepper$count <- individual_stepper$count + 1
      individual_stepper$current_id <- unique(amwoData.sm$animal_name)[individual_stepper$count]
      print(individual_stepper$current_id)
    }
    else{
      individual_stepper$compiled <- 1
    }
    
    individual_stepper$amwoDataID <- subset(amwoData.sm, amwoData.sm$animal_name==individual_stepper$current_id)
    
    selected_columns <- individual_stepper$amwoDataID %>% 
      dplyr::select("animal_name", "primary_point_state", "step_state", "time")  
    
    output$table <- renderExcel(excelTable(data = selected_columns, tableHeight = "800px"))
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(individual_stepper$amwoDataID))
    )
  })
  
  #  observeEvent(input$goButton,{
  #  })individual_stepper$current_id
  
  output$compile_status <- renderText({
    if(individual_stepper$compiled == 0){
      individual_stepper$current_id
    } else{
      "All individuals parsed"
    }
  })
}#end of server call

# Run the application 
shinyApp(ui = ui, server = server)