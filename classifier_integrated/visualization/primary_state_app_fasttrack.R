library(shiny)
library(tidyverse)
library(leaflet)
library(excelR)
library(move)

# NOTE: A WHITE SCREEN ON BOOT MEANS THE APP IS WORKING AS INTENDED. SHOULD TAKE ~20 SECONDS TO
# RETRIEVE DATA FROM MOVEBANK

# This version of the primary state review app uses a "fast track" process to speed review
# It downloads the most recent version of the point and step states from Movebank, and checks those
# states against those currently delineated. It only prompts the user to review an individual
# if those point and step states have changed, or are newly delineated, since the version on Movebank


# Define UI for application that draws a histogram
ui <- fluidPage(
  column(width =  6,
         leafletOutput(outputId = "plot",height = 700),
         h3(textOutput("compile_status"), align = "center"),
         div(style = "display: flex;",
             actionButton("backButton", "Back", width = '75px'),
             actionButton("nextButton", "Next", width = '75px'), #p("Proof the next individual")
             selectInput(inputId = "dropdown", label = NULL, choices = c())# choices = animal_list
         )
  ),
  column(width = 6,
         excelOutput("table", height = 400))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #amwoData.sm <- read.csv('HMMmale.csv')
  amwoData.sm <- readRDS(here::here("classifier_integrated", "fac_primary_state_delineation.rds"))
  
  amwoData.sm <- amwoData.sm %>%
    mutate(animal_name = as.character(animal_name),
           x = long,
           y = lat,
           date = as.Date(time),
           point_state = primary_point_state,
           step_state = primary_step_state) %>% 
    arrange(animal_name, time)
  
  # animal_list <- amwoData.sm$animal_name %>%
  #   unique()
  
  # Check results against those on Movebank
  
  amwo_move <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                               login = readRDS(here::here("movebank_credentials.rds")), 
                               removeDuplicatedTimestamps=TRUE) %>% 
    as.data.frame()
  
  amwo_move <- amwo_move %>% 
    transmute(event_id = event_id,
              mb_point_state = str_remove(behavioural_classification, "Point state: "),
              mb_step_state = str_remove(migration_stage, "Step state: ")) %>% 
    mutate(mb_point_state = ifelse(is.na(mb_point_state), "Not available", mb_point_state),
           mb_step_state = ifelse(is.na(mb_step_state), "Not available", mb_step_state))
  
  #test code
  # amwoData.sm[amwoData.sm$event_id == 14074649058, "step_state"] <- "test"
  
  animal_list <- amwoData.sm %>% 
    left_join(amwo_move) %>% 
    filter(!(point_state == mb_point_state & step_state == mb_step_state)) %>% 
    pull(animal_name) %>% 
    unique()
  
  print(animal_list)
  
  updateSelectInput(inputId = "dropdown", choices = animal_list)
  
  # Begin reactive processing
  
  individual_stepper <- reactiveValues() #These values can be defined w/in a reactive expression and will be remembered in other reactive expressions
  # individual_stepper$compiled <- 0
  individual_stepper$count <- which(animal_list == animal_list[1])
  individual_stepper$current_id <- animal_list[1]
  individual_stepper$amwoDataID <- filter(amwoData.sm, animal_name == animal_list[1])
  
  
  selected_columns <- filter(amwoData.sm, animal_name == animal_list[1]) %>% 
    dplyr::select("animal_name", "point_state", "step_state", "time") 
  
  output$table <- excelTable(data = selected_columns, tableHeight = "800px") %>% 
    renderExcel()
  
  getColor <- function(state) {
    sapply(state$point_state, function(states) { # individual_stepper$amwoDataID$point_state
      # Assignments
      # stationary- cyan
      # mig summer- pink
      # Migratory (fall)- pink
      # mig spring- green
      # foray loop (spring)- yellow
      # foray loop (summer)- yellow
      # foray loop (winter)- yellow
      # foray loop (fall)- yellow
      # dispersal (winter)- white
      # "Unknown- bugged frequent schedule"- orange
      
      if(states == "Stationary") {
        "blue"
      } else if(states == "Migratory (fall)") {
        "pink"
      } else if(states == "Migratory (summer)") {
        "red"
      } else if(states == "Migratory (spring)"){
        "green"
      } else if(states == "Foray loop"){
        "cadetblue"
      } else if(states == "Dispersal"){
        "darkred"
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
      addLegend("bottomright", pal = colorFactor(palette = c("darkred", "cadetblue", "cadetblue", "cadetblue", "cadetblue", "pink", "green", "red", "cyan", "orange"), #c("grey", "white", "black", "cyan", "yellow", "red", "green", "pink", "blue", "orange")
                                                 domain = c("Stationary", "Migratory (spring)", "Migratory (summer)", "Foray loop (spring)", "Foray loop (summer)", "Migratory (fall)", "Foray loop (winter)", "Dispersal (winter)", "Foray loop (fall)", "Unknown- bugged frequent schedule")), 
                values = individual_stepper$amwoDataID$point_state) %>% 
      addAwesomeMarkers(lng=individual_stepper$amwoDataID$x, 
                        lat=individual_stepper$amwoDataID$y, 
                        icon=individual_stepper$icons,
                        popup = individual_stepper$amwoDataID$date) %>%  
      addPolylines(lng =individual_stepper$amwoDataID$x, 
                   lat = individual_stepper$amwoDataID$y, 
                   weight=3, color="red")
  })#end of reactive plotting
  
  #updating when go button is pressed
  observeEvent(input$nextButton, {
    if(which(animal_list == individual_stepper$current_id) == length(animal_list)){ #If we're on the last ID, stay on the last ID
      individual_stepper$current_id <- animal_list[which(animal_list == individual_stepper$current_id)]
      print(individual_stepper$current_id)
    } else {
      individual_stepper$current_id <- animal_list[which(animal_list == individual_stepper$current_id) + 1]
      print(individual_stepper$current_id)
    }
    
    updateSelectInput(inputId = "dropdown", selected = individual_stepper$current_id)
    
    individual_stepper$amwoDataID <- filter(amwoData.sm, animal_name == individual_stepper$current_id)
    
    selected_columns <- individual_stepper$amwoDataID %>% 
      dplyr::select("animal_name", "point_state", "step_state", "time")  
    
    output$table <- renderExcel(excelTable(data = selected_columns, tableHeight = "800px"))
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(individual_stepper$amwoDataID))
    )
  })
  
  observeEvent(input$backButton, {
    if(which(animal_list == individual_stepper$current_id) == 1){ #If we're on the last ID, stay on the last ID
      individual_stepper$current_id <- animal_list[which(animal_list == individual_stepper$current_id)]
      print(individual_stepper$current_id)
    } else {
      individual_stepper$current_id <- animal_list[which(animal_list == individual_stepper$current_id) - 1]
      print(individual_stepper$current_id)
    }
    
    updateSelectInput(inputId = "dropdown", selected = individual_stepper$current_id)
    
    individual_stepper$amwoDataID <- filter(amwoData.sm, animal_name == individual_stepper$current_id)
    
    selected_columns <- individual_stepper$amwoDataID %>% 
      dplyr::select("animal_name", "point_state", "step_state", "time")  
    
    output$table <- renderExcel(excelTable(data = selected_columns, tableHeight = "800px"))
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(individual_stepper$amwoDataID))
    )
  })
  
  observeEvent(input$dropdown, {
    individual_stepper$current_id <- input$dropdown
    print(individual_stepper$current_id)
    
    
    individual_stepper$amwoDataID <- filter(amwoData.sm, animal_name == individual_stepper$current_id)
    
    selected_columns <- individual_stepper$amwoDataID %>% 
      dplyr::select("animal_name", "point_state", "step_state", "time")  
    
    output$table <- renderExcel(excelTable(data = selected_columns, tableHeight = "800px"))
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = as.character(getColor(individual_stepper$amwoDataID))
    )
  })
  
  output$compile_status <- renderText({
    individual_stepper$current_id
  })
}#end of server call

# Run the application 
shinyApp(ui = ui, server = server)