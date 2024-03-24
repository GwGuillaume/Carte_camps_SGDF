library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Full list of departments numbers
all_depts <- sort(unique(sgdf_data$`Numéro Département`))
# Full list of postal codes
all_postal_codes <- sort(unique(sgdf_data$`Code postal`))

function(input, output, session) {
  
  data_plt <- reactive({
    sgdf_data
  })
    
  ## Interactive Map ###########################################
  get_df_centroid <- function(df){
    centroids_sf <- st_as_sf(df, coords = c('long_camp', 'lat_camp')) %>% 
      summarize(geometry = st_union(geometry)) %>% 
      st_centroid %>% 
      st_coordinates() %>% 
      as_data_frame() %>% 
      mutate(long = X, lat = Y) %>% 
      select(-c(X, Y))
    return(centroids_sf)
  }
  
  print("Create the map")
  campInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(data_plt()[FALSE,])
    bounds <- input$map_bounds
    print("bounds")
    print(bounds)
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(data_plt(),
           lat_camp >= latRng[1] & lat_camp <= latRng[2] &
             long_camp >= lngRng[1] & long_camp <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(campInBounds()) == 0)
  #     return(NULL)
  #   
  #   hist(campInBounds()$centile,
  #        breaks = centileBreaks,
  #        main = "SuperZIP score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(allzips$centile),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(campInBounds()) == 0)
  #     return(NULL)
  #   
  #   print(xyplot(income ~ college, data = campInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })
  
  # ## Filter data
  # filter_df <- reactive({
  #   filter_df <- sgdf_data
  #   if(input$code_postal == "Tous"){
  #     sgdf_data
  #   } else {
  #     sgdf_data %>% filter(map_lgl(`Code postal`, ~any(. %in% input$code_postal)))
  #   }
  # })
  
  # Centroid
  print("Centroid")
  centroids_sf <- eventReactive({
    input$code_postal
    input$num_dept}, 
    {
      get_df_centroid(df = data_plt())
    })
  
  # Postal codes
  get_postal_codes <- function(num_dept){
    browser()
    if(num_dept == "Tous"){
      postal_codes <- c("Tous", all_postal_codes)
    } else {
      id_cp <-which(as.integer(substr(all_postal_codes, start = 1, stop = 2)) == num_dept)
      postal_codes <- all_postal_codes[id_cp]
    }
    return(postal_codes)
  }
  
  # codes_postaux <- c("Tous", unique(sgdf_data$`Code postal`))
  codes_postaux <- eventReactive(
    {
      input$num_dept
    }, {
      browser()
      ifelse(test = is.null(input$code_postal),
             yes = get_postal_codes(num_dept = "Tous"),
             no = get_postal_codes(input$num_dept))
    })
  
  output$postal_codes_list = renderUI({
    browser()
    selectInput("code_postal", "Code postal", selected="Tous", choices=codes_postaux(), multiple = FALSE)  # choices=c("Tous", unique(sgdf_data$`Code postal`))
  })
  
  # Mapped data
  data_plt <- eventReactive(
    {
      input$refresh_map
    }, {
      browser()
      # code_postal <- input$code_postal
      code_postal <- ifelse(test = is.null(input$code_postal) | input$code_postal=="Tous",
                            yes = c("Tous", all_postal_codes),  #"Tous",
                            no = input$code_postal)
      if(input$code_postal == "Tous"){
        sgdf_data %>% arrange(`Unité`)
      } else {
        data <- sgdf_data %>% 
          filter(map_lgl(`Code postal`, ~any(. %in% code_postal))) %>% 
          arrange(`Unité`)
      }
    })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    print("observe")
    colorBy <- input$color
    sizeBy <- input$size
    
    if (colorBy == "Unité") {
      colorData <- data_plt()$Couleur
    } else if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- data_plt()$Couleur
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- data_plt()[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(data_plt()$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- data_plt()[[sizeBy]] / max(data_plt()[[sizeBy]]) * 30000
    }
    
    output$map <- renderLeaflet({
      browser()
      data <- data_plt()
      leaflet(data = data_plt()) %>% 
        addTiles() %>%
        setView(lng = centroids_sf()$long, lat = centroids_sf()$lat, zoom = 6) %>% 
        clearShapes() %>%
        addCircleMarkers(lng = ~long_camp, lat = ~lat_camp, radius = 5, group = ~Unité, color = ~Couleur, opacity = 0.6, fill = TRUE, fillColor = ~Couleur, fillOpacity = 0.4, 
                         popup = paste0(
                           "<b>Ville : </b>"
                           , data$Ville
                           , "<br>"
                           , "<b>Unité : </b>"
                           , data$Unité
                           , "<br>"
                           , "<b>Date début : </b>"
                           , data$`Date début`
                           , "<br>"
                           , "<b>Date fin : </b>"
                           , data$`Date fin`
                           , "<br>"
                           , "<b>Lieu : </b>"
                           , data$Ville, " (", data$`Code postal`, "), ",  data$Département, " - ", data$Région
                           , "<br>"
                           , "<b>Nom structure participante : </b>"
                           , data$`Nom structure participante`
                           , "<br>"
                         ))
      })
    
    # observeEvent(input$code_postal, {
    #   codePostal <- input$code_postal
    #   if(codePostal == "Tous"){
    #     data_plt <- sgdf_data
    #   } else {
    #     data_plt <- sgdf_data %>% filter(map_lgl(`Code postal`, ~any(. %in% input$code_postal)))
    #   }
    #   # Order data by centile, we ensure that the (comparatively rare) SuperZIPs
    #   # will be drawn last and thus be easier to see
    #   data_plt <- data_plt()[order(data_plt()$Unité),]
    #   leafletProxy("map") %>% 
    #     addTiles() %>%
    #     setView(lng = centroids_sf()$long, lat = centroids_sf()$lat, zoom = 4) %>% 
    #     clearShapes() %>% 
    #     addCircleMarkers(lng = ~long_camp, lat = ~lat_camp, radius = 5, group = ~Unité, color = ~Couleur, opacity = 0.6, fill = TRUE, fillColor = ~Couleur, fillOpacity = 0.4, 
    #                      popup = paste0(
    #                        "<b>Ville : </b>"
    #                        , data$Ville
    #                        , "<br>"
    #                        , "<b>Unité : </b>"
    #                        , data$Unité
    #                        , "<br>"
    #                        , "<b>Date début : </b>"
    #                        , data$`Date début`
    #                        , "<br>"
    #                        , "<b>Date fin : </b>"
    #                        , data$`Date fin`
    #                        , "<br>"
    #                        , "<b>Lieu : </b>"
    #                        , data$Ville, " (", data$Département, " - ", data$Région, ")"
    #                        , "<br>"
    #                        , "<b>Nom structure participante : </b>"
    #                        , data$`Nom structure participante`
    #                        , "<br>"
    #                      ))
    # })

    
  })
  
  # # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })
  
  
  ## Data Explorer ###########################################
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectizeInput(session, "cities", choices = cities,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  # 
  # output$ziptable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df, outputId = "ziptable")
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}