
# ============================================================
# DS422 - Oahu Ocean Rescue Accessibility Shiny App
# Swimming | Lifeguard | Coast Guard
# ============================================================

# 1. PACKAGES -----------------------------------------------

library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(dplyr)
library(DT)

# 2. LOAD DATA ----------------------------------------------

results_path <- "outputs/ocean_rescue_accessibility.geojson"

if (!file.exists(results_path)) {
  stop(
    "Missing outputs/ocean_rescue_accessibility.geojson. ",
    "Render ocean_rescue.qmd first to create it."
  )
}

sites <- st_read(results_path, quiet = TRUE) |>
  st_transform(4326)

# Check that the Quarto report exported the needed fields.
required_columns <- c(
  "lifeguard_distance_km",
  "swim_distance_km"
)

missing_columns <- setdiff(
  required_columns,
  names(sites)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in exported GeoJSON: ",
    paste(missing_columns, collapse = ", "),
    ". Re-render the complete ocean_rescue.qmd."
  )
}

read_optional_sf <- function(path) {
  if (file.exists(path)) {
    st_read(path, quiet = TRUE) |>
      st_transform(4326)
  } else {
    NULL
  }
}

lifeguards <- read_optional_sf(
  "data/lifeguards.geojson"
)

shoreline <- read_optional_sf(
  "data/shoreline_access.geojson"
)

coast_guard_path <- "data/coast_guard_facilities.csv"

if (file.exists(coast_guard_path)) {
  coast_guard <- read.csv(
    coast_guard_path,
    stringsAsFactors = FALSE
  )
} else {
  coast_guard <- data.frame(
    name = character(),
    address = character()
  )
}

# Only plot Coast Guard points with coordinates.
# Facility coordinates are not automatically
# operational boat-departure coordinates.

coast_guard_sf <- NULL

if (all(
  c("longitude", "latitude") %in% names(coast_guard)
)) {
  
  coast_guard$longitude <- suppressWarnings(
    as.numeric(coast_guard$longitude)
  )
  
  coast_guard$latitude <- suppressWarnings(
    as.numeric(coast_guard$latitude)
  )
  
  valid <- is.finite(coast_guard$longitude) &
    is.finite(coast_guard$latitude) &
    coast_guard$longitude >= -180 &
    coast_guard$longitude <= 180 &
    coast_guard$latitude >= -90 &
    coast_guard$latitude <= 90
  
  if (any(valid)) {
    coast_guard_sf <- st_as_sf(
      coast_guard[valid, , drop = FALSE],
      coords = c("longitude", "latitude"),
      crs = 4326
    )
  }
}

# 3. USER INTERFACE -----------------------------------------

ui <- page_sidebar(
  
  title = "Oʻahu Ocean Rescue Accessibility",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  sidebar = sidebar(
    
    h4("Accessibility Controls"),
    
    selectInput(
      inputId = "service",
      label = "Select emergency service:",
      choices = c(
        "Swimming Rescue",
        "Lifeguard Response",
        "Coast Guard Response"
      ),
      selected = "Lifeguard Response"
    ),
    
    conditionalPanel(
      condition = "input.service != 'Coast Guard Response'",
      
      sliderInput(
        inputId = "speed",
        label = "Hypothetical travel speed (km/h):",
        min = 1,
        max = 15,
        value = 5,
        step = 0.5
      ),
      
      selectInput(
        inputId = "threshold",
        label = "Accessibility threshold:",
        choices = c(
          "5 minutes" = 5,
          "10 minutes" = 10,
          "20 minutes" = 20
        ),
        selected = 10
      )
    ),
    
    hr(),
    
    p(
      "Travel times are hypothetical proximity ",
      "scenarios, not actual emergency response times."
    ),
    
    hr(),
    
    h5("Map Legend"),
    
    p("Green: 0–5 minutes"),
    p("Yellow: 5–10 minutes"),
    p("Orange: 10–20 minutes"),
    p("Red: More than 20 minutes")
  ),
  
  layout_columns(
    
    
    
    value_box(
      title = "Coastal Sites",
      value = textOutput("total_sites")
    ),
    
    value_box(
      title = "Mean Scenario Time",
      value = textOutput("mean_time")
    ),
    
    value_box(
      title = "Sites Within Threshold",
      value = textOutput("accessible_sites")
    ),
    
    col_widths = c(4, 4, 4)
  ),
  
  card(
    card_header("Ocean Rescue Accessibility Map"),
    leafletOutput(
      "rescue_map",
      height = "540px"
    )
  ),
  
  card(
    card_header("Accessibility Results"),
    DTOutput("results_table")
  ),
  
  card(
    card_header("Methodology and Limitations"),
    uiOutput("methodology")
  )
  
)

# 4. SERVER -------------------------------------------------

server <- function(input, output, session) {
  
  # Change the default speed when a service changes.
  observeEvent(input$service, {
    
    if (input$service == "Swimming Rescue") {
      updateSliderInput(
        session,
        "speed",
        value = 2
      )
    }
    
    if (input$service == "Lifeguard Response") {
      updateSliderInput(
        session,
        "speed",
        value = 5
      )
    }
    
  })
  
  # 5. REACTIVE ACCESSIBILITY CALCULATIONS ------------------
  
  accessibility_data <- reactive({
    
    if (input$service == "Coast Guard Response") {
      return(NULL)
    }
    
    data <- sites
    
    if (input$service == "Lifeguard Response") {
      data$distance_km <-
        as.numeric(data$lifeguard_distance_km)
    } else {
      data$distance_km <-
        as.numeric(data$swim_distance_km)
    }
    
    req(input$speed > 0)
    
    data$time_min <-
      data$distance_km / input$speed * 60
    
    data$category <- cut(
      data$time_min,
      breaks = c(-Inf, 5, 10, 20, Inf),
      labels = c(
        "0-5 min",
        "5-10 min",
        "10-20 min",
        "20+ min"
      )
    )
    
    data
  })
  
  # 6. SUMMARY STATISTICS ----------------------------------
  
  output$total_sites <- renderText({
    nrow(sites)
  })
  
  output$mean_time <- renderText({
    
    data <- accessibility_data()
    
    if (is.null(data)) {
      return("Not available")
    }
    
    if (all(is.na(data$time_min))) {
      return("Not available")
    }
    
    paste0(
      round(
        mean(data$time_min, na.rm = TRUE),
        1
      ),
      " min"
    )
    
  })
  
  output$accessible_sites <- renderText({
    
    data <- accessibility_data()
    
    if (is.null(data)) {
      return("Not available")
    }
    
    sum(
      data$time_min <= as.numeric(input$threshold),
      na.rm = TRUE
    )
    
  })
  
  # 7. MAP COLORS -------------------------------------------
  
  time_labels <- c(
    "0-5 min",
    "5-10 min",
    "10-20 min",
    "20+ min"
  )
  
  time_colors <- c(
    "#2ECC71",  # Green: 0-5 minutes
    "#F1C40F",  # Yellow: 5-10 minutes
    "#E67E22",  # Orange: 10-20 minutes
    "#E74C3C"   # Red: 20+ minutes
  )
  
  time_palette <- leaflet::colorFactor(
    palette = time_colors,
    domain = time_labels,
    ordered = TRUE
  )
  

  
  # 8. INTERACTIVE MAP --------------------------------------
  
  output$rescue_map <- renderLeaflet({
    
    map <- leaflet(
      options = leafletOptions(preferCanvas = TRUE)
    ) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(
        lng = -157.95,
        lat = 21.48,
        zoom = 10
      )
    
    # Coast Guard: show available facility information.
    if (input$service == "Coast Guard Response") {
      
      if (!is.null(coast_guard_sf)) {
        
        map <- map |>
          addCircleMarkers(
            data = coast_guard_sf,
            radius = 8,
            color = "purple",
            fillOpacity = 0.9,
            popup = "Coast Guard facility"
          )
        
      }
      
      return(map)
    }
    
    data <- accessibility_data()
    
    map <- map |>
      addCircleMarkers(
        data = data,
        radius = 7,
        stroke = TRUE,
        weight = 1,
        color = ~time_palette(as.character(category)),
        fillColor = ~time_palette(as.character(category)),
        fillOpacity = 0.85,
        popup = ~paste0(
          "<b>Coastal Site</b>",
          "<br>Distance: ",
          round(distance_km, 2),
          " km",
          "<br>Hypothetical time: ",
          round(time_min, 1),
          " minutes",
          "<br>Category: ",
          category
        )
      )
    
    if (
      input$service == "Lifeguard Response" &&
      !is.null(lifeguards)
    ) {
      
      map <- map |>
        addCircleMarkers(
          data = lifeguards,
          radius = 5,
          color = "blue",
          fillOpacity = 0.9,
          group = "Lifeguard Towers"
        )
      
    }
    
    if (
      input$service == "Swimming Rescue" &&
      !is.null(shoreline)
    ) {
      
      map <- map |>
        addCircleMarkers(
          data = shoreline,
          radius = 4,
          color = "purple",
          fillOpacity = 0.8,
          group = "Shoreline Access"
        )
      
    }
    
    map |>
      addLegend(
        position = "bottomright",
        colors = time_colors,
        labels = time_labels,
        title = "Hypothetical Time",
        opacity = 1
      )
    
  })
  
  # 9. RESULTS TABLE ----------------------------------------
  
  output$results_table <- renderDT({
    
    if (input$service == "Coast Guard Response") {
      
      return(
        datatable(
          coast_guard,
          options = list(pageLength = 5),
          rownames = FALSE
        )
      )
      
    }
    
    data <- accessibility_data()
    
    table_data <- data |>
      st_drop_geometry() |>
      transmute(
        `Distance (km)` = round(distance_km, 2),
        `Time (min)` = round(time_min, 1),
        `Accessibility` = as.character(category)
      )
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  # 10. METHODOLOGY -----------------------------------------
  
  output$methodology <- renderUI({
    
    if (input$service == "Swimming Rescue") {
      
      tagList(
        h4("Swimming / Shoreline Accessibility"),
        p(
          "Distances are measured from coastal ",
          "recreation sites to the nearest mapped ",
          "public shoreline access point."
        ),
        p(
          "The selected speed is hypothetical. ",
          "The model does not account for currents, ",
          "waves, land barriers, or rescue equipment."
        )
      )
      
    } else if (
      input$service == "Lifeguard Response"
    ) {
      
      tagList(
        h4("Lifeguard Accessibility"),
        p(
          "Distances are measured from coastal ",
          "recreation sites to the nearest mapped ",
          "lifeguard tower."
        ),
        p(
          "This is a straight-line proximity model, ",
          "not a measured lifeguard response-time model."
        )
      )
      
    } else {
      
      tagList(
        h4("Coast Guard Accessibility"),
        p(
          "Coast Guard facility information is shown ",
          "when available."
        ),
        p(
          "Marine response-time estimates are not ",
          "available until operational departure points ",
          "and navigable marine routes are verified."
        ),
        p(
          "Facility coordinates, if provided, ",
          "must not be assumed to represent ",
          "boat-departure locations."
        )
      )
      
    }
    
  })
  
}

# 11. LAUNCH APP --------------------------------------------

shinyApp(
  ui = ui,
  server = server
)