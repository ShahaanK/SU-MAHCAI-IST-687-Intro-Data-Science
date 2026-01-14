#
# South Carolina Energy Usage Prediction Shiny App
# IST 687 Final Project
#
# This application allows users to explore energy usage patterns
# and predict household energy consumption based on various factors.
#

library(shiny)
library(shinythemes)
library(arrow)
library(tidyverse)
library(lubridate)
library(caret)


house_static_df <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")

# Stratified sampling function
stratified_sample_picker <- function(df, s) {
  fraction <- s / nrow(df)
  sampled_data <- df %>% 
    group_by(in.county) %>% 
    sample_frac(fraction)
  return(sampled_data)
}

# House energy picker function
house_energy_picker <- function(identifier) {
  tempFileStart <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/"
  tempFilePicked <- identifier
  tempFileEnd <- ".parquet"
  
  tempDF <- read_parquet(paste0(tempFileStart, tempFilePicked, tempFileEnd))
  tempDF$time <- as.POSIXct(tempDF$time)
  
  tempDF <- tempDF %>% 
    filter(time >= as.POSIXct("2018-07-01 00:00:00") & time <= as.POSIXct("2018-07-31 23:00:00"))  
  
  return(tempDF)
}

# House weather picker function
house_weather_picker <- function(countyCode) {
  tempFileStart <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/"
  tempFilePicked <- countyCode
  tempFileEnd <- ".csv"
  
  tempDF <- read_csv(paste0(tempFileStart, tempFilePicked, tempFileEnd), show_col_types = FALSE)
  tempDF <- tempDF %>% rename(time = date_time)
  tempDF$time <- as.POSIXct(tempDF$time)
  
  tempDF <- tempDF %>% 
    filter(time >= as.POSIXct("2018-07-01 00:00:00") & time <= as.POSIXct("2018-07-31 23:00:00"))
  
  tempDF$in.county <- countyCode
  return(tempDF)
}

# Process sample function
process_sample <- function(sampled_df) {
  counties_needed <- unique(sampled_df$in.county)
  weather_list <- list()
  
  for (county in counties_needed) {
    weather_list[[county]] <- house_weather_picker(county)
  }
  all_weather <- bind_rows(weather_list)
  
  house_ids <- sampled_df$bldg_id
  energy_list <- list()
  
  for (i in seq_along(house_ids)) {
    energy_df <- house_energy_picker(house_ids[i])
    energy_df$bldg_id <- house_ids[i]
    energy_list[[i]] <- energy_df
  }
  all_energy <- bind_rows(energy_list)
  
  house_county_map <- sampled_df %>% select(bldg_id, in.county)
  all_energy <- all_energy %>% left_join(house_county_map, by = "bldg_id")
  merged_E_W <- all_energy %>% left_join(all_weather, by = c("in.county", "time"))
  combined_df <- merged_E_W %>% left_join(house_static_df, by = c("bldg_id", "in.county"))
  
  return(combined_df)
}

# Interaction calculation function
interaction_calc <- function(df) {
  df$temp_x_insulation_wall <- df$`Dry Bulb Temperature [°C]` * df$in.insulation_wall
  df$temp_x_insulation_ceiling <- df$`Dry Bulb Temperature [°C]` * df$in.insulation_ceiling
  df$temp_x_hvac <- df$`Dry Bulb Temperature [°C]` * df$in.hvac_cooling_efficiency
  df$temp_x_windows <- df$`Dry Bulb Temperature [°C]` * df$in.windows
  df$temp_x_window_area <- df$`Dry Bulb Temperature [°C]` * df$in.window_areas
  df$hour_x_temp <- df$hour * df$`Dry Bulb Temperature [°C]`
  df$temp_x_humidity <- df$`Dry Bulb Temperature [°C]` * df$`Relative Humidity [%]`
  df$solar_x_windows <- df$`Global Horizontal Radiation [W/m2]` * df$in.windows
  df$solar_x_window_area <- df$`Global Horizontal Radiation [W/m2]` * df$in.window_areas
  return(df)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(
    div(
      h2("eSC Energy Usage Prediction Tool", style = "margin-bottom: 5px;"),
      h5("IST 687 Final Project - Analyzing Residential Energy Consumption", 
         style = "color: #666; margin-top: 0;")
    )
  ),
  
  # Tabbed interface
  tabsetPanel(
    tabPanel(
      "Data Explorer",
      icon = icon("database"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Data Loading Options"),
          hr(),
          
          numericInput(
            "sample_size",
            "Sample Size (number of houses):",
            value = 50,
            min = 10,
            max = 300,
            step = 10
          ),
          
          helpText("Note: Larger samples take longer to load but provide more representative data."),
          
          actionButton("load_data", "Load Data", 
                       class = "btn-primary", 
                       icon = icon("download")),
          
          hr(),
          
          h4("Display Options"),
          
          sliderInput(
            "n_rows",
            "Number of rows to display:",
            min = 5,
            max = 100,
            value = 10
          ),
          
          selectInput(
            "columns_display",
            "Select columns to display:",
            choices = NULL,
            multiple = TRUE
          )
        ),
        
        mainPanel(
          width = 9,
          
          h4("Dataset Preview"),
          
          conditionalPanel(
            condition = "!output.data_loaded",
            div(
              class = "alert alert-info",
              icon("info-circle"),
              " Click 'Load Data' to fetch the energy usage dataset. This may take a few moments."
            )
          ),
          
          verbatimTextOutput("data_summary"),
          
          hr(),
          
          h5("First N Rows of Dataset:"),
          div(style = "overflow-x: auto;",
              tableOutput("data_preview")
          )
        )
      )
    ),

    tabPanel(
      "Key Drivers",
      icon = icon("chart-line"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Visualization Options"),
          hr(),
          
          selectInput(
            "viz_type",
            "Select Visualization:",
            choices = c(
              "Peak Demand by Hour" = "peak_hour",
              "Usage by Wall Insulation" = "insulation",
              "Usage by HVAC Efficiency" = "hvac",
              "Usage by Window Type" = "windows",
              "Usage by Temperature" = "temperature",
              "Usage by Home Size" = "home_size",
              "Home Size vs HVAC (Faceted)" = "size_hvac"
            )
          ),
          
          hr(),
          
          h5("About This Visualization"),
          uiOutput("viz_description")
        ),
        
        mainPanel(
          width = 9,
          
          conditionalPanel(
            condition = "!output.data_loaded",
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " Please load data first in the 'Data Explorer' tab."
            )
          ),
          
          plotOutput("driver_plot", height = "500px"),
          
          hr(),
          
          h4("Key Insights"),
          uiOutput("viz_insights")
        )
      )
    ),
    
    tabPanel(
      "Predict Energy Usage",
      icon = icon("calculator"),
      
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("House Characteristics"),
          hr(),
          
          fluidRow(
            column(6,
                   h5("Weather Conditions"),
                   sliderInput("pred_temp", "Temperature (°C):", 
                               min = 15, max = 40, value = 28, step = 1),
                   sliderInput("pred_humidity", "Humidity (%):", 
                               min = 30, max = 100, value = 70, step = 5),
                   sliderInput("pred_solar", "Solar Radiation (W/m2):", 
                               min = 0, max = 1000, value = 600, step = 50),
                   sliderInput("pred_wind", "Wind Speed (m/s):", 
                               min = 0, max = 15, value = 3, step = 0.5)
            ),
            column(6,
                   h5("Time"),
                   sliderInput("pred_hour", "Hour of Day:", 
                               min = 0, max = 23, value = 15, step = 1),
                   selectInput("pred_day", "Day of Week:",
                               choices = c("Sunday" = 1, "Monday" = 2, "Tuesday" = 3,
                                           "Wednesday" = 4, "Thursday" = 5, 
                                           "Friday" = 6, "Saturday" = 7),
                               selected = 3)
            )
          ),
          
          hr(),
          
          fluidRow(
            column(6,
                   h5("Insulation Levels"),
                   sliderInput("pred_ceiling", "Ceiling Insulation (1-8):", 
                               min = 1, max = 8, value = 5, step = 1),
                   sliderInput("pred_wall", "Wall Insulation (1-16):", 
                               min = 1, max = 16, value = 8, step = 1),
                   sliderInput("pred_floor", "Floor Insulation (1-5):", 
                               min = 1, max = 5, value = 3, step = 1),
                   sliderInput("pred_foundation", "Foundation Wall (1-5):", 
                               min = 1, max = 5, value = 2, step = 1),
                   sliderInput("pred_slab", "Slab Insulation (1-6):", 
                               min = 1, max = 6, value = 2, step = 1)
            ),
            column(6,
                   h5("House Features"),
                   sliderInput("pred_windows", "Window Quality (1-11):", 
                               min = 1, max = 11, value = 6, step = 1),
                   sliderInput("pred_window_area", "Window Area (1-6):", 
                               min = 1, max = 6, value = 3, step = 1),
                   sliderInput("pred_ducts", "Duct Quality (1-14):", 
                               min = 1, max = 14, value = 7, step = 1),
                   sliderInput("pred_hvac", "HVAC Efficiency (1-10):", 
                               min = 1, max = 10, value = 5, step = 1),
                   sliderInput("pred_bedrooms", "Bedrooms:", 
                               min = 1, max = 6, value = 3, step = 1),
                   sliderInput("pred_stories", "Stories:", 
                               min = 1, max = 3, value = 2, step = 1)
            )
          ),
          
          hr(),
          
          fluidRow(
            column(6,
                   h5("Additional Features"),
                   selectInput("pred_cooling_setpoint", "Cooling Setpoint:",
                               choices = c("72F", "75F", "78F"),
                               selected = "72F"),
                   selectInput("pred_occupants", "Number of Occupants:",
                               choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"),
                               selected = "3"),
                   selectInput("pred_vintage", "Home Vintage:",
                               choices = c("<1940", "1940s", "1950s", "1960s", "1970s", 
                                           "1980s", "1990s", "2000s", "2010s"),
                               selected = "1990s")
            ),
            column(6,
                   h5("More Options"),
                   selectInput("pred_infiltration", "Air Infiltration:",
                               choices = c("0.25 ACH50", "0.5 ACH50", "0.75 ACH50", "1 ACH50",
                                           "1.5 ACH50", "2 ACH50", "3 ACH50", "4 ACH50",
                                           "5 ACH50", "6 ACH50", "7 ACH50", "8 ACH50",
                                           "10 ACH50", "15 ACH50", "20 ACH50", "25 ACH50"),
                               selected = "5 ACH50"),
                   selectInput("pred_orientation", "Home Orientation:",
                               choices = c("North", "Northeast", "East", "Southeast",
                                           "South", "Southwest", "West", "Northwest"),
                               selected = "South"),
                   selectInput("pred_garage", "Garage Type:",
                               choices = c("None", "1 Car", "2 Car", "3 Car"),
                               selected = "2 Car")
            )
          ),
          
          hr(),
          
          actionButton("predict", "Generate Prediction", 
                       class = "btn-success btn-lg", 
                       icon = icon("bolt"),
                       style = "width: 100%;")
        ),
        
        mainPanel(
          width = 8,
          
          conditionalPanel(
            condition = "!output.model_ready",
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " Please load data first in the 'Data Explorer' tab to train the prediction model."
            )
          ),
          
          h4("Prediction Results"),
          
          div(
            class = "well",
            style = "background-color: #e8f4f8; text-align: center; padding: 30px;",
            h2(textOutput("prediction_result"), style = "color: #2c3e50;"),
            p("Predicted hourly energy usage", style = "color: #666;")
          ),
          
          hr(),
          
          fluidRow(
            column(6,
                   h4("Scenario Comparison"),
                   plotOutput("scenario_plot", height = "300px")
            ),
            column(6,
                   h4("Feature Impact"),
                   plotOutput("feature_impact_plot", height = "300px")
            )
          ),
          
          hr(),
          
          h4("Model Interpretation Guide"),
          div(
            class = "panel panel-info",
            div(class = "panel-heading", 
                icon("info-circle"), " How to Interpret the Results"),
            div(
              class = "panel-body",
              
              h5("Understanding the Prediction:"),
              tags$ul(
                tags$li(strong("Predicted Usage (kWh):"), " This is the estimated electricity consumption for one hour based on the specified conditions."),
                tags$li(strong("Higher values"), " indicate greater energy demand, typically during hot afternoon hours with older/less efficient equipment."),
                tags$li(strong("Lower values"), " suggest better energy efficiency from improved insulation, modern HVAC, or cooler conditions.")
              ),
              
              hr(),
              
              h5("Key Factors Affecting Energy Usage:"),
              tags$ul(
                tags$li(strong("Temperature:"), " Each 1C increase can raise cooling demand by 3-5%. The model uses interaction terms to capture how temperature effects vary by insulation quality."),
                tags$li(strong("Hour of Day:"), " Peak usage typically occurs between 3-7 PM when temperatures are highest and occupants return home."),
                tags$li(strong("Insulation (Wall & Ceiling):"), " Higher R-values (higher numbers) reduce heat transfer, lowering cooling needs. Ceiling insulation is especially impactful in SC's hot climate."),
                tags$li(strong("HVAC Efficiency:"), " Modern heat pumps (rating 10) use 40-60% less energy than older room AC units (rating 1-3)."),
                tags$li(strong("Windows:"), " Double/triple-pane Low-E windows (8-11) significantly reduce solar heat gain compared to single-pane (1-4).")
              ),
              
              hr(),
              
              h5("Interaction Effects:"),
              p("The model captures how factors work together:"),
              tags$ul(
                tags$li("Well-insulated homes show ", em("smaller"), " usage increases when temperatures rise."),
                tags$li("Efficient HVAC systems provide ", em("greater"), " benefits during extreme heat."),
                tags$li("Solar radiation impact depends on window quality - better windows reduce heat gain.")
              ),
              
              hr(),
              
              h5("Using This for Planning:"),
              p("Compare scenarios by adjusting inputs to see potential savings from:"),
              tags$ul(
                tags$li("Upgrading insulation"),
                tags$li("Installing efficient HVAC"),
                tags$li("Replacing windows"),
                tags$li("Understanding peak vs. off-peak usage patterns")
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Model Info",
      icon = icon("info-circle"),
      
      fluidRow(
        column(8, offset = 2,
               
               h3("About the Prediction Model"),
               hr(),
               
               div(
                 class = "panel panel-default",
                 div(class = "panel-heading", h4("Model Overview")),
                 div(
                   class = "panel-body",
                   p("This application uses a ", strong("multiple linear regression model with interaction terms"), 
                     " to predict residential energy consumption in South Carolina."),
                   
                   h5("Target Variable:"),
                   tags$ul(
                     tags$li(code("total_usage"), " - Total hourly electricity consumption (kWh)")
                   ),
                   
                   h5("Model Features:"),
                   tags$ul(
                     tags$li(strong("Weather:"), " Temperature, humidity, solar radiation, wind speed"),
                     tags$li(strong("Time:"), " Hour of day, day of week"),
                     tags$li(strong("Building Envelope:"), " Ceiling, wall, floor, foundation, slab insulation"),
                     tags$li(strong("Windows:"), " Window type/quality, window area"),
                     tags$li(strong("HVAC:"), " Cooling efficiency, cooling setpoint, duct quality"),
                     tags$li(strong("House Characteristics:"), " Bedrooms, stories, occupants, vintage, garage, orientation, infiltration")
                   ),
                   
                   h5("Interaction Terms:"),
                   p("The model includes interaction terms to capture non-linear relationships:"),
                   tags$ul(
                     tags$li(code("temp x insulation_wall"), " - How wall insulation moderates temperature effects"),
                     tags$li(code("temp x insulation_ceiling"), " - Ceiling insulation's temperature interaction"),
                     tags$li(code("temp x hvac_efficiency"), " - HVAC performance under varying temperatures"),
                     tags$li(code("temp x windows"), " - Window quality's impact on heat transfer"),
                     tags$li(code("hour x temp"), " - Time-of-day temperature patterns"),
                     tags$li(code("solar x windows"), " - Solar gain through different window types")
                   )
                 )
               ),
               
               div(
                 class = "panel panel-default",
                 div(class = "panel-heading", h4("Data Source")),
                 div(
                   class = "panel-body",
                   p("The model is trained on simulated residential building data from the ", 
                     strong("ResStock dataset"), " for South Carolina, including:"),
                   tags$ul(
                     tags$li("Static house characteristics (building ID, county, construction details)"),
                     tags$li("Hourly energy consumption data for July 2018"),
                     tags$li("Corresponding weather data by county")
                   ),
                   p("Data is stratified by county to ensure geographic representation across the state.")
                 )
               ),
               
               div(
                 class = "panel panel-default",
                 div(class = "panel-heading", h4("Model Performance")),
                 div(
                   class = "panel-body",
                   verbatimTextOutput("model_summary"),
                   hr(),
                   p(em("Note: Model statistics are calculated after data is loaded."))
                 )
               ),
               
               div(
                 class = "panel panel-default",
                 div(class = "panel-heading", h4("Limitations")),
                 div(
                   class = "panel-body",
                   tags$ul(
                     tags$li("Model is trained on July data (peak cooling season) - may not generalize to heating season"),
                     tags$li("Simulated data may not capture all real-world variability"),
                     tags$li("Sample size affects model stability"),
                     tags$li("Categorical variables are converted to ordinal scales which assumes linear relationships"),
                     tags$li("Future climate scenarios (+5C) are extrapolations beyond training data")
                   )
                 )
               )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive values to store data and model
  rv <- reactiveValues(
    data = NULL,
    model = NULL,
    data_loaded = FALSE,
    model_ready = FALSE
  )
  
  observeEvent(input$load_data, {
    
    showModal(modalDialog(
      title = "Loading Data",
      "Fetching and processing energy data... This may take 1-2 minutes.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      # Sample and process data
      set.seed(42)
      sampled_data <- stratified_sample_picker(house_static_df, input$sample_size)
      mergedDF <- process_sample(sampled_data)
      
      # Factor encoding for ordinal variables
      roof_order <- c("Unfinished, Uninsulated", "Finished, Uninsulated", "Finished, R-7",
                      "Finished, R-13", "Finished, R-19", "Finished, R-30", 
                      "Finished, R-38", "Finished, R-49")
      mergedDF$in.insulation_roof <- as.numeric(factor(mergedDF$in.insulation_roof, levels = roof_order))
      
      ceiling_order <- c("None", "Uninsulated", "R-7", "R-13", "R-19", "R-30", "R-38", "R-49")
      mergedDF$in.insulation_ceiling <- as.numeric(factor(mergedDF$in.insulation_ceiling, levels = ceiling_order))
      
      floor_order <- c("None", "Uninsulated", "Ceiling R-13", "Ceiling R-19", "Ceiling R-30")
      mergedDF$in.insulation_floor <- as.numeric(factor(mergedDF$in.insulation_floor, levels = floor_order))
      
      found_wall_order <- c("None", "Uninsulated", "Wall R-5, Exterior", 
                            "Wall R-10, Exterior", "Wall R-15, Exterior")
      mergedDF$in.insulation_foundation_wall <- as.numeric(factor(mergedDF$in.insulation_foundation_wall, 
                                                                  levels = found_wall_order))
      
      wall_order <- c("None", "Wood Stud, Uninsulated", "CMU, 6-in Hollow, Uninsulated",
                      "Brick, 12-in, 3-wythe, Uninsulated", "Wood Stud, R-7", 
                      "CMU, 6-in Hollow, R-7", "Brick, 12-in, 3-wythe, R-7",
                      "Wood Stud, R-11", "CMU, 6-in Hollow, R-11", "Brick, 12-in, 3-wythe, R-11",
                      "Wood Stud, R-15", "CMU, 6-in Hollow, R-15", "Brick, 12-in, 3-wythe, R-15",
                      "Wood Stud, R-19", "CMU, 6-in Hollow, R-19", "Brick, 12-in, 3-wythe, R-19")
      mergedDF$in.insulation_wall <- as.numeric(factor(mergedDF$in.insulation_wall, levels = wall_order))
      
      slab_order <- c("None", "Uninsulated", "2ft R5 Under, Horizontal", 
                      "2ft R5 Perimeter, Vertical", "2ft R10 Under, Horizontal", 
                      "2ft R10 Perimeter, Vertical")
      mergedDF$in.insulation_slab <- as.numeric(factor(mergedDF$in.insulation_slab, levels = slab_order))
      
      window_order <- c("None", "Single, Clear, Metal", "Single, Clear, Non-metal",
                        "Single, Clear, Metal, Exterior Clear Storm",
                        "Single, Clear, Non-metal, Exterior Clear Storm",
                        "Double, Clear, Metal, Air", "Double, Clear, Non-metal, Air",
                        "Double, Clear, Metal, Air, Exterior Clear Storm",
                        "Double, Clear, Non-metal, Air, Exterior Clear Storm",
                        "Double, Low-E, Non-metal, Air, M-Gain",
                        "Triple, Low-E, Non-metal, Air, L-Gain")
      mergedDF$in.windows <- as.numeric(factor(mergedDF$in.windows, levels = window_order))
      
      duct_order <- c("None", "30% Leakage, Uninsulated", "30% Leakage, R-4",
                      "30% Leakage, R-6", "30% Leakage, R-8", "20% Leakage, Uninsulated",
                      "20% Leakage, R-4", "20% Leakage, R-6", "20% Leakage, R-8",
                      "10% Leakage, Uninsulated", "10% Leakage, R-4", "10% Leakage, R-6",
                      "10% Leakage, R-8", "0% Leakage, Uninsulated")
      mergedDF$in.ducts <- as.numeric(factor(mergedDF$in.ducts, levels = duct_order))
      
      cooling_order <- c("None", "Room AC, EER 8.5", "Room AC, EER 9.8", "AC, SEER 8",
                         "Room AC, EER 10.7", "AC, SEER 10", "Room AC, EER 12.0",
                         "AC, SEER 13", "AC, SEER 15", "Heat Pump")
      mergedDF$in.hvac_cooling_efficiency <- as.numeric(factor(mergedDF$in.hvac_cooling_efficiency, 
                                                               levels = cooling_order))
      
      window_area_order <- c("F6 B6 L6 R6", "F9 B9 L9 R9", "F12 B12 L12 R12",
                             "F15 B15 L15 R15", "F18 B18 L18 R18", "F30 B30 L30 R30")
      mergedDF$in.window_areas <- as.numeric(factor(mergedDF$in.window_areas, levels = window_area_order))
      
      # Time features
      mergedDF$hour <- hour(mergedDF$time)
      mergedDF$day_of_week <- wday(mergedDF$time)
      
      # Size categories
      mergedDF$size_category <- cut(mergedDF$in.sqft,
                                    breaks = c(0, 1500, 2500, 3500, Inf),
                                    labels = c("Small", "Medium", "Large", "XLarge"))
      
      # Temperature categories
      mergedDF$temp_category <- cut(mergedDF$`Dry Bulb Temperature [°C]`,
                                    breaks = c(-Inf, 22, 27, 32, Inf),
                                    labels = c("Cool (<22C)", "Mild (22-27C)", 
                                               "Warm (27-32C)", "Hot (>32C)"))
      
      # Insulation categories
      mergedDF$insulation_category <- cut(mergedDF$in.insulation_wall,
                                          breaks = c(0, 5, 10, 16),
                                          labels = c("Low (1-5)", "Medium (6-10)", "High (11-16)"))
      
      # HVAC categories
      mergedDF$hvac_category <- cut(mergedDF$in.hvac_cooling_efficiency,
                                    breaks = c(0, 3, 6, 10),
                                    labels = c("Low (1-3)", "Medium (4-6)", "High (7-10)"))
      
      # Window categories
      mergedDF$window_category <- cut(mergedDF$in.windows,
                                      breaks = c(0, 4, 7, 11),
                                      labels = c("Single Pane (1-4)", "Double Pane (5-7)", 
                                                 "Double/Triple Low-E (8-11)"))
      
      # Calculate total usage
      mergedDF$total_usage <- rowSums(select(mergedDF, starts_with("out.electricity.")), na.rm = TRUE)
      
      # Calculate interaction terms
      mergedDF <- interaction_calc(mergedDF)
      
      # Store processed data
      rv$data <- mergedDF
      rv$data_loaded <- TRUE
      
      # Update column selector
      updateSelectInput(session, "columns_display",
                        choices = names(mergedDF),
                        selected = c("bldg_id", "in.county", "total_usage", 
                                     "Dry Bulb Temperature [°C]", "hour", "in.sqft"))
      
      # Train the model - using explicit formula without in.county and bldg_id
      # This avoids the issue of needing those columns for prediction
      rv$model <- lm(total_usage ~ 
                       `Dry Bulb Temperature [°C]` + 
                       `Relative Humidity [%]` +
                       `Global Horizontal Radiation [W/m2]` +
                       `Wind Speed [m/s]` +
                       hour + day_of_week +
                       in.insulation_ceiling + in.insulation_wall + in.insulation_floor +
                       in.insulation_foundation_wall + in.insulation_slab +
                       in.windows + in.window_areas + in.ducts + in.hvac_cooling_efficiency +
                       in.bedrooms + in.geometry_stories + in.occupants +
                       temp_x_insulation_wall + temp_x_insulation_ceiling + temp_x_hvac +
                       temp_x_windows + temp_x_window_area + hour_x_temp + temp_x_humidity +
                       solar_x_windows + solar_x_window_area +
                       in.cooling_setpoint + in.infiltration + in.orientation +
                       in.roof_material + in.vintage + in.geometry_garage,
                     data = mergedDF)
      
      rv$model_ready <- TRUE
      
      removeModal()
      
      showNotification("Data loaded and model trained successfully!", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error loading data:", e$message), 
                       type = "error", duration = 10)
    })
  })
  
  output$data_loaded <- reactive({ rv$data_loaded })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({ rv$model_ready })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)
  
  output$data_summary <- renderPrint({
    req(rv$data)
    cat("Dataset Summary\n")
    cat("===============\n\n")
    cat("Total observations:", nrow(rv$data), "\n")
    cat("Number of houses:", n_distinct(rv$data$bldg_id), "\n")
    cat("Number of counties:", n_distinct(rv$data$in.county), "\n")
    cat("Date range:", as.character(min(rv$data$time)), "to", as.character(max(rv$data$time)), "\n")
    cat("Total columns:", ncol(rv$data), "\n\n")
    cat("Average total usage:", round(mean(rv$data$total_usage, na.rm = TRUE), 3), "kWh\n")
    cat("Max total usage:", round(max(rv$data$total_usage, na.rm = TRUE), 3), "kWh\n")
  })
  
  output$data_preview <- renderTable({
    req(rv$data)
    
    cols <- input$columns_display
    if (is.null(cols) || length(cols) == 0) {
      cols <- c("bldg_id", "in.county", "total_usage", "hour")
    }
    
    rv$data %>%
      select(all_of(cols)) %>%
      head(input$n_rows)
  })
  
  output$viz_description <- renderUI({
    desc <- switch(input$viz_type,
                   "peak_hour" = "Shows average energy usage across all hours of the day, identifying when peak demand occurs.",
                   "insulation" = "Compares hourly energy usage across different wall insulation levels (Low, Medium, High R-values).",
                   "hvac" = "Shows how HVAC cooling efficiency affects energy consumption throughout the day.",
                   "windows" = "Demonstrates the impact of window quality on reducing energy usage.",
                   "temperature" = "Illustrates how outdoor temperature drives cooling energy demand.",
                   "home_size" = "Compares energy usage patterns across different home sizes.",
                   "size_hvac" = "Combined view showing how home size and HVAC efficiency interact to affect usage.")
    p(desc)
  })
  
  output$driver_plot <- renderPlot({
    req(rv$data)
    
    df <- rv$data
    
    if (input$viz_type == "peak_hour") {
      peak_data <- df %>%
        group_by(hour) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(peak_data, aes(x = hour, y = avg_usage)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_point(color = "steelblue", size = 3) +
        geom_area(alpha = 0.2, fill = "steelblue") +
        geom_vline(xintercept = peak_data$hour[which.max(peak_data$avg_usage)], 
                   linetype = "dashed", color = "red", linewidth = 1) +
        labs(title = "Peak Demand Per Hour (Average Across All Counties)",
             subtitle = "July 2018 - Identifying Peak Usage Hours",
             x = "Hour of Day", y = "Average Usage (kWh)") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14))
      
    } else if (input$viz_type == "insulation") {
      insulation_data <- df %>%
        filter(!is.na(insulation_category)) %>%
        group_by(hour, insulation_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(insulation_data, aes(x = hour, y = avg_usage, color = insulation_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Low (1-5)" = "red", "Medium (6-10)" = "orange", 
                                      "High (11-16)" = "green")) +
        labs(title = "Average Hourly Usage by Wall Insulation Level",
             subtitle = "Better insulation (higher rating) reduces energy consumption",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "Wall Insulation") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")
      
    } else if (input$viz_type == "hvac") {
      hvac_data <- df %>%
        filter(!is.na(hvac_category)) %>%
        group_by(hour, hvac_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(hvac_data, aes(x = hour, y = avg_usage, color = hvac_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Low (1-3)" = "red", "Medium (4-6)" = "orange", 
                                      "High (7-10)" = "green")) +
        labs(title = "Average Hourly Usage by HVAC Cooling Efficiency",
             subtitle = "Higher efficiency systems use less energy",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "HVAC Efficiency") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")
      
    } else if (input$viz_type == "windows") {
      window_data <- df %>%
        filter(!is.na(window_category)) %>%
        group_by(hour, window_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(window_data, aes(x = hour, y = avg_usage, color = window_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Single Pane (1-4)" = "red", "Double Pane (5-7)" = "orange", 
                                      "Double/Triple Low-E (8-11)" = "green")) +
        labs(title = "Average Hourly Usage by Window Type",
             subtitle = "Better windows (double/triple pane, Low-E) reduce heat gain",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "Window Type") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")
      
    } else if (input$viz_type == "temperature") {
      weather_data <- df %>%
        filter(!is.na(temp_category)) %>%
        group_by(hour, temp_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(weather_data, aes(x = hour, y = avg_usage, color = temp_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Cool (<22C)" = "blue", "Mild (22-27C)" = "green",
                                      "Warm (27-32C)" = "orange", "Hot (>32C)" = "red")) +
        labs(title = "Average Hourly Usage by Temperature",
             subtitle = "Higher temperatures drive increased cooling demand",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "Temperature") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")
      
    } else if (input$viz_type == "home_size") {
      size_data <- df %>%
        filter(!is.na(size_category)) %>%
        group_by(hour, size_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(size_data, aes(x = hour, y = avg_usage, color = size_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Small" = "green", "Medium" = "blue", 
                                      "Large" = "orange", "XLarge" = "red")) +
        labs(title = "Average Hourly Usage by Home Size",
             subtitle = "Larger homes consume more energy",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "Home Size") +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")
      
    } else if (input$viz_type == "size_hvac") {
      size_hvac_data <- df %>%
        filter(!is.na(size_category) & !is.na(hvac_category)) %>%
        group_by(hour, size_category, hvac_category) %>%
        summarise(avg_usage = mean(total_usage, na.rm = TRUE), .groups = "drop")
      
      ggplot(size_hvac_data, aes(x = hour, y = avg_usage, color = hvac_category)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        facet_wrap(~size_category, ncol = 2, scales = "free_y") +
        scale_color_manual(values = c("Low (1-3)" = "red", "Medium (4-6)" = "orange", 
                                      "High (7-10)" = "green")) +
        labs(title = "Average Hourly Usage: Home Size vs HVAC Efficiency",
             subtitle = "Each panel shows a home size category; lines show HVAC efficiency levels",
             x = "Hour of Day", y = "Average Usage (kWh)", color = "HVAC Efficiency") +
        scale_x_continuous(breaks = seq(0, 23, 4)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14),
              strip.text = element_text(face = "bold", size = 11),
              legend.position = "bottom")
    }
  })
  
  output$viz_insights <- renderUI({
    insights <- switch(input$viz_type,
                       "peak_hour" = tagList(
                         tags$ul(
                           tags$li("Peak demand typically occurs between 3-7 PM"),
                           tags$li("Usage is lowest during early morning hours (2-6 AM)"),
                           tags$li("Afternoon peaks align with highest outdoor temperatures and return-from-work patterns")
                         )
                       ),
                       "insulation" = tagList(
                         tags$ul(
                           tags$li("Homes with high insulation (R-11 to R-19) use significantly less energy"),
                           tags$li("The insulation benefit is most pronounced during peak hours"),
                           tags$li("Upgrading from low to high insulation can reduce usage by 15-25%")
                         )
                       ),
                       "hvac" = tagList(
                         tags$ul(
                           tags$li("High-efficiency HVAC systems (SEER 13+, Heat Pumps) use 30-50% less energy"),
                           tags$li("The efficiency gap widens during hot afternoon hours"),
                           tags$li("Upgrading HVAC provides consistent savings across all hours")
                         )
                       ),
                       "windows" = tagList(
                         tags$ul(
                           tags$li("Double/Triple Low-E windows significantly reduce solar heat gain"),
                           tags$li("Single pane windows allow much more heat transfer"),
                           tags$li("Window upgrades show greatest impact during sunny afternoon hours")
                         )
                       ),
                       "temperature" = tagList(
                         tags$ul(
                           tags$li("Energy usage increases dramatically above 27C"),
                           tags$li("Hot days (>32C) can double energy consumption vs. cool days"),
                           tags$li("Temperature is the primary driver of summer energy demand")
                         )
                       ),
                       "home_size" = tagList(
                         tags$ul(
                           tags$li("XLarge homes use 2-3x more energy than small homes"),
                           tags$li("The size effect is consistent across all hours"),
                           tags$li("Larger homes have more volume to cool and more appliances")
                         )
                       ),
                       "size_hvac" = tagList(
                         tags$ul(
                           tags$li("HVAC efficiency matters more for larger homes"),
                           tags$li("Small homes with efficient HVAC can use less than large homes with inefficient systems"),
                           tags$li("The combination of home size and HVAC efficiency shows non-linear effects")
                         )
                       )
    )
    insights
  })
  
  prediction_value <- reactiveVal(NULL)
  
  # Creates prediction data
  create_prediction_data <- function(temp, humidity, solar, wind, hour_val, day_week,
                                     ceiling, wall, floor, foundation, slab,
                                     windows, window_area, ducts, hvac,
                                     bedrooms, stories, occupants,
                                     cooling_setpoint, infiltration, orientation,
                                     vintage, garage) {
    
    new_data <- data.frame(
      `Dry Bulb Temperature [°C]` = temp,
      `Relative Humidity [%]` = humidity,
      `Global Horizontal Radiation [W/m2]` = solar,
      `Wind Speed [m/s]` = wind,
      hour = hour_val,
      day_of_week = day_week,
      in.insulation_ceiling = ceiling,
      in.insulation_wall = wall,
      in.insulation_floor = floor,
      in.insulation_foundation_wall = foundation,
      in.insulation_slab = slab,
      in.windows = windows,
      in.window_areas = window_area,
      in.ducts = ducts,
      in.hvac_cooling_efficiency = hvac,
      in.bedrooms = bedrooms,
      in.geometry_stories = stories,
      in.occupants = occupants,
      in.cooling_setpoint = cooling_setpoint,
      in.infiltration = infiltration,
      in.orientation = orientation,
      in.roof_material = "Asphalt Shingles, Medium",
      in.vintage = vintage,
      in.geometry_garage = garage,
      check.names = FALSE
    )
    
    # Calculate interaction terms
    new_data$temp_x_insulation_wall <- new_data$`Dry Bulb Temperature [°C]` * new_data$in.insulation_wall
    new_data$temp_x_insulation_ceiling <- new_data$`Dry Bulb Temperature [°C]` * new_data$in.insulation_ceiling
    new_data$temp_x_hvac <- new_data$`Dry Bulb Temperature [°C]` * new_data$in.hvac_cooling_efficiency
    new_data$temp_x_windows <- new_data$`Dry Bulb Temperature [°C]` * new_data$in.windows
    new_data$temp_x_window_area <- new_data$`Dry Bulb Temperature [°C]` * new_data$in.window_areas
    new_data$hour_x_temp <- new_data$hour * new_data$`Dry Bulb Temperature [°C]`
    new_data$temp_x_humidity <- new_data$`Dry Bulb Temperature [°C]` * new_data$`Relative Humidity [%]`
    new_data$solar_x_windows <- new_data$`Global Horizontal Radiation [W/m2]` * new_data$in.windows
    new_data$solar_x_window_area <- new_data$`Global Horizontal Radiation [W/m2]` * new_data$in.window_areas
    
    return(new_data)
  }
  
  observeEvent(input$predict, {
    req(rv$model)
    
    tryCatch({
      # Create prediction data frame
      new_data <- create_prediction_data(
        temp = input$pred_temp,
        humidity = input$pred_humidity,
        solar = input$pred_solar,
        wind = input$pred_wind,
        hour_val = input$pred_hour,
        day_week = as.numeric(input$pred_day),
        ceiling = input$pred_ceiling,
        wall = input$pred_wall,
        floor = input$pred_floor,
        foundation = input$pred_foundation,
        slab = input$pred_slab,
        windows = input$pred_windows,
        window_area = input$pred_window_area,
        ducts = input$pred_ducts,
        hvac = input$pred_hvac,
        bedrooms = input$pred_bedrooms,
        stories = input$pred_stories,
        occupants = input$pred_occupants,
        cooling_setpoint = input$pred_cooling_setpoint,
        infiltration = input$pred_infiltration,
        orientation = input$pred_orientation,
        vintage = input$pred_vintage,
        garage = input$pred_garage
      )
      
      # Generate prediction
      pred <- predict(rv$model, newdata = new_data)
      prediction_value(max(0, pred))  # Ensure non-negative
      
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
    })
  })
  
  output$prediction_result <- renderText({
    pred <- prediction_value()
    if (is.null(pred)) {
      "Enter parameters and click 'Generate Prediction'"
    } else {
      paste(round(pred, 3), "kWh")
    }
  })
  
  output$scenario_plot <- renderPlot({
    req(rv$model, prediction_value())
    
    # Create scenarios for different hours
    hours <- 0:23
    predictions <- sapply(hours, function(h) {
      tryCatch({
        new_data <- create_prediction_data(
          temp = input$pred_temp,
          humidity = input$pred_humidity,
          solar = input$pred_solar,
          wind = input$pred_wind,
          hour_val = h,
          day_week = as.numeric(input$pred_day),
          ceiling = input$pred_ceiling,
          wall = input$pred_wall,
          floor = input$pred_floor,
          foundation = input$pred_foundation,
          slab = input$pred_slab,
          windows = input$pred_windows,
          window_area = input$pred_window_area,
          ducts = input$pred_ducts,
          hvac = input$pred_hvac,
          bedrooms = input$pred_bedrooms,
          stories = input$pred_stories,
          occupants = input$pred_occupants,
          cooling_setpoint = input$pred_cooling_setpoint,
          infiltration = input$pred_infiltration,
          orientation = input$pred_orientation,
          vintage = input$pred_vintage,
          garage = input$pred_garage
        )
        max(0, predict(rv$model, newdata = new_data))
      }, error = function(e) NA)
    })
    
    scenario_df <- data.frame(hour = hours, predicted = predictions)
    
    ggplot(scenario_df, aes(x = hour, y = predicted)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      geom_vline(xintercept = input$pred_hour, linetype = "dashed", color = "red") +
      geom_point(data = data.frame(hour = input$pred_hour, predicted = prediction_value()),
                 aes(x = hour, y = predicted), color = "red", size = 4) +
      labs(title = "Predicted Usage Throughout the Day",
           subtitle = "Red point shows your selected hour",
           x = "Hour", y = "Predicted Usage (kWh)") +
      scale_x_continuous(breaks = seq(0, 23, 4)) +
      theme_minimal()
  })
  
  output$feature_impact_plot <- renderPlot({
    req(rv$model)
    
    # Get model coefficients for key features
    coefs <- coef(rv$model)
    
    key_features <- c(
      "Temperature" = "`Dry Bulb Temperature [°C]`",
      "HVAC Efficiency" = "in.hvac_cooling_efficiency",
      "Wall Insulation" = "in.insulation_wall",
      "Ceiling Insulation" = "in.insulation_ceiling",
      "Windows" = "in.windows",
      "Hour" = "hour"
    )
    
    impact_df <- data.frame(
      Feature = names(key_features),
      Coefficient = sapply(key_features, function(f) {
        if (f %in% names(coefs)) coefs[f] else 0
      })
    )
    
    impact_df$Direction <- ifelse(impact_df$Coefficient > 0, "Increases Usage", "Decreases Usage")
    impact_df$AbsCoef <- abs(impact_df$Coefficient)
    
    ggplot(impact_df, aes(x = reorder(Feature, AbsCoef), y = Coefficient, fill = Direction)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Increases Usage" = "coral", "Decreases Usage" = "steelblue")) +
      labs(title = "Feature Impact on Energy Usage",
           subtitle = "Coefficient magnitude (per unit change)",
           x = "", y = "Coefficient") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$model_summary <- renderPrint({
    if (rv$model_ready && !is.null(rv$model)) {
      cat("Model Performance Metrics\n")
      cat("=========================\n\n")
      
      s <- summary(rv$model)
      cat("R-squared:", round(s$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(s$adj.r.squared, 4), "\n")
      cat("Residual Std Error:", round(s$sigma, 4), "\n")
      cat("F-statistic:", round(s$fstatistic[1], 2), "\n")
      cat("Number of predictors:", length(coef(rv$model)) - 1, "\n")
      cat("\nTop 10 Most Significant Predictors:\n")
      
      coef_df <- as.data.frame(s$coefficients)
      coef_df$Variable <- rownames(coef_df)
      coef_df <- coef_df[order(coef_df$`Pr(>|t|)`), ]
      print(head(coef_df[, c("Variable", "Estimate", "Pr(>|t|)")], 10))
    } else {
      cat("Model not yet trained. Please load data first.")
    }
  })
}

shinyApp(ui = ui, server = server)