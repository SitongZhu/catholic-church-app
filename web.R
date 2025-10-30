library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(readr)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(shinyjs)
library(future)
library(promises)

# ---- Set global ggplot theme to ensure white background on all platforms ----
ggplot2::theme_set(ggplot2::theme_minimal() + 
                     ggplot2::theme(
                       plot.background = ggplot2::element_rect(fill = "white", color = NA),
                       panel.background = ggplot2::element_rect(fill = "white", color = NA)
                     ))


# ---- Load & clean data function ----
load_data <- function() {
  
  data_file <- "web_data.csv"
  
  data <- read.csv(data_file, stringsAsFactors = FALSE, check.names = FALSE)
  data <- data %>%
    dplyr::mutate(
      Country = trimws(as.character(Country)),
      url = if ("url" %in% names(.)) trimws(as.character(url)) else NA_character_,
      diocese = if ("diocese" %in% names(.)) trimws(as.character(diocese)) else NA_character_,
      Mailing.Address = if ("Mailing.Address" %in% names(.)) trimws(as.character(Mailing.Address)) else NA_character_,
      Rite = if ("Rite" %in% names(.)) trimws(as.character(Rite)) else NA_character_,
      Official.Web.Site = if ("Official.Web.Site" %in% names(.)) trimws(as.character(Official.Web.Site)) else NA_character_,
      Telephone = if ("Telephone" %in% names(.)) trimws(as.character(Telephone)) else NA_character_,
      Additional.Notes = if ("Additional.Notes" %in% names(.)) trimws(as.character(Additional.Notes)) else NA_character_
    )
  return(data)
}

# Initial data loading
data <- load_data()

# ---- World centroids (for map) ----
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
centroids <- st_centroid(world_map)
cent_df <- data.frame(
  Country = as.character(world_map$name),
  lon = st_coordinates(centroids)[, 1],
  lat = st_coordinates(centroids)[, 2],
  stringsAsFactors = FALSE
)

# ---- Variable definitions ----
var_info <- list(
  "Catholic Population" = list(bases = "Catholics", flags = "Catholics_imputed", compute = function(s) s$Catholics),
  "Total Population" = list(bases = "Total.Population", flags = "Total.Population_imputed", compute = function(s) s$Total.Population),
  "Percentage of Catholics" = list(
    bases = c("Catholics", "Total.Population"),
    flags = c("Catholics_imputed", "Total.Population_imputed"),
    compute = function(s) if (s$Total.Population > 0) s$Catholics / s$Total.Population * 100 else 0
  ),
  "Diocesan Priests" = list(bases = "Diocesan.Priests", flags = "Diocesan.Priests_imputed", compute = function(s) s$Diocesan.Priests),
  "Religious Priests" = list(bases = "Religious.Priests", flags = "Religious.Priests_imputed", compute = function(s) s$Religious.Priests),
  "Total Priests" = list(bases = "Total.Priests", flags = "Total.Priests_imputed", compute = function(s) s$Total.Priests),
  "Catholics per Priest" = list(
    bases = c("Catholics", "Total.Priests"),
    flags = c("Catholics_imputed", "Total.Priests_imputed"),
    compute = function(s) if (s$Total.Priests > 0) s$Catholics / s$Total.Priests else 0
  ),
  "Permanent Deacons" = list(bases = "Permanent.Deacons", flags = "Permanent.Deacons_imputed", compute = function(s) s$Permanent.Deacons),
  "Male Religious" = list(bases = "Male.Religious", flags = "Male.Religious_imputed", compute = function(s) s$Male.Religious),
  "Female Religious" = list(bases = "Female.Religious", flags = "Female.Religious_imputed", compute = function(s) s$Female.Religious),
  "Number of Parishes" = list(bases = "Parishes", flags = "Parishes_imputed", compute = function(s) s$Parishes)
)

# ---- Plot theme for consistent styling ----
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
    axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
    axis.text = element_text(size = 10, color = "#7f8c8d"),
    legend.title = element_text(size = 12, face = "bold", color = "#34495e"),
    legend.text = element_text(size = 10, color = "#7f8c8d"),
    panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ---- CSS for sticky filters and custom title styling ----
tags$head(
  tags$style(HTML("
    body {
      font-size: 18px !important; /* Global font size, increased to 18px */
      line-height: 1.6 !important; /* Increased line height for better readability */
      font-family: 'Arial', sans-serif; /* Ensure clear fonts */
    }
    .sticky-filter {
      position: sticky;
      top: 0;
      height: calc(100vh - 60px);
      overflow-y: auto;
      background-color: #f5f6f5;
      padding: 15px;
      border-radius: 5px;
      font-size: 18px !important; /* Filter panel font */
    }
    .content-column {
      padding-right: 20px;
      font-size: 18px !important; /* Content area font */
    }
    h1, h2, h3, h4, h5, h6 {
      font-size: calc(1.5em + 4px) !important; /* Larger title font */
    }
    p, label, .shiny-input-container, .shiny-input-container label, .shiny-input-container select, .shiny-input-container input {
      font-size: 18px !important; /* Paragraph, label and input font */
    }
    .shiny-input-container p, uiOutput p { white-space: pre-line !important; overflow-wrap: break-word !important; }
    .box, .box-body, .box-title {
      font-size: 18px !important; /* Shinydashboard box font */
    }
    .value-box, .value-box .inner, .value-box .small-box h3, .value-box .small-box p {
      font-size: 20px !important; /* Larger valueBox font */
    }
    table, .dataTable, .dataTable td, .dataTable th {
      font-size: 18px !important; /* Table font */
    }
    .leaflet-popup-content {
      font-size: 16px !important; /* 地图弹出框字体 */
    }
    /* Custom styling for Global tab titles */
    #main_tabs .tab-pane[aria-labelledby='Global'] .title-panel h1,
    #main_tabs .tab-pane[aria-labelledby='Global'] .box-title {
      background-color: #2E7D32 !important; /* Dark green background */
      color: white !important; /* White text for contrast */
      padding: 10px 15px !important; /* Padding for better appearance */
      border-radius: 10px !important; /* Rounded corners */
      display: inline-block; /* Ensures padding and border-radius apply correctly */
    }
    /* Styling for the introduction box */
    .intro-box {
      background-color: #f5f6f5 !important;
      border: none !important;
      padding: 20px !important;
      border-radius: 5px !important;
      margin-bottom: 20px !important;
    }
    .intro-box .box-title {
      font-size: 20px !important;
      color: #2c3e50 !important;
      font-weight: bold !important;
      margin-bottom: 15px !important;
    }
    /* Styling for the tabbed trend analysis box */
    .trend-box .nav-tabs {
      background-color: #f5f6f5 !important;
      border-bottom: 1px solid #ecf0f1 !important;
      border-radius: 5px 5px 0 0 !important;
    }
    .trend-box .nav-tabs > li > a {
      color: #2c3e50 !important;
      font-size: 16px !important;
      font-weight: bold !important;
    }
    .trend-box .nav-tabs > li.active > a,
    .trend-box .nav-tabs > li.active > a:hover,
    .trend-box .nav-tabs > li.active > a:focus {
      background-color: #2E7D32 !important; /* Matches Global tab title color */
      color: white !important;
      border: none !important;
    }
    .trend-box .tab-content {
      background-color: #f5f6f5 !important;
      padding: 15px !important;
      border-radius: 0 0 5px 5px !important;
    }
    /* Progress bar styling */
    .progress {
      background-color: #ecf0f1 !important;
      border-radius: 10px !important;
      overflow: hidden !important;
    }
    .progress-bar {
      background-color: #3498db !important;
      transition: width 0.6s ease !important;
    }
    .progress-bar-striped {
      background-image: linear-gradient(45deg, rgba(255,255,255,.15) 25%, transparent 25%, transparent 50%, rgba(255,255,255,.15) 50%, rgba(255,255,255,.15) 75%, transparent 75%, transparent) !important;
      background-size: 1rem 1rem !important;
    }
    .progress-bar-animated {
      animation: progress-bar-stripes 1s linear infinite !important;
    }
    @keyframes progress-bar-stripes {
      0% { background-position: 1rem 0; }
      100% { background-position: 0 0; }
    }
  "))
)

# ====================== UI ======================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Mapping the Catholic Church: Geographic and Temporal Visualizations"),
  
  tabsetPanel(id = "main_tabs",
              
              # ---------- Tab 0: Introduction & Data Update ----------
              tabPanel(
                "Introduction",
                fluidRow(
                  column(
                    12,
                    shinydashboard::box(
                      title = HTML("<strong>Welcome to Catholic Data Visualization</strong>"),
                      status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
                      background = "light-blue",
                      HTML("
        <div style='font-size: 18px; line-height: 1.6; color: #2c3e50;'>
          <p>This website presents an interactive visualisation of global Catholic statistics. The data is sourced from
          <a href='https://www.catholic-hierarchy.org/diocese/lc.html' target='_blank'>Catholic-Hierarchy.org</a>,
          a comprehensive website that collates information on each diocese at the national level.</p>
          
          <p><strong>Data Coverage:</strong> 
          Our database spans from 1950 to the present and is available at two levels of temporal resolution:</p>
          <ul>
            <li>1950 – 2010: Consolidated into one data point per decade</li>
            <li>2010 – present: Annual records</li>
          </ul>
          
          <p><strong>Available Variables:</strong> 
          Discover trends across a wide range of key indicators, including:
          <ul>
          <li>Catholic Population</li>
          <li>Total Population</li>
          <li>Percentage of Catholics</li>
          <li>Diocesan Priests</li>
          <li>Religious Priests</li>
          <li>Total Priests</li>
          <li>Catholics per Priest</li>
          <li>Permanent Deacons</li>
          <li>Male Religious</li>
          <li>Female Religious</li>
          <li>Number of Parishes</li>
          </ul>
          

          
          <p><strong>Interactive Features:</strong> Use our interactive tools to filter and analyze data by time period, country, or specific variable across three levels of detail:</p>
          <ul>
            <li><strong>Global View:</strong> Country-level world map aggregation</li>
            <li><strong>Country View:</strong> In-depth diocesan comparisons within a selected nation</li>
            <li><strong>Diocese View:</strong> Detailed analysis of individual dioceses</li>
          </ul>
          
          <p><strong>Data Modes:</strong> View the data in two formats depending on your research needs:</p>
          <ul>
            <li><strong>Raw Data</strong> – Presented exactly as published by Catholic-Hierarchy.org</li>
            <li><strong>Statistically Processed Data</strong> – Enhanced with linear interpolation to fill missing values and enable smoother trend analysis</li>
          </ul>
        </div>
      ")
                    )
                  )
                ),
                
                # Data Update Section
                fluidRow(
                  column(
                    12,
                    shinydashboard::box(
                      title = HTML("<strong>Data Update</strong>"),
                      status = "warning", solidHeader = TRUE, width = 12, collapsible = FALSE,
                      HTML("<div style='font-size: 16px; color: #2c3e50; margin-bottom: 20px;'>
                        <p><span id='data-status'>Loading...</span></p>
                        <p>To refresh the dataset with the latest information from Catholic-Hierarchy.org, please click the button below. </p>
                        <p>Please note that the update process may take approximately 20–30 minutes.</p>
                      </div>"),
                      div(
                        style = "text-align: center; margin: 20px 0;",
                        actionButton("update_data", "Update Data", 
                                     class = "btn-primary btn-lg",
                                     style = "font-size: 18px; padding: 15px 30px;"),
                        br(), br(),
                        div(id = "progress-container", style = "display: none;",
                            div(class = "progress", style = "height: 25px; margin: 10px 0;",
                                div(id = "progress-bar", class = "progress-bar progress-bar-striped progress-bar-animated",
                                    role = "progressbar", style = "width: 0%", "0%")),
                            div(id = "progress-text", style = "font-size: 14px; color: #666;", "Preparing...")),
                        div(id = "update-status", style = "margin-top: 15px; font-size: 14px;")
                      )
                    )
                  )
                )
              ),
              
              # ---------- Tab 1: Global ----------
              tabPanel(
                "Global",
                # Introduction
                
                
                # Global Statistics Overview
                fluidRow(
                  column(
                    12,
                    shinydashboard::box(
                      title = HTML("<strong>Global Statistics Overview</strong>"),
                      status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
                      fluidRow(
                        column(3, shinydashboard::valueBoxOutput("vb_countries", width = 12)),
                        column(3, shinydashboard::valueBoxOutput("vb_dioceses", width = 12)),
                        column(3, shinydashboard::valueBoxOutput("vb_catholics", width = 12)),
                        column(3, shinydashboard::valueBoxOutput("vb_priests", width = 12))
                      )
                    )
                  )
                ),
                
                # Filters + Map + Charts
                fluidRow(
                  column(
                    9,
                    class = "content-column",
                    shinydashboard::box(
                      title = HTML("<strong>Interactive Global Map of Catholic Statistics</strong>"),
                      status = "primary", solidHeader = FALSE, width = 12, collapsible = FALSE,
                      background = "light-blue",
                      HTML("
        <p style='font-size: 18px; line-height: 1.6; color: #2c3e50;'>
          This interactive world map visualizes the global distribution of the selected variable. Each country is represented by a bubble positioned at its geographic centroid. Bubble size reflects the value of the selected variable (larger bubbles indicate higher values), while bubble color shows the year-over-year trend:
        </p>
        <ul style='font-size: 18px; line-height: 1.6; color: #2c3e50;'>
          <li>Green – Increase compared to the previous year</li>
          <li>Red – Decrease compared to the previous year</li>
          <li>Gray – No change from the previous year</li>
        </ul>
      ")
                    ),
                    leafletOutput("map", height = "600px"),
                    br(),
                    shinydashboard::box(
                      title = HTML("<strong>Bar Chart</strong>"),
                      status = "primary", solidHeader = TRUE, width = 12,
                      uiOutput("barChartDescription"),
                      fluidRow(
                        column(11, plotOutput("barChart", height = "350px")),
                        column(1, downloadButton("download_barChart", "JPG")),
                        column(1, downloadButton("download_barChart_png", "PNG")),
                        column(1, downloadButton("download_barChart_pdf", "PDF"))
                      )
                    ),
                    shinydashboard::box(
                      title = HTML("<strong>Country-Level Trend Analysis</strong>"),
                      status = "primary", solidHeader = TRUE, width = 12, class = "trend-box",
                      tabsetPanel(
                        tabPanel(
                          "Line Chart",
                          uiOutput("trendChartDescription"),
                          fluidRow(
                            column(11, plotOutput("trendPlot", height = "400px")),
                            column(1, downloadButton("download_trendPlot", "JPG")),
                            column(1, downloadButton("download_trendPlot_png", "PNG")),
                            column(1, downloadButton("download_trendPlot_pdf", "PDF"))
                          )
                        ),
                        tabPanel(
                          "Table",
                          uiOutput("trendTableDescription"), # Replace static p with dynamic uiOutput
                          fluidRow(
                            column(11, div(
                              style = "width:100%; overflow-x:auto;",
                              tableOutput("trendTable")
                            )),
                            column(1, downloadButton("download_trendTable", "CSV"))
                          )
                        )
                      )
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      class = "sticky-filter",
                      h4("Filters"),
                      p("Please use these controls to customize the data that is displayed on the map and charts:"),
                      radioButtons(
                        inputId = "time_period",
                        label = "Time Period",
                        choices = c("Before 2010" = "before_2010", "After 2010" = "after_2010"),
                        selected = "after_2010",
                        inline = TRUE
                      ),
                      p("Please indicate whether you require data from before or after 2010.
If you require data from before 2010, please select 'Before 2010.' If you require data from after 2010, please select 'After 2010.'"),
                      uiOutput("dynamic_year_slider"),
                      p("Slide to select a specific year within the selected time period."),
                      selectInput("countries", "Select countries for comparison (leave blank for all)",
                                  choices = sort(unique(data$Country)), multiple = TRUE),
                      p("Please select one or more countries for study. If you leave this field blank, the top countries will be included."),
                      selectInput("var", "Select Variable", choices = names(var_info)),
                      p("Please select a variable, such as 'Total Catholics' or 'Percent Catholic,' in order to visualize the data."),
                      selectInput("mode", "Data Mode", choices = c("Statistically Processed Data" = "Imputed", "Raw Data" = "Non-Imputed")),
                      p("Please note that 'Statistically Processed Data' includes statistically estimated values for missing data. 'Raw Data' shows only original, unprocessed data.")
                    )
                  )
                )
              ),
              
              # ---------- Tab 2: Country-Level ----------
              tabPanel(
                "Country-Level",
                fluidRow(
                  column(
                    12,
                    shinydashboard::box(
                      title = HTML("<strong>Country-Level Catholic Data Visualization</strong>"),
                      status = "warning", solidHeader = TRUE, width = 12, collapsible = FALSE,
                      background = "light-blue",
                      HTML("
  <div style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif; text-align: left;'>
  
  <p>On this page, users can explore comprehensive Catholic statistics at the country level. 
  These statistics can be filtered by selecting a country of interest. 
  Flexible filters allow users to:</p>

  <ul style='margin-left: 25px;'>
    <li>Choose key variables such as <em>Catholic population</em>, <em>percent Catholic</em>, <em>total population</em>, 
  <em>diocesan and religious priests</em>, <em>total priests</em>, <em>permanent deacons</em>, 
  <em>male and female religious</em>, and <em>parishes</em></li>
  
    <li>Specify a time period (<em>pre-2010 aggregated data</em> or <em>post-2010 annual data</em>) and the exact year</li>
<li>Select a data mode (statistically processed data or raw data)</li>
  </ul>

  <p>Interactive charts and tables provide clear insights into diocesan development patterns:</p>

  <ul style='margin-left: 25px;'>
    <li>The bar chart provides a comparison of selected dioceses within the country for the chosen year</li>
    <li>The Dioceses-Level Trend Analysis uses line charts to show changes over time</li>
    <li>A complete diocesan data table is available in the trend analysis section for detailed reference</li>
  </ul>

  <p>This page enables users to intuitively observe and analyze country-wide Catholic developments 
  through an engaging visual interface.</p>

  </div>
  ")
                    )
                    
                  )
                ),
                fluidRow(
                  column(
                    9,
                    class = "content-column",
                    shinydashboard::box(
                      title = HTML("<strong>Bar Chart</strong>"),
                      status = "warning", solidHeader = TRUE, width = 12,
                      uiOutput("cl_barChartDescription"),
                      fluidRow(
                        column(11, plotOutput("cl_barChart", height = "350px")),
                        column(1, downloadButton("download_cl_barChart", "JPG")),
                        column(1, downloadButton("download_cl_barChart_png", "PNG")),
                        column(1, downloadButton("download_cl_barChart_pdf", "PDF"))
                      )
                    ),
                    shinydashboard::box(
                      title = HTML("<strong>Dioceses-Level Trend Analysis</strong>"),
                      status = "warning", solidHeader = TRUE, width = 12, class = "trend-box",
                      tabsetPanel(
                        tabPanel(
                          "Line Chart",
                          uiOutput("cl_trendChartDescription"),
                          fluidRow(
                            column(11, plotOutput("cl_trend", height = "350px")),
                            column(1, downloadButton("download_cl_trend", "JPG")),
                            column(1, downloadButton("download_cl_trend_png", "PNG")),
                            column(1, downloadButton("download_cl_trend_pdf", "PDF"))
                          )
                        ),
                        tabPanel(
                          "Table",
                          uiOutput("cl_trendTableDescription"),
                          fluidRow(
                            column(11, div(
                              style = "width:100%; overflow-x:auto;",
                              tableOutput("cl_table")
                            )),
                            column(1, downloadButton("download_cl_table", "CSV"))
                          )
                        )
                      )
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      class = "sticky-filter",
                      h4("Filters"),
                      p("Please use these controls to customize the data that is displayed on the map and charts:"),
                      radioButtons(
                        inputId = "cl_time_period",
                        label = "Time Period",
                        choices = c("Before 2010" = "before_2010", "After 2010" = "after_2010"),
                        selected = "after_2010",
                        inline = TRUE
                      ),
                      p("Please indicate whether you require data from before or after 2010.
If you require data from before 2010, please select 'Before 2010.' If you require data from after 2010, please select 'After 2010.'"),
                      uiOutput("cl_dynamic_year_slider"),
                      p("Slide to select a specific year within the selected time period."),
                      selectInput("cl_country", "Country",
                                  choices = sort(unique(data$Country)), multiple = FALSE),
                      p("Please select a country to focus on trends and comparisons."),
                      uiOutput("cl_diocese_ui"),
                      p("Select one or more dioceses to study. If you leave this field blank, the top dioceses will be included."),
                      selectInput("cl_var", "Select Variable", choices = names(var_info)),
                      p("Please select a variable, such as 'Total Catholics' or 'Percent Catholic,' in order to visualize the data."),
                      selectInput("cl_mode", "Data Mode", choices = c("Statistically Processed Data" = "Imputed", "Raw Data" = "Non-Imputed")),
                      p("Please note that 'Statistically Processed Data' includes statistically estimated values for missing data. 'Raw Data' shows only original, unprocessed data.")
                    )
                  )
                )
              ),
              
              # ---------- Tab 3: Diocese-Level ----------
              tabPanel(
                "Diocese-Level",
                fluidRow(
                  column(
                    12,
                    shinydashboard::box(
                      title = HTML("<strong>Diocese-Level Catholic Data Visualization</strong>"),
                      status = "info", solidHeader = TRUE, width = 12, collapsible = FALSE,
                      background = "light-blue",
                      HTML("<p style='font-size: 18px; line-height: 1.6; color: #2c3e50;'>
        On this page, users can explore comprehensive Catholic statistics at the diocese level, alongside general information about a specific diocese. 
        These statistics can be filtered by selecting a specific diocese of interest. 
        Flexible filters allow users to:

</p>
<ul style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>
  <li>Choose key variables such as <em>Catholic population</em>, <em>percent Catholic</em>, <em>total population</em>, 
  <em>diocesan and religious priests</em>, <em>total priests</em>, <em>permanent deacons</em>, 
  <em>male and female religious</em>, and <em>parishes</em></li>
  <li>Specify a time period (<em>pre-2010 aggregated data</em> or <em>post-2010 annual data</em>)</li>
  <li>Select a data mode (statistically processed data or raw data)</li>

</ul>
<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>
Interactive charts and tables will be provided to facilitate clear insights into diocesan development patterns.
      </p>")
                    )
                  )
                ),
                fluidRow(
                  column(
                    9,
                    class = "content-column",
                    shinydashboard::box(
                      title = HTML("<strong>General Information</strong>"),
                      status = "info", solidHeader = TRUE, width = 12,
                      htmlOutput("dl_general_info")
                    ),
                    shinydashboard::box(
                      title = HTML("<strong>Catholic Data Visualization</strong>"),
                      status = "info", solidHeader = TRUE, width = 12, collapsible = FALSE,#collapsible = TRUE, collapsed = FALSE,
                      class = "trend-box",
                      tabsetPanel(
                        tabPanel(
                          "Line Chart",
                          uiOutput("dl_trendChartDescription"),
                          fluidRow(
                            column(11, plotOutput("dl_trend", height = "400px")),
                            column(1, downloadButton("download_dl_trend", "JPG")),
                            column(1, downloadButton("download_dl_trend_png", "PNG")),
                            column(1, downloadButton("download_dl_trend_pdf", "PDF"))
                          )
                        ),
                        tabPanel(
                          "Bar Chart",
                          uiOutput("dl_barChartDescription"),
                          fluidRow(
                            column(11, plotOutput("dl_bar", height = "400px")),
                            column(1, downloadButton("download_dl_bar", "JPG")),
                            column(1, downloadButton("download_dl_bar_png", "PNG")),
                            column(1, downloadButton("download_dl_bar_pdf", "PDF"))
                          )
                        ),
                        tabPanel(
                          "Table",
                          uiOutput("dl_trendTableDescription"),
                          fluidRow(
                            column(11, div(
                              style = "width:100%; overflow-x:auto;",
                              DTOutput("dl_data_table")
                            )),
                            column(1, downloadButton("download_dl_data_table", "CSV"))
                          )
                        )
                      )
                    )
                  ),
                  column(
                    3,
                    wellPanel(
                      class = "sticky-filter",
                      h4("Filters"),
                      p("Please use these controls to customize the data that is displayed on the map and charts:"),
                      radioButtons(
                        inputId = "dl_time_period",
                        label = "Time Period",
                        choices = c("Before 2010" = "before_2010", "After 2010" = "after_2010", "All Years" = "all_years"),
                        selected = "after_2010",
                        inline = TRUE
                      ),
                      p("Please select a time period to filter data by year range."),
                      uiOutput("dl_diocese_ui"),
                      p("Please select a specific diocese for analysis."),
                      selectInput("dl_var", "Select Variable", choices = names(var_info), selected = "Catholic Population", multiple = TRUE),
                      p("Please select one or more variables, such as 'Total Catholics' or 'Percent Catholic,' in order to visualize the data."),
                      selectInput("dl_mode", "Data Mode", choices = c("Statistically Processed Data" = "Imputed", "Raw Data" = "Non-Imputed")),
                      p("Please note that 'Statistically Processed Data' includes statistically estimated values for missing data. 'Raw Data' shows only original, unprocessed data.")
                      
                    )
                  )
                )
              ),
  )
)

# ==================== Server ====================
server <- function(input, output, session) {
  
  # Reactive data object
  reactive_data <- reactiveVal(data)
  
  # Function to update data status
  update_data_status <- function() {
    status_text <- if (file.exists("web_data.csv")) {
      paste("Last updated:", 
            format(file.info("web_data.csv")$mtime, "%Y-%m-%d %H:%M"))
    } else {
      "Using original data"
    }
    shinyjs::html("data-status", status_text)
  }
  
  # Initialize data status
  observe({
    update_data_status()
  })
  
  # Update select input options when data changes
  observe({
    current_data <- reactive_data()
    # Update country selection for Global page
    updateSelectInput(session, "countries", 
                      choices = sort(unique(current_data$Country)))
    # Update country selection for Country-Level page
    updateSelectInput(session, "cl_country", 
                      choices = sort(unique(current_data$Country)))
  })
  
  get_diocese_country <- reactive({
    req(input$dl_diocese)
    current_data <- reactive_data()
    df <- current_data %>% dplyr::filter(diocese == input$dl_diocese) %>% dplyr::slice(1)
    country <- if (!is.na(df$Country)) df$Country else "Unknown"
    country
  })
  
  # Data update handling
  observeEvent(input$update_data, {
    # Disable update button
    shinyjs::disable("update_data")
    
    # Show progress bar
    shinyjs::show("progress-container")
    shinyjs::html("update-status", "")
    
    # Execute data update asynchronously
    future({
      tryCatch({
        # Update progress - Step 1
        shinyjs::html("progress-text", "Step 1/4: Initializing update process...")
        shinyjs::runjs("$('#progress-bar').css('width', '10%').text('10%')")
        
        # Update progress - Step 2
        shinyjs::html("progress-text", "Step 2/4: Running dc.R data collection script...")
        shinyjs::runjs("$('#progress-bar').css('width', '30%').text('30%')")
        
        # Run dc.R script
        source("dc.R", local = TRUE)
        
        # Update progress - Step 3
        shinyjs::html("progress-text", "Step 3/4: Loading and validating new data...")
        shinyjs::runjs("$('#progress-bar').css('width', '70%').text('70%')")
        
        # Reload data
        new_data <- load_data()
        
        # Validate data
        if (is.null(new_data) || nrow(new_data) == 0) {
          stop("New data is empty or invalid")
        }
        
        # Update progress - Step 4
        shinyjs::html("progress-text", "Step 4/4: Finalizing update...")
        shinyjs::runjs("$('#progress-bar').css('width', '100%').text('100%')")
        
        # Return success result
        list(success = TRUE, data = new_data, message = paste0("Data updated successfully! Loaded ", nrow(new_data), " records."))
        
      }, error = function(e) {
        # Return error result
        list(success = FALSE, data = NULL, message = paste("Error:", e$message))
      })
    }) %...>% (function(result) {
      if (result$success) {
        # Update reactive data
        reactive_data(result$data)
        
        # Update status display
        update_data_status()
        
        # Display success message
        shinyjs::html("update-status", 
                      paste0("<div style='color: green; font-weight: bold; margin-bottom: 10px;'>", 
                             result$message, "</div>",
                             "<div style='color: #666; font-size: 14px;'>",
                             "Data update completed. All charts and tables will automatically use the latest data.",
                             "</div>"))
        
        # Show notification
        showNotification("Data updated successfully! Page refreshed with latest data.", 
                         duration = 5)
      } else {
        # Display error message
        shinyjs::html("update-status", 
                      paste0("<div style='color: red; font-weight: bold;'>", 
                             result$message, "</div>"))
        
        showNotification(paste("Data update failed:", result$message), 
                         duration = 10)
      }
      
      # Hide progress bar and re-enable button
      shinyjs::hide("progress-container")
      shinyjs::enable("update_data")
      
    }) %...!% (function(error) {
      # Handle async errors
      shinyjs::html("update-status", 
                    paste0("<div style='color: red; font-weight: bold;'>", 
                           "Unexpected error: ", error$message, "</div>"))
      
      showNotification(paste("Unexpected error:", error$message), 
                       duration = 10)
      
      # Hide progress bar and re-enable button
      shinyjs::hide("progress-container")
      shinyjs::enable("update_data")
    })
  })
  
  # ---------------------------------------------- Global -------------------------------------------------------------------------------
  
  output$dynamic_year_slider <- renderUI({
    current_data <- reactive_data()
    available_years <- sort(unique(current_data$Year_process))
    
    if (input$time_period == "before_2010") {
      years_before <- available_years[available_years <= 2010]
      if (length(years_before) == 0) {
        return(helpText("No data available for this time period"))
      }
      sliderTextInput(
        inputId = "year",
        label = "Year:",
        choices = years_before,
        selected = 2000,
        grid = TRUE,
        animate = TRUE
      )
    } else {
      years_after <- available_years[available_years >= 2010]
      if (length(years_after) == 0) {
        return(helpText("No data available for this time period"))
      }
      sliderTextInput(
        inputId = "year",
        label = "Year:",
        choices = years_after,
        selected = 2020,
        grid = TRUE,
        animate = TRUE
      )
    }
  })
  
  data_reactive <- reactive({
    req(input$year)
    
    current_data <- reactive_data()
    current_year <- as.integer(input$year)
    sorted_years <- sort(unique(current_data$Year_process))
    prev_candidates <- sorted_years[sorted_years < current_year]
    prev_year <- if (length(prev_candidates) > 0) max(prev_candidates) else NULL
    var <- input$var
    mode <- input$mode
    countries <- input$countries
    flags <- var_info[[var]]$flags
    bases <- var_info[[var]]$bases
    compute_func <- var_info[[var]]$compute
    
    data_year <- current_data %>% filter(Year_process == current_year)
    if (mode == "Non-Imputed") {
      filter_cond <- apply(data_year[, flags, drop = FALSE] == 0, 1, all)
      data_year <- data_year[filter_cond, ]
    }
    if (length(countries) > 0) data_year <- data_year %>% filter(Country %in% countries)
    
    agg_current <- NULL
    if (nrow(data_year) > 0) {
      agg_current <- data_year %>%
        group_by(Country) %>%
        summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
      agg_current$value <- apply(agg_current[, bases, drop = FALSE], 1, function(row) {
        s <- as.list(row)
        names(s) <- bases
        compute_func(s)
      })
    }
    
    agg_prev <- NULL
    if (!is.null(prev_year)) {
      data_prev <- current_data %>% filter(Year_process == prev_year)
      if (mode == "Non-Imputed") {
        filter_cond_prev <- apply(data_prev[, flags, drop = FALSE] == 0, 1, all)
        data_prev <- data_prev[filter_cond_prev, ]
      }
      if (length(countries) > 0) data_prev <- data_prev %>% filter(Country %in% countries)
      if (nrow(data_prev) > 0) {
        agg_prev <- data_prev %>%
          group_by(Country) %>%
          summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
        agg_prev$value_prev <- apply(agg_prev[, bases, drop = FALSE], 1, function(row) {
          s <- as.list(row)
          names(s) <- bases
          compute_func(s)
        })
      }
    }
    
    list(agg_current = agg_current, agg_prev = agg_prev, prev_year = prev_year)
  })
  
  global_stats <- reactive({
    req(input$year)
    
    current_data <- reactive_data()
    current_year <- as.integer(input$year)
    mode <- input$mode
    sel_countries <- input$countries
    
    df <- current_data %>% dplyr::filter(Year_process == current_year)
    
    if (length(sel_countries) > 0) {
      df <- df %>% dplyr::filter(Country %in% sel_countries)
    }
    
    flags_to_check <- c("Catholics_imputed", "Total.Priests_imputed", "Dioceses_imputed", "Total.Dioceses_imputed")
    if (mode == "Non-Imputed") {
      present_flags <- flags_to_check[flags_to_check %in% names(df)]
      if (length(present_flags) > 0) {
        keep <- apply(df[, present_flags, drop = FALSE] == 0 | is.na(df[, present_flags, drop = FALSE]), 1, all)
        df <- df[keep, , drop = FALSE]
      }
    }
    
    countries_n <- dplyr::n_distinct(df$Country[!is.na(df$Country)])
    
    total_dioceses <- if ("url" %in% names(df)) {
      urls <- trimws(as.character(df$url))
      dplyr::n_distinct(urls[!is.na(urls) & urls != ""])
    } else {
      NA_integer_
    }
    
    total_catholics <- if ("Catholics" %in% names(df)) {
      round(sum(df$Catholics, na.rm = TRUE))
    } else {
      NA_real_
    }
    
    total_priests <- if ("Total.Priests" %in% names(df)) {
      round(sum(df$Total.Priests, na.rm = TRUE))
    } else {
      dioc <- if ("Diocesan.Priests" %in% names(df)) df$Diocesan.Priests else 0
      rel  <- if ("Religious.Priests" %in% names(df)) df$Religious.Priests else 0
      round(sum(dplyr::coalesce(dioc, 0) + dplyr::coalesce(rel, 0), na.rm = TRUE))
    }
    
    list(
      countries_n = countries_n,
      total_dioceses = total_dioceses,
      total_catholics = total_catholics,
      total_priests = total_priests
    )
  })
  
  .fmt_num <- function(x) { if (is.na(x)) return("N/A"); format(x, big.mark = ",", scientific = FALSE, trim = TRUE) }
  
  output$cl_trendTableDescription <- renderUI({
    req(input$cl_time_period, input$cl_var, input$cl_country) # Ensure required inputs are available
    
    time_period_text <- switch(input$cl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010" = paste0("2010–", format(Sys.Date(), "%Y"))
    )
    mode_text <- switch(input$cl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle diocese selection
    diocese_text <- if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      "the top dioceses"
    } else if (nrow(cl_choice_df()) == 1) 
    {"a single diocese"
    }
    else{
      paste("the selected diocese(s) (", paste(input$cl_dioceses, collapse = ", "), ")", sep = "")
    }
    
    # Conditional data description based on mode
    data_description <- paste("This table provides a foundation for the the 'Line Chart', which illustrate")
    
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " the variable <strong>", input$cl_var, "</strong> across <strong>", diocese_text, "</strong> in <strong>", input$cl_country, "</strong>. ",
      "Each row in the table corresponds to a dioceses, and each column to a year. 
      The values in the table reflect the selected variable. ",
      "The data covers the time period <strong>", time_period_text, "</strong> and is presented using <strong>", mode_text, "</strong>. ",
      "Please scroll horizontally to view all years and use the table for detailed data analysis or export.",
      "</p>"
    ))
  })
  
  output$trendTableDescription <- renderUI({
    req(input$time_period, input$var)  # Ensure required inputs are available
    
    time_period_text <- switch(input$time_period,
                               "before_2010" = "Before and Including 2010",
                               "after_2010" = "2010 and After")
    
    # Handle country selection
    country_text <- if (is.null(input$countries) || length(input$countries) == 0) {
      "all countries"
    } else {
      paste("the selected countries (", paste(input$countries, collapse = ", "), ")", sep = "")
    }
    
    # Conditional phrasing based on Data Mode
    data_description <- paste(
      "This table provides a foundation for the the 'Line Chart', which illustrate")
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " the variable <strong>", input$var, "</strong> across <strong>", country_text, "</strong>. ",
      " Each row in the table corresponds to a country, and each column to a year.
       The values in the table reflect the selected variable. ",
      "The data covers the period ", time_period_text, " and is presented using ", 
      switch(input$mode, "Imputed" = "<strong>statistically processed data</strong>",
             "Non-Imputed" = "<strong>raw data</strong>"), ". ",
      "Please scroll horizontally to view all years and use the table for detailed data analysis or export.",
      "</p>"
    ))
  })
  
  
  output$trendChartDescription <- renderUI({
    req(input$time_period, input$var)  # Ensure required inputs are available
    
    time_period_text <- switch(input$time_period,
                               "before_2010" = "1950–2010",
                               "after_2010" = paste0("2010–", format(Sys.Date(), "%Y"))
    )
    # Handle country selection
    country_text <- if (is.null(input$countries) || length(input$countries) == 0) {
      "top 10 countries"
    } else {
      paste("the selected countries (", paste(input$countries, collapse = ", "), ")", sep = "")
    }
    
    # Conditional phrasing based on Data Mode
    data_description <- ("This line chart presents trends over time for the selected variable")
    
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " <strong>", input$var, "</strong> across ", country_text, ". ",
      "Each line represents a different country, with distinct colors to facilitate straightforward comparison. ",
      "The chart enables users to observe growth, decline, or stability of the selected variable. ",
      "The data is represents here covers the time period <strong>", time_period_text, "</strong> and is based on ", 
      switch(input$mode, "Imputed" = "<strong>statistically processed data</strong>",
             "Non-Imputed" = "<strong>raw data</strong>"), ".",
      "</p>"
    ))
  })
  
  # Get Country for the selected diocese
  
  
  
  
  output$dl_barChartDescription <- renderUI({
    req(input$dl_time_period, input$dl_var, input$dl_diocese)
    
    time_period_text <- switch(input$dl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
                               "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    mode_text <- switch(input$dl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle multiple variables
    var_text <- if (length(input$dl_var) > 1) {
      paste("the selected variables (<strong>", paste(input$dl_var, collapse = ", "), "</strong>)", sep = "")
    } else {
      paste("the variable <strong>", input$dl_var[1], "</strong>", sep = "")
    }
    
    # Get country from general information
    diocese_country <- get_diocese_country()
    
    # Conditional data description based on mode
    data_description <-  paste("This bar chart presents data on")
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " ", var_text," across the years for the <strong>Diocese of ", input$dl_diocese, "</strong> in <strong>", diocese_country, "</strong>. ",
      "The data has been sorted by year to facilitate straightforward comparison and covers the time period <strong>", time_period_text, "</strong> based on <strong>", mode_text, "</strong>.",
      "</p>"
    ))
  })
  
  output$dl_trendTableDescription <- renderUI({
    req(input$dl_time_period, input$dl_var, input$dl_diocese)
    
    time_period_title <- switch(
      input$dl_time_period,
      "before_2010" = "1950–2010",
      "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
      "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    
    mode_text <- switch(input$dl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle multiple variables
    var_text <- if (length(input$dl_var) > 1) {
      paste("the selected variable(s) (<strong>", paste(input$dl_var, collapse = ", "), "</strong>)", sep = "")
    } else {
      paste("the selected variable <strong>", input$dl_var[1], "</strong>", sep = "")
    }
    
    # Get country from general information
    diocese_country <- get_diocese_country()
    
    # Conditional data description based on mode
    data_description <- paste ("This table provides a foundation for the line chart and bar chart, which illustrate")
    
    mode_text <- switch(input$dl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " ", var_text, " for the <strong>Diocese of ", input$dl_diocese, "</strong> in <strong>", diocese_country, "</strong>. ",
      "The data covers the time period <strong>", time_period_title, "</strong> and is based on <strong>", mode_text, "</strong>. ",
      "Please flip to the next page to view all available years and use this function for detailed data extraction.",
      "</p>"
    ))
  })
  
  output$barChartDescription <- renderUI({
    req(input$year, input$var) # Ensure year and variable are selected
    
    mode_text <- switch(input$mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    if (length(input$countries) == 0) {
      HTML(paste0("<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>
      This bar chart presents data on the <strong>", input$var, "</strong> in <strong>", input$year, "</strong> of the <strong>top 10 countries</strong>.
      The countries have been ordered in descending order to facilitate straightforward comparison. The data is based on <strong>", mode_text, "</strong>.</p>"))
    } else {
      selected_countries <- paste(input$countries, collapse = ", ")
      HTML(paste0("<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial,
                  sans-serif;'>This bar chart presents data on the <strong>", input$var, "</strong> in <strong>", input$year, "</strong> of the selected countries (<strong>", selected_countries, "</strong>). 
                 The countries have been ordered in descending order to facilitate straightforward comparison. The data is based on <strong>", mode_text, "</strong>.</p>"))
    }
  })
  
  output$dl_trendChartDescription <- renderUI({
    req(input$dl_time_period, input$dl_var, input$dl_diocese)
    
    time_period_text <- switch(input$dl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
                               "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    mode_text <- switch(input$dl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle multiple variables
    var_text <- if (length(input$dl_var) > 1) {
      paste("the selected variables (", paste(input$dl_var, collapse = ", "), ")", sep = "")
    } else {
      paste("the variable <strong>", input$dl_var[1], "</strong>", sep = "")
    }
    
    # Get country from general information
    diocese_country <- get_diocese_country()
    
    # Conditional data description based on mode
    data_description <- (
      "This line chart represents trends over time for"
    )
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " ", var_text, " of the <strong>Diocese ", input$dl_diocese, "</strong> 
      in <strong>", diocese_country, "</strong>. ",
      "The chart enables users to observe growth, decline, or stability 
      of the selected variable. ",
      "The data is represents here covers the time period <strong>", time_period_text, "</strong> 
      and is based on <strong>", mode_text, "</strong>.",
      "</p>"
    ))
  })
  
  output$vb_countries <- shinydashboard::renderValueBox({
    gs <- global_stats()
    shinydashboard::valueBox(.fmt_num(gs$countries_n), paste0("Countries in ", input$year), color = "aqua")
  })
  
  output$vb_dioceses <- shinydashboard::renderValueBox({
    gs <- global_stats()
    shinydashboard::valueBox(.fmt_num(gs$total_dioceses), "Total Dioceses", color = "light-blue")
  })
  
  output$vb_catholics <- shinydashboard::renderValueBox({
    gs <- global_stats()
    shinydashboard::valueBox(.fmt_num(gs$total_catholics), "Total Catholics", color = "green")
  })
  
  output$vb_priests <- shinydashboard::renderValueBox({
    gs <- global_stats()
    shinydashboard::valueBox(.fmt_num(gs$total_priests), "Total Priests", color = "yellow")
  })
  
  trend_reactive <- reactive({
    req(input$year, input$time_period, input$var, input$mode)
    
    current_data <- reactive_data()
    current_year <- as.integer(input$year)
    var <- input$var
    mode <- input$mode
    countries <- input$countries
    time_period <- input$time_period
    flags <- var_info[[var]]$flags
    bases <- var_info[[var]]$bases
    compute_func <- var_info[[var]]$compute
    
    # Filter data based on time period
    df <- current_data
    if (time_period == "before_2010") {
      df <- df %>% filter(Year_process <= 2010)
    } else if (time_period == "after_2010") {
      df <- df %>% filter(Year_process >= 2010)
    }
    
    # Apply data mode filter
    if (mode == "Non-Imputed") { 
      keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
      df <- df[keep, ]
    }
    
    # Filter by selected countries, if any
    if (length(countries) > 0) {
      df <- df %>% filter(Country %in% countries)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    # Aggregate data by year and country
    agg <- df %>%
      group_by(Year_process, Country) %>%
      summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
    agg$value <- apply(agg[, bases, drop = FALSE], 1, function(row) {
      s <- as.list(row)
      names(s) <- bases
      compute_func(s)
    })
    
    # If no countries selected, use top 10 based on selected year
    if (length(countries) == 0) {
      latest <- agg %>% 
        filter(Year_process == current_year, !is.na(value), is.finite(value)) %>% 
        arrange(desc(value)) %>% 
        head(10) %>% 
        pull(Country)
      agg <- agg %>% filter(Country %in% latest)
    }
    
    agg
  })
  
  output$map <- renderLeaflet({
    data_list <- data_reactive()
    if (is.null(data_list)) {
      return(leaflet() %>% addTiles() %>% addPopups(lng = 0, lat = 0, popup = "Please select a year first"))
    }
    
    agg_current <- data_list$agg_current
    if (is.null(agg_current)) {
      return(leaflet() %>% addTiles() %>% addPopups(lng = 0, lat = 0, popup = "No data available for selected criteria"))
    }
    agg_prev <- data_list$agg_prev
    prev_year <- data_list$prev_year
    if (!is.null(prev_year) && !is.null(agg_prev)) {
      agg_current <- left_join(agg_current, agg_prev %>% select(Country, value_prev), by = "Country")
      agg_current$direction <- dplyr::case_when(
        is.na(agg_current$value_prev) ~ "no_data",
        abs(agg_current$value - agg_current$value_prev) < 1e-6 ~ "stable",
        agg_current$value > agg_current$value_prev ~ "increase",
        TRUE ~ "decrease"
      )
      agg_current$color <- dplyr::case_when(
        agg_current$direction == "increase" ~ "lightgreen",
        agg_current$direction == "decrease" ~ "red",
        agg_current$direction == "stable" ~ "gray",
        TRUE ~ "gray"
      )
    } else { 
      agg_current$color <- "gray"
      agg_current$direction <- "N/A"
    }
    
    agg_current$Country <- dplyr::case_when(agg_current$Country == "Germany" ~ "Germany", TRUE ~ agg_current$Country)
    
    map_data <- left_join(agg_current, cent_df, by = "Country") %>% filter(!is.na(lat) & !is.na(lon) & is.finite(value))
    if (nrow(map_data) == 0) {
      return(leaflet() %>% addTiles() %>% addPopups(lng = 0, lat = 0, popup = "No data available for selected criteria"))
    }
    
    max_val <- max(map_data$value, na.rm = TRUE)
    min_radius <- 2
    max_radius <- 30
    map_data$radius <- if (max_val > 0) min_radius + (max_radius - min_radius) * sqrt(map_data$value / max_val) else min_radius
    
    leaflet(map_data) %>% addTiles() %>% addCircleMarkers(
      lng = ~lon, lat = ~lat, radius = ~radius, fillColor = ~color, fillOpacity = 0.7, stroke = FALSE,
      popup = ~paste("<b>Country:</b> ", Country, "<br><b>Value (", input$var, "):</b> ", round(value, 2),
                     "<br><b>Trend vs ", data_list$prev_year, ":</b> ", direction)
    )
  })
  
  output$barChart <- renderPlot({
    data_list <- data_reactive()
    if (is.null(data_list)) { 
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1,"Please select a year first")
      return()
    }
    
    agg_current <- data_list$agg_current
    if (is.null(agg_current) || nrow(agg_current) == 0) { 
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1,"No data")
      return()
    }
    agg_current <- agg_current %>% arrange(desc(value))
    if (length(input$countries) == 0) agg_current <- head(agg_current, 10)
    
    chart_title <- if (length(input$countries) == 0) paste("", input$var, "by Country in", input$year, " – Top 10 Countries")
    else paste("", input$var, "by Country in", input$year, " – Selected Countries")
    
    ggplot(agg_current, aes(x = reorder(Country, -value), y = value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title   = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
        axis.title   = element_text(size = 12, face = "bold", color = "#34495e"),
        axis.text    = element_text(size = 10, color = "#7f8c8d")
      )  +
      labs(title = chart_title, x = "Country", y = input$var)
  })
  
  # Download handler for barChart
  output$download_barChart <- downloadHandler(
    filename = function() {
      paste("barChart-", input$var, "-", input$year, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      data_list <- data_reactive()
      if (is.null(data_list)) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "Please select a year first") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
      } else if (is.null(data_list$agg_current) || nrow(data_list$agg_current) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        agg_current <- data_list$agg_current %>% arrange(desc(value))
        if (length(input$countries) == 0) agg_current <- head(agg_current, 10)
        
        chart_title <- if (length(input$countries) == 0) paste("", input$var, "by Country in", input$year, " – Top 10 Countries")
        else paste("", input$var, "by Country in", input$year, " – Selected Countries")
        
        p <- ggplot(agg_current, aes(x = reorder(Country, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title   = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title   = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text    = element_text(size = 10, color = "#7f8c8d")
          )  +
          labs(title = chart_title, x = "Country", y = input$var)
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_cl_barChart_png <- downloadHandler(
    filename = function() {
      paste("cl_barChart-", input$cl_var, "-", input$cl_country, "-", input$cl_year, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      cr <- cl_current_reactive()
      if (is.null(cr) || nrow(cr) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Top 8 Dioceses in", input$cl_country, "")
          } else {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– All Dioceses in", input$cl_country, "")
          }
        } else {
          chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Selected Diocese(s) in", input$cl_country, "")
        }
        
        p <- ggplot(cr, aes(x = reorder(label, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          plot_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(title = chart_title, x = "Diocese", y = input$cl_var)
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_cl_barChart_pdf <- downloadHandler(
    filename = function() {
      paste("cl_barChart-", input$cl_var, "-", input$cl_country, "-", input$cl_year, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      cr <- cl_current_reactive()
      if (is.null(cr) || nrow(cr) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Top 8 Dioceses in", input$cl_country, "")
          } else {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– All Dioceses in", input$cl_country, "")
          }
        } else {
          chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Selected Diocese(s) in", input$cl_country, "")
        }
        
        p <- ggplot(cr, aes(x = reorder(label, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          plot_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(title = chart_title, x = "Diocese", y = input$cl_var)
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$download_cl_trend_png <- downloadHandler(
    filename = function() {
      paste("cl_trend-", input$cl_var, "-", input$cl_country, "-", input$cl_time_period, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tr <- cl_trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        time_period_title <- switch(
          input$cl_time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
          "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
        )
        
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Top 8 Dioceses in ", input$cl_country)
          } else {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – All Dioceses in ", input$cl_country)
          }
        } else {
          chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Selected Diocese(s) in ", input$cl_country)
        }
        
        p <- ggplot2::ggplot(tr, ggplot2::aes(x = Year_process, y = value, color = label, group = label)) +
          ggplot2::geom_line(linewidth = 1) + 
          ggplot2::geom_point(size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::theme( axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
                          axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
                          axis.text = element_text(size = 10, color = "#7f8c8d")) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          ggplot2::labs(title = chart_title,
                        x = "Year", y = input$cl_var, color = "Diocese")
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_cl_trend_pdf <- downloadHandler(
    filename = function() {
      paste("cl_trend-", input$cl_var, "-", input$cl_country, "-", input$cl_time_period, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tr <- cl_trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        time_period_title <- switch(
          input$cl_time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
          "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
        )
        
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Top 8 Dioceses in ", input$cl_country)
          } else {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – All Dioceses in ", input$cl_country)
          }
        } else {
          chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Selected Diocese(s) in ", input$cl_country)
        }
        
        p <- ggplot2::ggplot(tr, ggplot2::aes(x = Year_process, y = value, color = label, group = label)) +
          ggplot2::geom_line(linewidth = 1) + 
          ggplot2::geom_point(size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::theme( axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
                          axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
                          axis.text = element_text(size = 10, color = "#7f8c8d")) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          ggplot2::labs(title = chart_title,
                        x = "Year", y = input$cl_var, color = "Diocese")
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$download_dl_trend_png <- downloadHandler(
    filename = function() {
      paste("dl_trend-", paste(input$dl_var, collapse = "_"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = Year, y = value)) +
                geom_line(linewidth = 1.2, color = "#3498db") +
                geom_point(size = 3, color = "#3498db", shape = 19, fill = "white", stroke = 1.5) +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(df_filtered$Year), labels = as.integer(unique(df_filtered$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("", var, " Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = Year, y = Value, color = Variable, group = Variable)) +
                geom_line(linewidth = 1.2) +
                geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
                scale_color_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(long_df$Year), labels = as.integer(unique(long_df$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("Selected Variables Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = "Value",
                  color = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_dl_trend_pdf <- downloadHandler(
    filename = function() {
      paste("dl_trend-", paste(input$dl_var, collapse = "_"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = Year, y = value)) +
                geom_line(linewidth = 1.2, color = "#3498db") +
                geom_point(size = 3, color = "#3498db", shape = 19, fill = "white", stroke = 1.5) +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(df_filtered$Year), labels = as.integer(unique(df_filtered$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("", var, " Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = Year, y = Value, color = Variable, group = Variable)) +
                geom_line(linewidth = 1.2) +
                geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
                scale_color_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(long_df$Year), labels = as.integer(unique(long_df$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("Selected Variables Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = "Value",
                  color = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$download_dl_bar_png <- downloadHandler(
    filename = function() {
      paste("dl_bar-", paste(input$dl_var, collapse = "_"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = factor(Year), y = value)) +
                geom_bar(stat = "identity", fill = "#3498db") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(df_filtered$Year)))) +
                labs(
                  title = paste0("", var, " by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = factor(Year), y = Value, fill = Variable)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_fill_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(long_df$Year)))) +
                labs(
                  title = paste0("Selected Variables by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = "Value",
                  fill = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_dl_bar_pdf <- downloadHandler(
    filename = function() {
      paste("dl_bar-", paste(input$dl_var, collapse = "_"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = factor(Year), y = value)) +
                geom_bar(stat = "identity", fill = "#3498db") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(df_filtered$Year)))) +
                labs(
                  title = paste0("", var, " by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = factor(Year), y = Value, fill = Variable)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_fill_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(long_df$Year)))) +
                labs(
                  title = paste0("Selected Variables by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = "Value",
                  fill = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$trendPlot <- renderPlot({
    tr <- trend_reactive()
    if (is.null(tr) || nrow(tr) == 0) { 
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1,"No data")
      return()
    }
    
    # Determine time period for title
    time_period_title <- switch(
      input$time_period,
      "before_2010" = "1950–2010",
      "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
      "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    
    title_text <- if (length(input$countries) == 0) {
      paste0(input$var, " Over Time, ", time_period_title, " – Top 10 Countries")
    } else {
      paste0(input$var, " Over Time, ", time_period_title, " – Selected Countries")
    }
    
    ggplot(tr, aes(x = Year_process, y = value, color = Country, group = Country)) +
      geom_line(linewidth = 1) + 
      geom_point(size = 2) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
        axis.text = element_text(size = 10, color = "#7f8c8d")
      ) +
      scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
      labs(
        title = title_text,
        x = "Year",
        y = input$var,
        color = "Country"
      )
  })
  
  # Download handler for trendPlot
  output$download_trendPlot <- downloadHandler(
    filename = function() {
      paste("trendPlot-", input$var, "-", input$time_period, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      tr <- trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        # Determine time period for title
        time_period_title <- switch(
          input$time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y"))
        )
        
        title_text <- if (length(input$countries) == 0) {
          paste0(input$var, " Over Time, ", time_period_title, " – Top 10 Countries")
        } else {
          paste0(input$var, " Over Time, ", time_period_title, " – Selected Countries")
        }
        
        p <- ggplot(tr, aes(x = Year_process, y = value, color = Country, group = Country)) +
          geom_line(linewidth = 1) + 
          geom_point(size = 2) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text = element_text(size = 10, color = "#7f8c8d")
          ) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(
            title = title_text,
            x = "Year",
            y = input$var,
            color = "Country"
          )
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_barChart_png <- downloadHandler(
    filename = function() {
      paste("barChart-", input$var, "-", input$year, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      data_list <- data_reactive()
      if (is.null(data_list)) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "Please select a year first") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
      } else if (is.null(data_list$agg_current) || nrow(data_list$agg_current) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        agg_current <- data_list$agg_current %>% arrange(desc(value))
        if (length(input$countries) == 0) agg_current <- head(agg_current, 10)
        
        chart_title <- if (length(input$countries) == 0) paste("", input$var, "by Country in", input$year, " – Top 10 Countries")
        else paste("", input$var, "by Country in", input$year, " – Selected Countries")
        
        p <- ggplot(agg_current, aes(x = reorder(Country, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title   = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title   = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text    = element_text(size = 10, color = "#7f8c8d")
          )  +
          labs(title = chart_title, x = "Country", y = input$var)
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_barChart_pdf <- downloadHandler(
    filename = function() {
      paste("barChart-", input$var, "-", input$year, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      data_list <- data_reactive()
      if (is.null(data_list)) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "Please select a year first") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
      } else if (is.null(data_list$agg_current) || nrow(data_list$agg_current) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        agg_current <- data_list$agg_current %>% arrange(desc(value))
        if (length(input$countries) == 0) agg_current <- head(agg_current, 10)
        
        chart_title <- if (length(input$countries) == 0) paste("", input$var, "by Country in", input$year, " – Top 10 Countries")
        else paste("", input$var, "by Country in", input$year, " – Selected Countries")
        
        p <- ggplot(agg_current, aes(x = reorder(Country, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title   = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title   = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text    = element_text(size = 10, color = "#7f8c8d")
          )  +
          labs(title = chart_title, x = "Country", y = input$var)
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$download_trendPlot_png <- downloadHandler(
    filename = function() {
      paste("trendPlot-", input$var, "-", input$time_period, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tr <- trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        # Determine time period for title
        time_period_title <- switch(
          input$time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y"))
        )
        
        title_text <- if (length(input$countries) == 0) {
          paste0(input$var, " Over Time, ", time_period_title, " – Top 10 Countries")
        } else {
          paste0(input$var, " Over Time, ", time_period_title, " – Selected Countries")
        }
        
        p <- ggplot(tr, aes(x = Year_process, y = value, color = Country, group = Country)) +
          geom_line(linewidth = 1) + 
          geom_point(size = 2) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text = element_text(size = 10, color = "#7f8c8d")
          ) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(
            title = title_text,
            x = "Year",
            y = input$var,
            color = "Country"
          )
      }
      ggsave(file, p, device = "png", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$download_trendPlot_pdf <- downloadHandler(
    filename = function() {
      paste("trendPlot-", input$var, "-", input$time_period, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tr <- trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) { 
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        # Determine time period for title
        time_period_title <- switch(
          input$time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y"))
        )
        
        title_text <- if (length(input$countries) == 0) {
          paste0(input$var, " Over Time, ", time_period_title, " – Top 10 Countries")
        } else {
          paste0(input$var, " Over Time, ", time_period_title, " – Selected Countries")
        }
        
        p <- ggplot(tr, aes(x = Year_process, y = value, color = Country, group = Country)) +
          geom_line(linewidth = 1) + 
          geom_point(size = 2) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
            axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
            axis.text = element_text(size = 10, color = "#7f8c8d")
          ) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(
            title = title_text,
            x = "Year",
            y = input$var,
            color = "Country"
          )
      }
      ggsave(file, p, device = "pdf", width = 12, height = 8)
    }
  )
  
  output$trendTable <- renderTable({
    tr <- trend_reactive()
    if (is.null(tr) || nrow(tr) == 0) return(NULL)
    
    tr %>%
      dplyr::rename(Year = Year_process) %>%
      dplyr::mutate(value = if (input$var %in% c("Percent.Catholic")) round(value, 2) else round(value, 0)) %>%
      dplyr::select(Country, Year, value) %>%
      tidyr::pivot_wider(names_from = Year, values_from = value) %>%
      dplyr::arrange(Country)
  }, bordered = TRUE, striped = TRUE)
  
  # Download handler for trendTable
  output$download_trendTable <- downloadHandler(
    filename = function() {
      paste("trendTable-", input$var, "-", input$time_period, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tr <- trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
      } else {
        table_data <- tr %>%
          dplyr::rename(Year = Year_process) %>%
          dplyr::mutate(value = if (input$var %in% c("Percentage of Catholics")) round(value, 2) else round(value, 0)) %>%
          dplyr::select(Country, Year, value) %>%
          tidyr::pivot_wider(names_from = Year, values_from = value) %>%
          dplyr::arrange(Country)
        write.csv(table_data, file, row.names = FALSE)
      }
    }
  )
  
  # ---------- Country-Level (URL as Diocese/Unit) ----------
  
  output$cl_dynamic_year_slider <- renderUI({
    current_data <- reactive_data()
    available_years <- sort(unique(current_data$Year_process))
    
    if (input$cl_time_period == "before_2010") {
      years_before <- available_years[available_years <= 2010]
      if (length(years_before) == 0) {
        return(helpText("No data available for this time period"))
      }
      sliderTextInput(
        inputId = "cl_year",
        label = "Year:",
        choices = years_before,
        selected = 2000,
        grid = TRUE,
        animate = TRUE
      )
    } else {
      years_after <- available_years[available_years >= 2010]
      if (length(years_after) == 0) {
        return(helpText("No data available for this time period"))
      }
      sliderTextInput(
        inputId = "cl_year",
        label = "Year:",
        choices = years_after,
        selected = 2020,
        grid = TRUE,
        animate = TRUE
      )
    }
  })
  
  cl_choice_df <- reactive({
    req(input$cl_country)
    current_data <- reactive_data()
    df <- current_data %>%
      dplyr::filter(trimws(Country) == trimws(input$cl_country)) %>%
      dplyr::filter(!is.na(diocese), diocese != "") %>%
      dplyr::distinct(diocese)
    if (nrow(df) == 0) tibble::tibble(diocese = character(0)) else df
  })
  
  output$cl_diocese_ui <- renderUI({
    chdf <- cl_choice_df()
    if (nrow(chdf) == 0)
      return(helpText("No dioceses found for the selected country."))
    selectizeInput(
      "cl_dioceses", "Dioceses",
      choices = sort(chdf$diocese),
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Select one or more dioceses")
    )
  })
  
  top_dioceses_current <- function(country, year, var, mode) {
    current_data <- reactive_data()
    df <- current_data %>%
      dplyr::filter(trimws(Country) == trimws(country), Year_process == as.integer(year)) %>%
      dplyr::filter(!is.na(diocese), diocese != "")
    if (nrow(df) == 0 || !"diocese" %in% names(df)) return(character(0))
    
    bases <- var_info[[var]]$bases
    flags <- var_info[[var]]$flags
    compute_func <- var_info[[var]]$compute
    
    if (mode == "Non-Imputed" && all(flags %in% names(df))) {
      keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
      df <- df[keep, , drop = FALSE]
    }
    
    agg <- df %>%
      dplyr::group_by(diocese) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
    if (nrow(agg) == 0) return(character(0))
    
    agg$value <- apply(agg[, bases, drop = FALSE], 1, function(row) {
      s <- as.list(row)
      names(s) = bases
      compute_func(s)
    })
    
    agg %>% 
      dplyr::filter(!is.na(value), is.finite(value)) %>% 
      dplyr::arrange(dplyr::desc(value)) %>% 
      dplyr::slice(1:8) %>% 
      dplyr::pull(diocese)
  }
  
  observeEvent(list(input$cl_country, input$cl_year, input$cl_var, input$cl_mode), {
    req(input$cl_year)
    
    chdf <- cl_choice_df()
    choices_vec <- sort(chdf$diocese)
    sel <- input$cl_dioceses
    if (is.null(sel) || length(sel) == 0) {
      sel <- top_dioceses_current(input$cl_country, input$cl_year, input$cl_var, input$cl_mode)
    } else {
      sel <- intersect(sel, choices_vec)
    }
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "cl_dioceses",
      choices = choices_vec,
      selected = sel,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  cl_current_reactive <- reactive({
    req(input$cl_country, input$cl_year, input$cl_var, input$cl_mode)
    current_data <- reactive_data()
    year <- as.integer(input$cl_year)
    var <- input$cl_var
    mode <- input$cl_mode
    
    df <- current_data %>%
      dplyr::filter(trimws(Country) == trimws(input$cl_country), Year_process == year) %>%
      dplyr::filter(!is.na(diocese), diocese != "")
    if (!"diocese" %in% names(df) || nrow(df) == 0) return(NULL)
    
    bases <- var_info[[var]]$bases
    flags <- var_info[[var]]$flags
    compute_func <- var_info[[var]]$compute
    if (mode == "Non-Imputed" && all(flags %in% names(df))) {
      keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
      df <- df[keep, , drop = FALSE]
    }
    
    agg <- df %>%
      dplyr::group_by(diocese) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
    
    if (nrow(agg) == 0) return(NULL)
    
    agg$value <- apply(agg[, bases, drop = FALSE], 1, function(row) {
      s <- as.list(row)
      names(s) <- bases
      compute_func(s)
    })
    
    # Always filter by selected dioceses, fallback to top 8 if none selected
    if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      want <- top_dioceses_current(input$cl_country, input$cl_year, input$cl_var, input$cl_mode)
    } else {
      want <- trimws(as.character(input$cl_dioceses))
    }
    agg <- agg %>% dplyr::filter(diocese %in% want)
    
    agg %>% 
      dplyr::filter(!is.na(value), is.finite(value)) %>% 
      dplyr::arrange(dplyr::desc(value)) %>% 
      dplyr::rename(label = diocese)
  })
  
  cl_trend_reactive <- reactive({
    req(input$cl_country, input$cl_var, input$cl_mode, input$cl_time_period, input$cl_year)
    current_data <- reactive_data()
    var <- input$cl_var
    mode <- input$cl_mode
    time_period <- input$cl_time_period
    
    df <- current_data %>%
      dplyr::filter(trimws(Country) == trimws(input$cl_country), !is.na(diocese), diocese != "")
    
    if (time_period == "after_2010") {
      df <- df %>% dplyr::filter(Year_process >= 2010)
    } else if (time_period == "before_2010") {
      df <- df %>% dplyr::filter(Year_process >= 1950, Year_process <= 2010)
    }
    
    if (!"diocese" %in% names(df) || nrow(df) == 0) return(NULL)
    
    bases <- var_info[[var]]$bases
    flags <- var_info[[var]]$flags
    compute_func <- var_info[[var]]$compute
    if (mode == "Non-Imputed" && all(flags %in% names(df))) {
      keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
      df <- df[keep, , drop = FALSE]
    }
    
    agg <- df %>%
      dplyr::group_by(Year_process, diocese) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
    if (nrow(agg) == 0) return(NULL)
    
    agg$value <- apply(agg[, bases, drop = FALSE], 1, function(row) {
      s <- as.list(row)
      names(s) <- bases
      compute_func(s)
    })
    
    if (!is.null(input$cl_dioceses) && length(input$cl_dioceses) > 0) {
      want <- trimws(as.character(input$cl_dioceses))
      agg <- agg %>% dplyr::filter(diocese %in% want)
    } else {
      top_ds <- top_dioceses_current(input$cl_country, input$cl_year, input$cl_var, input$cl_mode)
      if (length(top_ds) > 0) agg <- agg %>% dplyr::filter(diocese %in% trimws(as.character(top_ds)))
    }
    
    agg %>% dplyr::rename(label = diocese)
  })
  
  output$cl_trend <- renderPlot({
    tr <- cl_trend_reactive()
    if (is.null(tr) || nrow(tr) == 0) { 
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1,"No data")
      return()
    }
    time_period_title <- switch(
      input$cl_time_period,
      "before_2010" = "1950–2010",
      "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
      "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    
    total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
    var_label <- input$cl_var
    
    if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      if (!is.na(total_ds) && total_ds > 8) {
        chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Top 8 Dioceses in ", input$cl_country)
      } else {
        chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – All Dioceses in ", input$cl_country)
      }
    } else {
      chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Selected Diocese(s) in ", input$cl_country)
    }
    ggplot2::ggplot(tr, ggplot2::aes(x = Year_process, y = value, color = label, group = label)) +
      ggplot2::geom_line(linewidth = 1) + 
      ggplot2::geom_point(size = 2) +
      ggplot2::theme_minimal() +
      ggplot2::theme( axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
                      axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
                      axis.text = element_text(size = 10, color = "#7f8c8d")) +
      scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      ggplot2::labs(title = chart_title,
                    x = "Year", y = input$cl_var, color = "Diocese")
  })
  
  # Download handler for cl_trend
  output$download_cl_trend <- downloadHandler(
    filename = function() {
      paste("cl_trend-", input$cl_var, "-", input$cl_country, "-", input$cl_time_period, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      tr <- cl_trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        time_period_title <- switch(
          input$cl_time_period,
          "before_2010" = "1950–2010",
          "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
          "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
        )
        
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Top 8 Dioceses in ", input$cl_country)
          } else {
            chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – All Dioceses in ", input$cl_country)
          }
        } else {
          chart_title <- paste0(var_label, " Over Time, ", time_period_title, " – Selected Diocese(s) in ", input$cl_country)
        }
        
        p <- ggplot2::ggplot(tr, ggplot2::aes(x = Year_process, y = value, color = label, group = label)) +
          ggplot2::geom_line(linewidth = 1) + 
          ggplot2::geom_point(size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::theme( axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                          plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
                          axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
                          axis.text = element_text(size = 10, color = "#7f8c8d")) +
          scale_x_continuous(breaks = unique(tr$Year_process), labels = as.integer(unique(tr$Year_process))) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          ggplot2::labs(title = chart_title,
                        x = "Year", y = input$cl_var, color = "Diocese")
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$cl_table <- renderTable({
    tr <- cl_trend_reactive()
    if (is.null(tr) || nrow(tr) == 0) return(NULL)
    
    tr %>%
      dplyr::rename(Year = Year_process) %>%
      dplyr::select(label, Year, value) %>%
      tidyr::pivot_wider(names_from = Year, values_from = value) %>%
      dplyr::arrange(label)
  }, bordered = TRUE, striped = TRUE)
  
  # Download handler for cl_table
  output$download_cl_table <- downloadHandler(
    filename = function() {
      paste("cl_table-", input$cl_var, "-", input$cl_country, "-", input$cl_time_period, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tr <- cl_trend_reactive()
      if (is.null(tr) || nrow(tr) == 0) {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
      } else {
        table_data <- tr %>%
          dplyr::rename(Year = Year_process) %>%
          dplyr::select(label, Year, value) %>%
          tidyr::pivot_wider(names_from = Year, values_from = value) %>%
          dplyr::arrange(label)
        write.csv(table_data, file, row.names = FALSE)
      }
    }
  )
  
  output$cl_table_annotation <- renderUI({
    HTML("<p style='font-size: 12px; color: #7f8c8d;'>Note: Values represent imputed data for Catholics. Data sourced from [insert source]. Missing values are denoted as 0.00.</p>")
  })
  
  # ---------- Country-Level Bar Chart ----------
  output$cl_barChartDescription <- renderUI({
    req(input$cl_year, input$cl_var, input$cl_country) # Ensure year, variable, and country are selected
    
    time_period_text <- switch(input$cl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010" = paste0("2010–", format(Sys.Date(), "%Y"))
    )
    mode_text <- switch(input$cl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle diocese selection
    diocese_text <- if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      "the top diocese(s)"
    } else if (nrow(cl_choice_df()) == 1) 
    {"a single diocese"
    }
    else{
      paste("the selected diocese(s) (", paste(input$cl_dioceses, collapse = ", "), ")", sep = "")
    }
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>
      This bar chart presents data on the <strong>", input$cl_var, "</strong> in <strong>",input$cl_year,"</strong>
      of <strong>", diocese_text, "</strong> in <strong>", input$cl_country, "</strong>.
      The dioceses have been ordered in descending order to facilitate straightforward comparison.
      The data is based on <strong>", mode_text, "</strong>.",
      "</p>"
    ))
  })
  
  output$cl_trendChartDescription <- renderUI({
    req(input$cl_time_period, input$cl_var, input$cl_country, input$cl_mode)
    time_period_text <- switch(input$cl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010" = paste0("2010–", format(Sys.Date(), "%Y"))
    )
    # Ensure required inputs are available
    
    time_period_text <- switch(input$cl_time_period,
                               "before_2010" = "1950–2010",
                               "after_2010" = paste0("2010–", format(Sys.Date(), "%Y"))
    )
    mode_text <- switch(input$cl_mode,
                        "Imputed" = "statistically processed data",
                        "Non-Imputed" = "raw data")
    
    # Handle diocese selection
    diocese_text <- if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      "the top dioceses"
    } else if (nrow(cl_choice_df()) == 1) 
    {"a single diocese"
    }
    else{
      paste("the selected diocese(s) (", paste(input$cl_dioceses, collapse = ", "), ")", sep = "")
    }
    
    # Conditional data description based on mode
    data_description <-  ("This line chart presents trends over time for the selected variable")
    
    HTML(paste0(
      "<p style='font-size: 18px; line-height: 1.6; color: #2c3e50; font-family: Arial, sans-serif;'>",
      data_description, " <strong>", input$cl_var, "</strong> across <strong>", diocese_text, "</strong> in 
    <strong>", input$cl_country, "</strong>. ", # <--- 修正 2: input$cl_var
      "Each line represents a diocese, with distinct colors to facilitate straightforward comparison. ",
      "The chart enables users to observe growth, decline, or stability of the selected variable. ",
      "The data is represents here covers the time period <strong>", time_period_text, "</strong> and is based on ", 
      switch(input$cl_mode, "Imputed" = "<strong>statistically processed data</strong>", # <--- 修正 3: input$cl_mode
             "Non-Imputed" = "<strong>raw data</strong>"), ".",
      "</p>"
    ))
  })
  
  output$cl_barChart <- renderPlot({
    df <- cl_current_reactive()
    if (is.null(df) || nrow(df) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No data")
      return()
    }
    total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
    var_label <- input$cl_var
    
    if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
      if (!is.na(total_ds) && total_ds > 8) {
        chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Top 8 Dioceses in", input$cl_country, "")
      } else {
        chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– All Dioceses in", input$cl_country, "")
      }
    } else {
      chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Selected Diocese(s) in", input$cl_country, "")
    }
    
    ggplot(df, aes(x = reorder(label, -value), y = value)) +
      geom_bar(stat = "identity", fill = "steelblue") + # Use orange to match 'warning' status
      plot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
      labs(title = chart_title, x = "Diocese", y = input$cl_var)
  })
  
  # Download handler for cl_barChart
  output$download_cl_barChart <- downloadHandler(
    filename = function() {
      paste("cl_barChart-", input$cl_var, "-", input$cl_country, "-", input$cl_year, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      df <- cl_current_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        total_ds <- tryCatch(nrow(cl_choice_df()), error = function(e) NA_integer_)
        var_label <- input$cl_var
        
        if (is.null(input$cl_dioceses) || length(input$cl_dioceses) == 0) {
          if (!is.na(total_ds) && total_ds > 8) {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Top 8 Dioceses in", input$cl_country, "")
          } else {
            chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– All Dioceses in", input$cl_country, "")
          }
        } else {
          chart_title <- paste("", input$cl_var, "by Dioceses in", input$cl_year ,"– Selected Diocese(s) in", input$cl_country, "")
        }
        
        p <- ggplot(df, aes(x = reorder(label, -value), y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          plot_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) + 
          labs(title = chart_title, x = "Diocese", y = input$cl_var)
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
  
  # ---------- Diocese-Level ----------
  
  # ---------- Diocese-Level ----------
  
  dl_choice_df <- reactive({
    search_term <- input$dl_diocese_search
    current_data <- reactive_data()
    
    # Retrieve all unique dioceses and their associated countries
    df <- current_data %>%
      dplyr::filter(!is.na(diocese), diocese != "") %>%
      dplyr::select(diocese, Country) %>%
      dplyr::distinct()
    
    # Apply search term filter if provided
    if (!is.null(search_term) && search_term != "") {
      df <- df %>% dplyr::filter(stringr::str_detect(diocese, fixed(search_term, ignore_case = TRUE)))
    }
    
    df
  })
  
  output$dl_diocese_ui <- renderUI({
    choices_df <- dl_choice_df()
    if (nrow(choices_df) == 0) {
      return(helpText("No dioceses found for the search criteria."))
    }
    
    choices_vec <- sort(choices_df$diocese)
    selectizeInput(
      "dl_diocese",
      "Diocese",
      choices = choices_vec,
      selected = if (length(choices_vec) > 0) choices_vec[1] else NULL,
      options = list(
        placeholder = "Select a diocese",
        plugins = list("remove_button")
      )
    )
  })
  
  # Synchronize dl_country with selected diocese
  observeEvent(input$dl_diocese, {
    req(input$dl_diocese)
    current_data <- reactive_data()
    diocese_country <- current_data %>%
      dplyr::filter(diocese == input$dl_diocese) %>%
      dplyr::select(Country) %>%
      dplyr::distinct() %>%
      dplyr::pull(Country)
    
    if (length(diocese_country) > 0) {
      updateSelectInput(
        session = session,
        inputId = "dl_country",
        selected = diocese_country[1]
      )
    }
  })
  
  # Clear search term when country changes (optional, to avoid confusion)
  observeEvent(input$dl_country, {
    updateTextInput(session, "dl_diocese_search", value = "")
  })
  
  dl_general_info_reactive <- reactive({
    req(input$dl_diocese)
    current_data <- reactive_data()
    df <- current_data %>% dplyr::filter(diocese == input$dl_diocese) %>% dplyr::slice(1)
    
    # 只包含非 NA 的字段
    info <- list()
    if (!is.na(df$diocese)) info[["diocese"]] <- df$diocese
    if (!is.na(df$Country)) info[["Country"]] <- df$Country
    if (!is.na(df$url)) info[["url"]] <- df$url
    if (!is.na(df$Mailing.Address)) info[["Mailing.Address"]] <- df$Mailing.Address
    if (!is.na(df$Rite)) info[["Rite"]] <- df$Rite
    if (!is.na(df$Official.Web.Site)) info[["Official.Web.Site"]] <- df$Official.Web.Site
    if (!is.na(df$Telephone)) info[["Telephone"]] <- df$Telephone
    if (!is.na(df$Additional.Notes)) info[["Additional.Notes"]] <- df$Additional.Notes
    
    # 如果 info 为空，则返回空内容
    if (length(info) == 0) {
      return(HTML("<p>No information available.</p>"))
    }
    
    # 生成 HTML 表格
    info_html <- paste0(
      "<table style='width: 100%; border-collapse: collapse;'>",
      paste(sapply(names(info), function(item) {
        value <- info[[item]]
        # 映射字段名到简单易懂的表述
        item_label <- switch(item,
                             "diocese" = "Diocese Name",
                             "Country" = "Country",
                             "url" = "Website Link",
                             "Mailing.Address" = "Mailing Address",
                             "Rite" = "Church Rite",
                             "Official.Web.Site" = "Official Website",
                             "Telephone" = "Phone Number",
                             "Additional.Notes" = "Extra Notes",
                             sub("\\\\.", " ", item) # 默认情况保持原样
        )
        paste0(
          "<tr>",
          "<td style='padding: 5px; font-weight: bold; width: 30%;'>",
          item_label,
          "</td>",
          "<td style='padding: 5px;'>",
          if (item %in% c("url", "Official.Web.Site")) {
            paste0("<a href='", value, "' target='_blank'>", value, "</a>")
          } else {
            value
          },
          "</td>",
          "</tr>"
        )
      }), collapse = ""),
      "</table>"
    )
    
    HTML(info_html)
  })
  
  output$dl_general_info <- renderUI({
    dl_general_info_reactive()
  })
  
  
  
  dl_data_reactive <- reactive({
    req(input$dl_diocese, input$dl_var, input$dl_mode, input$dl_time_period)
    current_data <- reactive_data()
    vars <- input$dl_var
    if (length(vars) == 0) return(NULL)
    mode <- input$dl_mode
    time_period <- input$dl_time_period
    
    df <- current_data %>%
      dplyr::filter(diocese == input$dl_diocese)
    
    if (time_period == "before_2010") {
      df <- df %>% dplyr::filter(Year_process <= 2010)
    } else if (time_period == "after_2010") {
      df <- df %>% dplyr::filter(Year_process >= 2010)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    if (length(vars) == 1) {
      var <- vars[1]
      bases <- var_info[[var]]$bases
      flags <- var_info[[var]]$flags
      compute_func <- var_info[[var]]$compute
      
      if (mode == "Non-Imputed" && all(flags %in% names(df))) {
        keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
        df_filtered <- df[keep, , drop = FALSE]
      } else {
        df_filtered <- df
      }
      
      if (nrow(df_filtered) == 0) return(NULL)
      
      agg <- df_filtered %>%
        dplyr::group_by(Year_process) %>%
        dplyr::summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
      
      if (nrow(agg) == 0) return(NULL)
      
      agg[[var]] <- apply(agg[, bases, drop = FALSE], 1, function(row) {
        s <- as.list(row)
        names(s) <- bases
        compute_func(s)
      })
      
      agg %>%
        dplyr::select(Year = Year_process, !!sym(var)) %>%
        dplyr::arrange(Year)
    } else {
      result_list <- list()
      for (var in vars) {
        bases <- var_info[[var]]$bases
        flags <- var_info[[var]]$flags
        compute_func <- var_info[[var]]$compute
        
        if (mode == "Non-Imputed" && all(flags %in% names(df))) {
          keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
          df_filtered <- df[keep, , drop = FALSE]
        } else {
          df_filtered <- df
        }
        
        if (nrow(df_filtered) == 0) {
          result_list[[var]] <- NULL
          next
        }
        
        agg <- df_filtered %>%
          dplyr::group_by(Year_process) %>%
          dplyr::summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
        
        agg[[var]] <- apply(agg[, bases, drop = FALSE], 1, function(row) {
          s <- as.list(row)
          names(s) <- bases
          compute_func(s)
        })
        
        result_list[[var]] <- agg %>% dplyr::select(Year_process, !!var := !!sym(var))
      }
      
      if (length(result_list) == 0 || all(sapply(result_list, is.null))) return(NULL)
      
      result_list <- Filter(Negate(is.null), result_list)
      if (length(result_list) == 0) return(NULL)
      
      merged_df <- result_list[[1]]
      for (i in 2:length(result_list)) {
        if (!is.null(result_list[[i]])) {
          merged_df <- merged_df %>% dplyr::full_join(result_list[[i]], by = "Year_process")
        }
      }
      
      merged_df %>%
        dplyr::rename(Year = Year_process) %>%
        dplyr::arrange(Year)
    }
  })
  
  output$dl_data_table <- renderDT({
    df <- dl_data_reactive()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data available"), options = list(pageLength = 5)))
    }
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'tip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(df),
        backgroundColor = '#f5f6f5',
        fontWeight = 'normal',
        color = '#2c3e50'
      ) %>%
      formatStyle(
        'Year',
        fontWeight = 'bold'
      )
  })
  
  # Download handler for dl_data_table
  output$download_dl_data_table <- downloadHandler(
    filename = function() {
      paste("dl_data_table-", input$dl_diocese, "-", paste(input$dl_var, collapse = "-"), "-", input$dl_time_period, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- dl_data_reactive()
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  dl_trend_reactive <- reactive({
    req(input$dl_diocese, input$dl_var, input$dl_mode, input$dl_time_period)
    current_data <- reactive_data()
    vars <- input$dl_var
    if (length(vars) == 0) return(NULL)
    mode <- input$dl_mode
    time_period <- input$dl_time_period
    
    df <- current_data %>%
      dplyr::filter(diocese == input$dl_diocese)
    
    if (time_period == "before_2010") {
      df <- df %>% dplyr::filter(Year_process <= 2010)
    } else if (time_period == "after_2010") {
      df <- df %>% dplyr::filter(Year_process >= 2010)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    if (length(vars) == 1) {
      var <- vars[1]
      bases <- var_info[[var]]$bases
      flags <- var_info[[var]]$flags
      compute_func <- var_info[[var]]$compute
      
      if (mode == "Non-Imputed" && all(flags %in% names(df))) {
        keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
        df_filtered <- df[keep, , drop = FALSE]
      } else {
        df_filtered <- df
      }
      
      if (nrow(df_filtered) == 0) return(NULL)
      
      agg <- df_filtered %>%
        dplyr::group_by(Year_process) %>%
        dplyr::summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
      
      if (nrow(agg) == 0) return(NULL)
      
      agg$value <- apply(agg[, bases, drop = FALSE], 1, function(row) {
        s <- as.list(row)
        names(s) <- bases
        compute_func(s)
      })
      
      agg %>%
        dplyr::select(Year = Year_process, value) %>%
        dplyr::arrange(Year)
    } else {
      result_list <- list()
      for (var in vars) {
        bases <- var_info[[var]]$bases
        flags <- var_info[[var]]$flags
        compute_func <- var_info[[var]]$compute
        
        if (mode == "Non-Imputed" && all(flags %in% names(df))) {
          keep <- apply(df[, flags, drop = FALSE] == 0, 1, all)
          df_filtered <- df[keep, , drop = FALSE]
        } else {
          df_filtered <- df
        }
        
        if (nrow(df_filtered) == 0) {
          result_list[[var]] <- NULL
          next
        }
        
        agg <- df_filtered %>%
          dplyr::group_by(Year_process) %>%
          dplyr::summarise(across(all_of(bases), ~sum(., na.rm = TRUE)), .groups = "drop")
        
        agg[[var]] <- apply(agg[, bases, drop = FALSE], 1, function(row) {
          s <- as.list(row)
          names(s) <- bases
          compute_func(s)
        })
        
        result_list[[var]] <- agg %>% dplyr::select(Year_process, !!var := !!sym(var))
      }
      
      if (length(result_list) == 0) return(NULL)
      merged_df <- result_list[[1]]
      for (i in 2:length(result_list)) {
        if (!is.null(result_list[[i]])) {
          merged_df <- merged_df %>% dplyr::full_join(result_list[[i]], by = "Year_process")
        }
      }
      
      merged_df %>%
        dplyr::rename(Year = Year_process) %>%
        dplyr::arrange(Year)
    }
  })
  
  output$dl_bar <- renderPlot({
    df <- dl_trend_reactive()
    if (is.null(df) || nrow(df) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No data")
      return()
    }
    
    vars <- input$dl_var
    if (length(vars) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No variables selected")
      return()
    }
    
    time_period_title <- switch(
      input$dl_time_period,
      "before_2010" = "1950–2010",
      "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
      "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    
    
    if (length(vars) == 1) {
      var <- vars[1]
      df <- df %>% dplyr::filter(!is.na(value))
      if (nrow(df) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="")
        text(1, 1, paste("No valid data for", var))
        return()
      }
      ggplot(df, aes(x = factor(Year), y = value)) +
        geom_bar(stat = "identity", fill = "#3498db") +
        plot_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        scale_x_discrete(labels = as.integer(levels(factor(df$Year)))) +
        labs(
          title = paste0("", var, " by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
          x = "Year",
          y = var
        )
    } else {
      long_df <- df %>%
        tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
        dplyr::filter(!is.na(Value))
      
      if (nrow(long_df) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="")
        text(1, 1, "No valid data for selected variables")
        return()
      }
      
      ggplot(long_df, aes(x = factor(Year), y = Value, fill = Variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        plot_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        scale_x_discrete(labels = as.integer(levels(factor(long_df$Year)))) +
        labs(
          title = paste0("Selected Variables by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
          x = "Year",
          y = "Value",
          fill = "Variable"
        )
    }
  })
  
  # Download handler for dl_bar
  output$download_dl_bar <- downloadHandler(
    filename = function() {
      paste("dl_bar-", paste(input$dl_var, collapse = "-"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = factor(Year), y = value)) +
                geom_bar(stat = "identity", fill = "#3498db") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(df_filtered$Year)))) +
                labs(
                  title = paste0("", var, " by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = factor(Year), y = Value, fill = Variable)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_fill_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_discrete(labels = as.integer(levels(factor(long_df$Year)))) +
                labs(
                  title = paste0("Selected Variables by Year, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = "Value",
                  fill = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
  
  output$dl_trend <- renderPlot({
    df <- dl_trend_reactive()
    if (is.null(df) || nrow(df) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No data")
      return()
    }
    
    vars <- input$dl_var
    if (length(vars) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No variables selected")
      return()
    }
    
    time_period_title <- switch(
      input$dl_time_period,
      "before_2010" = "1950–2010",
      "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
      "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
    )
    
    if (length(vars) == 1) {
      var <- vars[1]
      df <- df %>% dplyr::filter(!is.na(value))
      if (nrow(df) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="")
        text(1, 1, paste("No valid data for", var))
        return()
      }
      ggplot(df, aes(x = Year, y = value)) +
        geom_line(linewidth = 1.2, color = "#3498db") +
        geom_point(size = 3, color = "#3498db", shape = 19, fill = "white", stroke = 1.5) +
        plot_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        scale_x_continuous(breaks = unique(df$Year), labels = as.integer(unique(df$Year))) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
        labs(
          title = paste0("", var, " Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
          x = "Year",
          y = var
        )
    } else {
      long_df <- df %>%
        tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
        dplyr::filter(!is.na(Value))
      
      if (nrow(long_df) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="")
        text(1, 1, "No valid data for selected variables")
        return()
      }
      
      ggplot(long_df, aes(x = Year, y = Value, color = Variable, group = Variable)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
        scale_color_brewer(palette = "Set1") +
        plot_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        scale_x_continuous(breaks = unique(long_df$Year), labels = as.integer(unique(long_df$Year))) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
        labs(
          title = paste0("Selected Variables Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
          x = "Year",
          y = "Value",
          color = "Variable"
        )
    }
  })
  
  # Download handler for dl_trend
  output$download_dl_trend <- downloadHandler(
    filename = function() {
      paste("dl_trend-", paste(input$dl_var, collapse = "-"), "-", input$dl_diocese, "-", input$dl_time_period, "-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      df <- dl_trend_reactive()
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                 panel.background = element_rect(fill = "white", color = NA))
      } else {
        vars <- input$dl_var
        if (length(vars) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No variables selected") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                 panel.background = element_rect(fill = "white", color = NA))
        } else {
          time_period_title <- switch(
            input$dl_time_period,
            "before_2010" = "1950–2010",
            "after_2010"  = paste0("2010–", format(Sys.Date(), "%Y")),
            "all_years"   = paste0("1950–", format(Sys.Date(), "%Y"))
          )
          
          if (length(vars) == 1) {
            var <- vars[1]
            df_filtered <- df %>% dplyr::filter(!is.na(value))
            if (nrow(df_filtered) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = paste("No valid data for", var)) + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                             panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(df_filtered, aes(x = Year, y = value)) +
                geom_line(linewidth = 1.2, color = "#3498db") +
                geom_point(size = 3, color = "#3498db", shape = 19, fill = "white", stroke = 1.5) +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(df_filtered$Year), labels = as.integer(unique(df_filtered$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("", var, " Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, " "),
                  x = "Year",
                  y = var
                )
            }
          } else {
            long_df <- df %>%
              tidyr::pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
              dplyr::filter(!is.na(Value))
            if (nrow(long_df) == 0) {
              p <- ggplot() + annotate("text", x = 1, y = 1, label = "No valid data for selected variables") + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA), 
                                                                                                                                    panel.background = element_rect(fill = "white", color = NA))
            } else {
              p <- ggplot(long_df, aes(x = Year, y = Value, color = Variable, group = Variable)) +
                geom_line(linewidth = 1.2) +
                geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
                scale_color_brewer(palette = "Set1") +
                plot_theme +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
                scale_x_continuous(breaks = unique(long_df$Year), labels = as.integer(unique(long_df$Year))) +
                scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ 
                labs(
                  title = paste0("Selected Variables Over Time, ", time_period_title, " – Diocese of ", input$dl_diocese, ""),
                  x = "Year",
                  y = "Value",
                  color = "Variable"
                )
            }
          }
        }
      }
      ggsave(file, p, device = "jpg", width = 12, height = 8, dpi = 300)
    }
  )
}

# ---- Run app ----
shinyApp(ui = ui, server = server)
