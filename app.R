# This research uses data from The Eviction Lab at Princeton University, a project directed by Matthew Desmond and designed by Ashley Gromis, Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung, and Adam Porton. The Eviction Lab is funded by the JPB, Gates, and Ford Foundations as well as the Chan Zuckerberg Initiative. More information is found at evictionlab.org.
#

library(shiny)

# Load packages
require(rgdal)
require(leaflet)
require(leaflet.extras)

require(dplyr)
require(readxl)
require(stringr)

require(raptr)
require(PBSmapping)
library(ggplot2)
library(plotly)

# Data preparation

# Load FL eviction data. Source: https://data-downloads.evictionlab.org/
FL_counties <- readOGR("FL_eviction_counties.geojson")
FL_16 <- FL_counties

# Select only 2016 data and rename columns
FL_16@data <- FL_counties@data %>% 
  dplyr::select(GEOID, west, east, north, south, n, pl, ends_with("16")) %>% 
  rename(county = n, state = pl)
colnames(FL_16@data) <- c("GEOID", "west", "east", "north", "south", "county", "state",
                          "population", "poverty_rate", "renter_occupied_households", "pct_renter",
                          "median_gross_rent", "median_income", "median_property_value",
                          "rent_burden", "pct_white", "pct_af_am", "pct_hispanic", "pct_am_ind",
                          "pct_asian", "pct_nh", "pct_multiple", "pct_other", 
                          "eviction_filings", "evictions", "eviction_rate", "evic_filing_rate",
                          "low_flag", "imputed", "sub")

# Add lat and long to the dataset
# Calculate centroids of counties to find lat and long
FL_polyset <- SpatialPolygons2PolySet(x = FL_16)
centroid <- calcCentroid(FL_polyset, rollup = 1) %>% 
  rename(lat = Y, long = X)

# Bind county centroids to Florida data
FL_16@data <- cbind(FL_16@data, centroid)

# Change rates into true rates by dividing by 100; also change some column types
FL_16@data$county <- as.factor(FL_16@data$county)
FL_16@data$eviction_rate <- round(FL_16@data$eviction_rate/100, 4)
FL_16@data$poverty_rate <- round(FL_16@data$poverty_rate/100, 4)
FL_16@data$evic_filing_rate <- round(FL_16@data$evic_filing_rate/100, 4)
FL_16@data$renter_occ_households <- as.numeric(FL_16@data$median_property_val)
FL_16@data$median_property_val <- as.numeric(FL_16@data$median_property_val)

# Add indicator if county has # evictions greater than the mean, for use in boxplot
FL_16@data <- FL_16@data %>% 
  mutate(above_avg_evic = ifelse(evictions > mean(evictions, na.rm = TRUE),
                                 "Above Average", "Below Average"))

# Rename dataframe for ease of use, and alphabetize
df_FL <- FL_16@data %>% arrange(county)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Florida Eviction and Housing Data"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(

        # Input 1: county
        selectInput("county", label = "Select a county",
                    choices = unique(df_FL$county)),
        br(),
    
        # Input 2: variable of interest
        helpText("The variable of interest will be used for the heatmap, scatterplot,
                 and summary statistics"),
        selectInput("var1", label = "Select a variable of interest:", 
                    choices = c("Population" = "population",
                                "Poverty Rate" = "poverty_rate",
                                "Renter Occupied Housholds" = "renter_occ_households",
                                "Percent Renter" = "pct_renter",
                                "Median Gross Rent" = "median_gross_rent",
                                "Median Income" = "median_income",
                                "Median Property Value" = "median_property_val",
                                "Rent Burden" = "rent_burden",
                                "Eviction Filings" = "eviction_filings",
                                "Evictions" = "evictions",
                                "Eviction Rate" = "eviction_rate"
                                ),
                    selected = "Median Property Value"),
        br(),
        
        # Input 3: whether the user wants to include the number of evictions
        checkboxInput("evics", "Include number of evictions on map", TRUE),
        helpText("The size of the red circle on the map scales with the number of evictions")
      ),
      
      # Organize output into four tabs
      mainPanel(
        tabsetPanel(type= "tabs",
          tabPanel("County-level Map",
                   p("MAP DIRECTIONS"),
                   leafletOutput("map")),
          tabPanel("Exploratory Analysis",
                   br(),
                   p("Hover over each point to see the numbers along with the
                     associated county"),
                   plotlyOutput("scatter"),
                   DT::dataTableOutput("evic_rate"),
                   br(),
                   h4("Correlation"),
                   textOutput("corr")),
          tabPanel("Summary Statistics",
                   br(),
                   p("Compare two boxplots of the selected input, separated by counties with 
                     above average vs. below average number of evictions"),
                   plotlyOutput("box")),
          tabPanel("Raw Data",
                   br(),
                   p("To download the dataset, click the button below"),                   
                   p("Below is the data table used for the map and plots"),
                   DT::dataTableOutput("dt")
          )
        )
      )
   )
)

server <- function(input, output) {
  
  # df_FL_top <- reactive({
  #   df_FL %>% 
  #     arrange(desc(input$var1)) %>% 
  #     top_n(input$topN)
  # })
  
  # Plot of eviction rate vs selected user input
  # User can hover over each point to see the associated county
  output$scatter <- renderPlotly(
    ggplotly(
      ggplot(data = df_FL, aes(county = county)) +
        geom_point(aes_string(x = input$var1, y = "eviction_rate")),
      tooltip = c("county", input$var1, "eviction_rate")
    )
  )
  
  # Correlation between number of evictions and selected input 
  output$corr <- renderText({
    correlation <- as.character(round(cor(x = df_FL$evictions, y = df_FL[[input$var1]], 
                                          use = "complete.obs"), 2))
    paste("The correlation between the number of evictions and ", input$var1, "is", correlation)
  })
 
  # Boxplot of selected user input, fill by above vs. below average number of evictions
  df_boxinput <- reactive({
    df_FL %>% 
      select(input$var1, above_avg_evic) %>% 
      filter(!is.na(above_avg_evic))
  })

  output$box <- renderPlotly(
    ggplotly(
      ggplot(data = df_boxinput(), aes_string(x = "above_avg_evic", 
                                              y = input$var1, fill = "above_avg_evic")) +
        geom_boxplot() + 
        coord_flip() +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        ggtitle(paste("Number of Evictions vs"), input$var1) +
        guides(fill = guide_legend("Number of Evictions"))
    )
  )
  
  # Data table for the data used 
  output$dt <- DT::renderDataTable(
    DT::datatable(data = df_FL[,c(6:36)],
                  options = list(scrollX = TRUE))
  )
  
  
  # Map Instructions
  # Create base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Open Street") %>% 
      fitBounds(lng1 = min(df_FL$long), lat1 = min(df_FL$lat),
                lng2 = max(df_FL$long), lat2 = max(df_FL$lat))
  })
  
  # Highlight the selected county
  countySelect <- reactive({
    cnty <- subset(FL_16, county == input$county)
    
    return(cnty)
  })
  
  # Create a reactive palette for the selected input
  qpal <- reactive({
    colorQuantile("Blues", df_FL[[input$var1]], n = 5, na.color = "white")
  })

  # Redraw map, colored by selected variable, highlighted by county
  observe({
    color_pal <- qpal()
    county <- countySelect()

    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      clearMarkers() %>% 
      addPolygons(data = FL_16,
                  weight = 1,
                  smoothFactor = .2,
                  fillOpacity = .8,
                  fillColor = ~color_pal(df_FL[[input$var1]])) %>%
      addLegend(title = input$var1, pal = color_pal, values = df_FL[[input$var1]],
                opacity = 1) %>%
      addPolygons(data = county, color = "yellow", weight = 5, stroke = TRUE)
    
    if (input$evics == TRUE) {
      leafletProxy("map") %>%
        addCircleMarkers(data = df_FL, lat = ~lat, lng = ~long,
                         radius = ~log(evictions),
                         popup = ~paste(county, "<br>",
                                        "Number of evictions:", as.character(evictions)),
                         color = "red")
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

