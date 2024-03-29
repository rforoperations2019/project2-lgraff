# This research uses data from The Eviction Lab at Princeton University, 
# a project directed by Matthew Desmond and designed by Ashley Gromis, Lavar Edmonds, 
# James Hendrickson, Katie Krywokulski, Lillian Leung, and Adam Porton. The Eviction Lab is
# funded by the JPB, Gates, and Ford Foundations as well as the Chan Zuckerberg Initiative. 
# More information is found at evictionlab.org.


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
library(stringr)
library(tools)

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
FL_16@data$renter_occupied_households <- as.numeric(FL_16@data$renter_occupied_households)
FL_16@data$median_property_value <- as.numeric(FL_16@data$median_property_value)

# Add indicator if county has # evictions greater than the mean, for use in boxplot
FL_16@data <- FL_16@data %>% 
  mutate(above_avg_evic = ifelse(evictions > mean(evictions, na.rm = TRUE),
                                 "Above Average", "Below Average"))

# Rename dataframe for ease of use, and alphabetize
df_FL <- FL_16@data %>% arrange(county)

# Define UI for application
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
                                "Renter Occupied Housholds" = "renter_occupied_households",
                                "Percent Renter" = "pct_renter",
                                "Median Gross Rent" = "median_gross_rent",
                                "Median Income" = "median_income",
                                "Median Property Value" = "median_property_value",
                                "Rent Burden" = "rent_burden",
                                "Eviction Filings" = "eviction_filings",
                                "Evictions" = "evictions",
                                "Eviction Rate" = "eviction_rate"
                                ),
                    selected = "median_property_value"),
        br(),
        
        # Input 3: whether the user wants to include the number of evictions
        checkboxInput("evics", "Include number of evictions on map", TRUE),
        helpText("The size of the red circle on the map scales with the number of evictions")
      ),
      
      # Organize output into four tabs
      mainPanel(
        tabsetPanel(type= "tabs",
          tabPanel("County-level Map",
                   br(),
                   p("The map of Florida below is delineated by county. It will change
                     into a heatmap determined by five percentile groups of the variable you 
                     selected."),
                   p("You can also choose to display a red circle 
                     that scales with the number of evictions for each county. If you click 
                     the circle, you will see a popup that shows the number."),
                   p("When you select a county, it will appear highlighted in yellow."),
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
                   downloadLink("downloadData", "Download"),
                   br(),
                   br(),
                   p("Below is the data table used for the map and plots,
                     alphabetized by county"),
                   br(),
                   DT::dataTableOutput("dt")
          )
        )
      )
   )
)

server <- function(input, output) {
  
  # Plot of eviction rate vs selected user input
  # User can hover over each point to see the associated county
  output$scatter <- renderPlotly(
    ggplotly(
      ggplot(data = df_FL, aes(county = county)) +
        geom_point(aes_string(x = input$var1, y = "eviction_rate"), color = "blue") +
        ggtitle(paste("Eviction rate vs", toTitleCase(str_replace_all(input$var1, "_", " ")))) +
        labs(x = toTitleCase(str_replace_all(input$var1, "_", " ")),
             y = "Eviction Rate"),
      tooltip = c("county", input$var1, "eviction_rate")
    )
  )
  
  # Correlation between eviction rate and selected input 
  output$corr <- renderText({
    correlation <- as.character(round(cor(x = df_FL$eviction_rate, y = df_FL[[input$var1]], 
                                          use = "complete.obs"), 2))
    paste("The correlation between the eviction rate and ", str_replace_all(input$var1, "_", " "),
          "is", correlation)
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
        guides(fill = guide_legend("Number of Evictions"))
    )
  )
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("FloridaEvictions.csv")
    },
    content = function(file) {
      write.csv(df_FL, file)
    }
  )
  
  # Render Data table. Do not include geo identifying columns
  output$dt <- DT::renderDataTable(
    DT::datatable(data = df_FL[,c(6:27,34)],
                  options = list(scrollX = TRUE),
                  class = 'white-space: nowrap')
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
    colorQuantile("Blues", domain = df_FL[[input$var1]],
                  n = 5, na.color = "white")
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
      addLegend(title = paste(str_replace_all(input$var1, "_", " "), "percentile"),
                pal = color_pal,
                values = df_FL[[input$var1]], opacity = 1,
                position = c("bottomleft")) %>% 
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

