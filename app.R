#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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

# Data preparation

# Load FL eviction data. Source: https://data-downloads.evictionlab.org/
FL_counties <- readOGR("FL_eviction_counties.geojson")
FL_16 <- FL_counties

# Select only 2016 data and rename columns
FL_16@data <- FL_counties@data %>% 
  dplyr::select(GEOID, west, east, north, south, n, pl, ends_with("16")) %>% 
  rename(county = n, state = pl)
colnames(FL_16@data) <- c("GEOID", "west", "east", "north", "south", "county", "state",
                          "population", "poverty_rate", "renter_occ_households", "pct_renter",
                          "median_gross_rent", "median_income", "median_property_val",
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

FL_16@data$county <- as.factor(FL_16@data$county)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Input 1: county
        selectInput("county", label = h3("Select a county:"), 
                    choices = unique(levels(FL_16@data$county)))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

