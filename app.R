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

df_FL <- FL_16@data

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Florida Housing Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Input 1: county
        selectInput("county", label = "Select a county:", 
                    choices = unique(levels(FL_16@data$county))),
        
        # Input 2: x-variable
        selectInput("var1", label = "Select a variable 1:", 
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
                                )),
        
        # Input 3: y-variable
        selectInput("var2", label = "Select a variable 2:", 
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
                    ))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("scatter")
      )
   )
)

server <- function(input, output) {
  
  output$scatter <- renderPlotly(
    ggplotly(
      ggplot(data = FL_16@data) +
        geom_point(aes_string(x = input$var1, y = input$var2))
    )
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

