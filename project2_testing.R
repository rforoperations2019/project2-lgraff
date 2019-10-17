# Load packages
require(rgdal)
require(leaflet)
require(leaflet.extras)

require(dplyr)
require(readxl)
require(stringr)

require(raptr)
require(PBSmapping)
require(ggplot2)
require(plotly)
library(knitr)

# Load FL eviction data. Source: https://data-downloads.evictionlab.org/
FL_counties <- readOGR("FL_eviction_counties.geojson")
#View(FL_counties@data)
FL_16 <- FL_counties

# Select only 2016 data and rename columns
FL_16@data <- FL_counties@data %>% 
  dplyr::select(GEOID, west, east, north, south, n, pl, ends_with("16")) %>% 
  rename(county = n, state = pl)
#View(FL_16@data)
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
#View(FL_16@data)

# Data types
#sapply(FL_16@data, typeof)
FL_16@data$county <- as.factor(FL_16@data$county)

# add indicator if county has # evictions greater than the mean
FL_16@data <- FL_16@data %>% 
  mutate(above_avg_evic = ifelse(evictions > mean(evictions, na.rm = TRUE),
                                 "Above Average", "Below Average"))

df_FL <- FL_16@data

# Plots
# scatterplot
ggplotly(
  ggplot(data = df_FL, aes(county = county)) +
    geom_point(aes(x = median_gross_rent, y = eviction_rate/100)),
  tooltip = c("county")
) 

test1 <- df_FL %>% 
  select(population, above_avg_evic) %>% 
  filter(!is.na(above_avg_evic))


ggplotly(
  ggplot(test1, aes_string(x = "above_avg_evic", y = "population", fill = "above_avg_evic")) +
    geom_boxplot() + 
    coord_flip() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    guides(fill = guide_legend("Number of Evictions"))
)



g <- ggplot(data = df_FL, aes(county = county)) +
  geom_point(aes(x = median_gross_rent, y = eviction_rate/100))
ggplotly(g, tooltip = c("county")) 

ggplot(data = test) +
  geom_point(aes(x = median_property_val, y = median_income))

max(test$eviction_rate, na.rm = T)  

# histogram
ggplot(data = test) +
  geom_histogram(aes(x = median_gross_rent))

# nicely formatted summary stats
t <- round(data.frame(unclass(summary(df_FL$evic_filing_rate))), 3)
colnames(t) <- c("evic_filing_rate")

t2 <- t(t)

ev <- df_FL[["evictions"]]

df_FL_top <- df_FL %>% 
    arrange(desc(median_income)) %>% 
    top_n(10)
