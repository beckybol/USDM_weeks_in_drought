## Code taken from 2019 script by Becky Bolinger and updated
## new mapping using ggplot2 and more efficient calculations by eliminating for loops.
## Primarily used as initial test for setting up R in Codespaces.

# load libraries
library(tidyverse)
library(httr)
library(sf)
library(tigris)

# 1. Define Parameters
state_abbr <- "CO"
sdate <- "1/1/2000"
edate <- "3/29/2026"

# 2. Construct the single API URL for all counties in the state
url <- paste0(
  "https://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?",
  "aoi=", state_abbr,
  "&startdate=", sdate,
  "&enddate=", edate,
  "&statisticsType=1"
)

print(paste("Fetching USDM data for all counties in", state_abbr, "..."))

# 3. Fetch and parse the data in one shot
response <- GET(url)

if (status_code(response) == 200) {
  raw_text <- content(response, "text", encoding = "UTF-8")
  usdm_raw_data <- read_csv(raw_text, show_col_types = FALSE)

  print("Data successfully retrieved! Now crunching the numbers...")
} else {
  stop(paste("Failed to fetch data. Status code:", status_code(response)))
}

# 4. Wrangle the Data
# The API returns FIPS, County, State, None, D0, D1, D2, D3, D4, ValidStart, ValidEnd
county_drought_summary <- usdm_raw_data %>%
  # Convert severity columns to numeric
  mutate(across(c(D0, D1, D2, D3, D4), as.numeric)) %>%
  # Group by both FIPS and County name so we keep the names in our final table
  group_by(FIPS, County) %>%
  # Count how many weeks the value was greater than 0
  summarize(
    weeks_D0 = sum(D0 > 0, na.rm = TRUE),
    weeks_D1 = sum(D1 > 0, na.rm = TRUE),
    weeks_D2 = sum(D2 > 0, na.rm = TRUE),
    weeks_D3 = sum(D3 > 0, na.rm = TRUE),
    weeks_D4 = sum(D4 > 0, na.rm = TRUE),
    .groups = "drop" # Drops the grouping structure afterward to keep the dataframe clean
  )

print("Data processing complete!")
head(county_drought_summary)

# --- NEW SPATIAL MAPPING BLOCK ---

# Good practice: tell tigris to cache the shapefile so it doesn't download every time
options(tigris_use_cache = TRUE)

print("Downloading Colorado county shapefiles...")

# 1. Fetch the shapefile for Colorado counties
co_counties_sf <- counties(state = "CO", cb = TRUE, class = "sf", progress_bar = FALSE)

# 2. Join the spatial data with our drought summary data
# The Census uses "GEOID" for FIPS codes, and our USDM data uses "FIPS"
co_map_data <- co_counties_sf %>%
  left_join(county_drought_summary, by = c("GEOID" = "FIPS")) %>%
  # Create the color buckets just like your 2019 script
  mutate(
    D4_bucket = cut(
      weeks_D4, 
      breaks = c(-Inf, 0, 10, 20, 30, 40, Inf),
      labels = c("0", "1-10", "10-20", "20-30", "30-40", ">40")
    )
  )

# 3. Define your custom 2019 color palette
custom_colors <- c(
  "0" = "#F1EEF6",      # Light grey for zero weeks
  "1-10" = "#F9F936",   # Yellow
  "10-20" = "#F9DE36",  # Darker Yellow
  "20-30" = "#F9BB36",  # Orange
  "30-40" = "#F95C36",  # Red-Orange
  ">40" = "#9C280B"     # Dark Red
)

print("Generating map...")

# 4. Draw the map with ggplot2
drought_map <- ggplot(data = co_map_data) +
  # geom_sf is the magic function that automatically maps the polygon boundaries
  geom_sf(aes(fill = D4_bucket), color = "black", linewidth = 0.2) +
  scale_fill_manual(values = custom_colors, name = "Weeks in D4") +
  # theme_void() removes the longitude/latitude grid lines and grey background
  theme_void() +
  labs(
    title = "Number of Weeks in D4 (Exceptional Drought)",
    subtitle = paste("Colorado Data from USDM:", sdate, "to", edate),
    caption = "Data: U.S. Drought Monitor | Graphic: R & ggplot2"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    plot.margin = margin(15, 15, 15, 15)
  )

# 5. Save the output to a file
ggsave("co_d4_drought_map.png", plot = drought_map, width = 8, height = 6, dpi = 300)
print("Map saved to workspace as 'co_d4_drought_map.png'!")