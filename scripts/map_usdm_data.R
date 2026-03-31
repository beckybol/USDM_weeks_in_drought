# load libraries
library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# 1. Load the pre-processed data 
national_drought_summary <- read_rds("data/national_drought_summary.rds")

# ==========================================
# --- ⚙️ CONFIGURE YOUR MAP HERE ⚙️ ---
# ==========================================

target_area <- "US"          # "US" for national, or state abbreviation like "CO", "CA"
target_drought_level <- "D3" # Choose "D0", "D1", "D2", "D3", or "D4"

# Color Scale Toggle
use_continuous_scale <- TRUE # Set to TRUE for a smooth gradient, FALSE for discrete buckets

# Discrete Bucket Settings (Only applies if use_continuous_scale is FALSE)
custom_breaks <- c(-Inf, 0, 10, 20, 30, 40, Inf)
custom_labels <- c("0", "1-10", "10-20", "20-30", "30-40", ">40")
custom_colors <- c(
  "0" = "#F1EEF6",
  "1-10" = "#F9F936",
  "10-20" = "#F9DE36",
  "20-30" = "#F9BB36",
  "30-40" = "#F95C36",
  ">40" = "#9C280B"
)

# ==========================================

d_level <- c("D0", "D1", "D2", "D3", "D4")
d_category <- c("(Abnormally Dry)", "(Moderate Drought)", "(Severe Drought)", "(Extreme Drought)", "(Exceptional Drought)")
target_category <- d_category[d_level == target_drought_level]

print(paste("Preparing data for", target_drought_level, "in", target_area, "..."))

# 2. Setup the Spatial Data
if (target_area == "US") {
  map_sf <- counties(cb = TRUE, class = "sf", progress_bar = FALSE) %>%
    filter(as.numeric(STATEFP) <= 56) %>%
    shift_geometry() %>%
    left_join(national_drought_summary, by = c("GEOID" = "FIPS"))
  
  title_text <- paste("Number of Weeks in", target_drought_level, target_category)
  file_name <- paste0("map_US_", target_drought_level, "_drought.png")
  
} else {
  state_data <- national_drought_summary %>% filter(State == target_area)
  
  map_sf <- counties(state = target_area, cb = TRUE, class = "sf", progress_bar = FALSE) %>%
    left_join(state_data, by = c("GEOID" = "FIPS"))
  
  title_text <- paste(target_area, "Weeks in", target_drought_level, target_category)
  file_name <- paste0("map_", target_area, "_", target_drought_level, "_drought.png")
}

# 3. Dynamically Select the Target Column
target_col <- paste0("weeks_", target_drought_level)

# Create a generic 'target_weeks' column so ggplot doesn't get confused
map_sf <- map_sf %>%
  mutate(target_weeks = .data[[target_col]])

# 4. Build the Map Base
drought_map <- ggplot(data = map_sf) +
  theme_void() +
  labs(
    title = title_text,
    caption = "Data: U.S. Drought Monitor | Graphic: R & ggplot2"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm") # Makes the continuous color bar look nicer!
  )

# 5. Apply the Chosen Color Scale Logic
if (use_continuous_scale) {
  # Add the continuous layers
  drought_map <- drought_map +
    geom_sf(
      data = map_sf %>% mutate(target_weeks = na_if(target_weeks, 0)),
      aes(fill = target_weeks),
      color = "black",
      linewidth = 0.1
    ) +
    # scale_fill_viridis_c is a fantastic, colorblind-friendly continuous palette built into ggplot
    scale_fill_viridis_c(
      option = "inferno", 
      direction = -1, # Flips it so darker = more drought
      name = paste("Weeks in", target_drought_level), 
      na.value = "#F1EEF6"
    )
} else {
  # Apply the custom bucket logic
  map_sf <- map_sf %>%
    mutate(drought_bucket = cut(target_weeks, breaks = custom_breaks, labels = custom_labels))
  
  # Add the discrete layers (Need to override the data source to include our new bucket column)
  drought_map <- drought_map +
    geom_sf(data = map_sf, aes(fill = drought_bucket), color = "black", linewidth = 0.1) +
    scale_fill_manual(
      values = custom_colors, 
      name = paste("Weeks in", target_drought_level), 
      na.value = "#F1EEF6"
    )
}

# 6. Add Projection (if state-level) and Save
if (target_area != "US") {
  drought_map <- drought_map + coord_sf(crs = 5070)
}

ggsave(file_name, plot = drought_map, width = 10, height = 6, dpi = 300)
print(paste("Success! Map saved as", file_name))