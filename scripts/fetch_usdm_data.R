## Code fetches USDM data for all counties in the U.S.A. 
## Becky Bolinger using Gemini, 2026

# load libraries
library(tidyverse)
library(httr)

# 1. Setup Environment
# Create a data folder to keep your workspace clean
if(!dir.exists("data")) dir.create("data")

sdate <- "1/1/2000"
edate <- "3/29/2026"

print("Fetching USDM data state-by-state. This might take a few minutes...")

# 2. Define the fetching function
# This function takes a single state abbreviation, hits the API, and returns a dataframe
fetch_state_data <- function(state_abbr) {
  url <- paste0(
    "https://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?",
    "aoi=", state_abbr,
    "&startdate=", sdate,
    "&enddate=", edate,
    "&statisticsType=1"
  )
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    raw_text <- content(response, "text", encoding = "UTF-8")
    # Quick check to make sure the API didn't return an empty string
    if(nchar(raw_text) > 10) { 
      return(read_csv(raw_text, col_types = cols(FIPS = "c"), show_col_types = FALSE))
    }
  }
  return(NULL) # Return nothing if the call fails, so it doesn't break the loop
}

# 3. The Magic Loop
# map_dfr() runs function for every state in state.abb, and automatically
# binds the results together into one massive dataframe (Row-binding).
all_usdm_raw <- map_dfr(state.abb, fetch_state_data)

# --- Save the raw data immediately ---
print("Saving raw national data...")
write_rds(all_usdm_raw, "data/national_usdm_raw.rds")
# optional compression of rds file
# write_rds(all_usdm_raw, "data/national_usdm_raw.rds", compress = "gz")
# ------------------------------------------

print("Data retrieved! Crunching the national summary...")

# 4. Wrangle the National Data
national_drought_summary <- all_usdm_raw %>%
  mutate(across(c(D0, D1, D2, D3, D4), as.numeric)) %>%
  # Notice we group by State here as well, so we can filter easily later!
  group_by(FIPS, County, State) %>%
  summarize(
    weeks_D0 = sum(D0 > 0, na.rm = TRUE),
    weeks_D1 = sum(D1 > 0, na.rm = TRUE),
    weeks_D2 = sum(D2 > 0, na.rm = TRUE),
    weeks_D3 = sum(D3 > 0, na.rm = TRUE),
    weeks_D4 = sum(D4 > 0, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Save the data to your local disk as a compressed R object (.rds)
write_rds(national_drought_summary, "data/national_drought_summary.rds")

print("Pipeline complete! Data saved to data/national_drought_summary.rds")