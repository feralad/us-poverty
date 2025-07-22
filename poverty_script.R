# ──────────────────────────────────────────────────────────────────────────────
# SET UP ENVIRONMENT -----------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Specify needed packages
packages <- c("tidyverse", "tidycensus", "tigris", "sf", "scales", "sfarrow", "arrow")

# Identify which packages are already installed
installed_packages <- rownames(installed.packages())

# Identify which packages still need to be installed
to_install <- setdiff(packages, installed_packages)

# Install any missing packages
if(length(to_install)) install.packages(to_install)

# Load all required packages
lapply(packages, library, character.only = TRUE)

# Run `options(tigris_use_cache = TRUE)` to cache shapefiles for use in future sessions

# ──────────────────────────────────────────────────────────────────────────────
# GET DATA ---------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Run `tidycensus_variables <- load_variables(2023, "acs5")` to help determine needed variables
## This creates searchable data frame
## acs5 is the 5-year American Community Survey

# Define needed variables and create vector for easier reuse
## Population is defined as "population for whom poverty status is determined"
needed_variables <- c("all; All; 1; population; B17001_001" = "B17001_001", 
                      "all; All; 1; population_below_poverty_level; B17001_002" = "B17001_002",
                      "white_alone; White Alone; 9; population; B17001A_001" = "B17001A_001", 
                      "white_alone; White Alone; 9; population_below_poverty_level; B17001A_002" = "B17001A_002",
                      "black; Black or African American Alone; 4; population; B17001B_001" = "B17001B_001", 
                      "black; Black or African American Alone; 4; population_below_poverty_level; B17001B_002" = "B17001B_002", 
                      "aian; American Indian and Alaska Native Alone; 2; population; B17001C_001" = "B17001C_001", 
                      "aian; American Indian and Alaska Native Alone; 2; population_below_poverty_level; B17001C_002" = "B17001C_002", 
                      "asian; Asian Alone; 3; population; B17001D_001" = "B17001D_001", 
                      "asian; Asian Alone; 3; population_below_poverty_level; B17001D_002" = "B17001D_002", 
                      "nhpi; Native Hawaiian and Other Pacific Islander Alone; 6; population; B17001E_001" = "B17001E_001", 
                      "nhpi; Native Hawaiian and Other Pacific Islander Alone; 6; population_below_poverty_level; B17001E_002" = "B17001E_002",
                      "other; Some Other Race Alone; 7; population; B17001F_001" = "B17001F_001", 
                      "other; Some Other Race Alone; 7; population_below_poverty_level; B17001F_002" = "B17001F_002", 
                      "two_or_more; Two or More Races; 8; population; B17001G_001" = "B17001G_001", 
                      "two_or_more; Two or More Races; 8; population_below_poverty_level; B17001G_002" = "B17001G_002", 
                      "wanhl; White Alone, Not Hispanic or Latino; 10; population; B17001H_001" = "B17001H_001", 
                      "wanhl; White Alone, Not Hispanic or Latino; 10; population_below_poverty_level; B17001H_002" = "B17001H_002", 
                      "hispanic_latino; Hispanic or Latino; 5; population; B17001I_001" = "B17001I_001", 
                      "hispanic_latino; Hispanic or Latino; 5; population_below_poverty_level; B17001I_002" = "B17001I_002")

# Create function to get poverty data for unique geographic levels using needed variables
## get_acs is function from tidycensus package
fn_get_poverty_data <- function(needed_geography, needed_variables) {
  get_acs(
    geography = needed_geography,
    variables = needed_variables,
    summary_var = "B17001_001",
    geometry = TRUE,
    output = "tidy",
    year = 2023,
    moe_level = 90,
    survey = "acs5"
  )
}

# Run the function for each desired geographic level
us_poverty_data <- fn_get_poverty_data("us", needed_variables)
state_poverty_data <- fn_get_poverty_data("state", needed_variables)
county_poverty_data <- fn_get_poverty_data("county", needed_variables)

# ──────────────────────────────────────────────────────────────────────────────
# WRANGLE DATA -----------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Join all data frames into one (stack rows)
poverty_data <- bind_rows(
  # Add geographic level column
  us_poverty_data %>% mutate(geo_level = "us"),
  state_poverty_data %>% mutate(geo_level = "state_territory"),
  county_poverty_data %>% mutate(geo_level = "county")
  ) %>%
  # Split variable information into separate columns
  separate_wider_delim(variable, 
                       delim = "; ", 
                       names = c("race_ethnicity_short", "race_ethnicity_full", "sort_order_race_ethnicity", "variable", "var_id")
                       ) %>%
  # Coerce sort_order_race_ethnicity to numeric type and add country name column for future use
  mutate(sort_order_race_ethnicity = as.numeric(sort_order_race_ethnicity),
         country_n = "United States") %>%
  # Relocate columns for better readability
  relocate(country_n, .after = NAME) %>%
  relocate(geo_level, .before = GEOID) %>%
  relocate(geometry, .after = GEOID)

# Nest data frame into multiple data frames by race/ethnicity
nested_poverty_data <- poverty_data %>%
  # Rename columns for clarity
  rename(E = estimate, M = moe, summary_all_population_E = summary_est,
         summary_all_population_M = summary_moe) %>%
  group_by(race_ethnicity_short) %>%
  nest() %>%
  # Pivot wider to ensure unique geographic area is only one row and create appropriate columns
  mutate(
    data = map(data, ~ .x %>%
                 pivot_wider(
                   names_from = variable,
                   values_from = c(var_id, E, M),
                   names_glue = "{variable}_{.value}", 
                   names_vary = "slowest"
                 )
    )
  )
  
# ──────────────────────────────────────────────────────────────────────────────
# CUSTOM FUNCTIONS -------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Use {{x}} instead of x to enable function use with dplyr verbs

# Categorize percent population below poverty level E into groups
## NA happens when population estimate and population below poverty level estimate both = 0
fn_group_percent_population_below_poverty_level_E <- function(x) {
  case_when(is.na({{x}}) ~ "Zero Population Estimate", 
            {{x}} >= .40 ~ "Extreme", 
            {{x}} >= .30 ~ "Very High", 
            {{x}} >= .20 ~ "High", 
            {{x}} >= .10 ~ "Moderate",
            {{x}} < .10 ~ "Low")
}

# Apply value/sort order to percent population below poverty level E group
fn_sort_group_percent_population_below_poverty_level_E <- function(x) {
  case_when({{x}} == "Extreme" ~ 1, 
            {{x}} == "Very High" ~ 2,
            {{x}} == "High" ~ 3,
            {{x}} == "Moderate" ~ 4, 
            {{x}} == "Low" ~ 5, 
            {{x}} == "Zero Population Estimate" ~ 6)
}

# Categorize percent population below poverty level M into groups
fn_group_percent_population_below_poverty_level_M <- function(x) {
  case_when(is.na({{x}}) ~ "Zero Population Estimate", 
            {{x}} > .10 ~ "More than 10%", 
            {{x}} >= .05 ~ "5 to 10%", 
            {{x}} < .05 ~ "Less than 5%")
}

# Apply value/sort order to percent population below poverty level M group
fn_sort_group_percent_population_below_poverty_level_M <- function(x) {
  case_when({{x}} == "Less than 5%" ~ 1, 
            {{x}} == "5 to 10%" ~ 2,
            {{x}} == "More than 10%" ~ 3,
            {{x}} == "Zero Population Estimate" ~ 4)
}

# Apply value/sort order to reliability percent population below poverty level E/M
fn_sort_reliability_percent_population_below_poverty_level_E <- function(x) {
  case_when({{x}} == "Reliable" ~ 1, 
            {{x}} == "Unreliable" ~ 2,
            {{x}} == "Zero Population Estimate" ~ 3)
}

# Categorize population or population below poverty level E relative to mean
fn_group_relative_population_size <- function(x) {
  case_when(
    {{x}} > 50 ~ "> 50x mean",
    {{x}} > 25 ~ "> 25x mean",
    {{x}} > 10 ~ "> 10x mean",
    {{x}} > 5 ~ "> 5x mean",
    {{x}} > 2 ~ "> 2x mean",
    {{x}} > 1.25 ~ "> 1.25x mean",
    {{x}} >= 0.75 ~ "~ Equal to mean",
    {{x}} >= 0.5 ~ "< 3/4x mean",
    {{x}} >= 0.25 ~ "< 1/2x mean",
    {{x}} >= 0.125 ~ "< 1/4x mean",
    {{x}} < 0.125 ~ "< 1/8x mean"
  )
}

# Apply value/sort order to relative population size group
fn_sort_group_relative_population_size <- function(x) {
  case_when(
    {{x}} == "> 50x mean" ~ 1,
    {{x}} == "> 25x mean" ~ 2,
    {{x}} == "> 10x mean" ~ 3,
    {{x}} == "> 5x mean" ~ 4,
    {{x}} == "> 2x mean" ~ 5,
    {{x}} == "> 1.25x mean" ~ 6,
    {{x}} == "~ Equal to mean" ~ 7,
    {{x}} == "< 3/4x mean" ~ 8,
    {{x}} == "< 1/2x mean" ~ 9,
    {{x}} == "< 1/4x mean" ~ 10,
    {{x}} == "< 1/8x mean" ~ 11
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# CALCULATED FIELDS (ALL) ------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Add new "calculated" columns and round decimal values where needed
nested_poverty_data <- nested_poverty_data %>% 
  mutate(
    data = map(data, ~.x %>%
                 mutate(
                   # Calculate coefficient of variation for population below poverty (for reference)
                   ## Margin of error divided by 1.645 (for 90% confidence level) divided by estimate
                   population_below_poverty_level_CV = 
                     (population_below_poverty_level_M / 1.645) / 
                         population_below_poverty_level_E,
                   # Calculate upper and lower bounds of population below poverty
                   upper_bound_population_below_poverty_level_E = 
                     population_below_poverty_level_E + 
                        population_below_poverty_level_M,
                   lower_bound_population_below_poverty_level_E = 
                     population_below_poverty_level_E - 
                        population_below_poverty_level_M,
                   # Upper bound cannot be greater than total population
                   upper_bound_population_below_poverty_level_E = 
                     if_else(upper_bound_population_below_poverty_level_E > population_E,
                             population_E, upper_bound_population_below_poverty_level_E),
                   # Lower bound cannot be less than 0
                   lower_bound_population_below_poverty_level_E = 
                     if_else(lower_bound_population_below_poverty_level_E < 0, 
                             0, lower_bound_population_below_poverty_level_E),
                   # Calculate percent of population and margin of error
                   percent_population_E =
                     population_E / summary_all_population_E,
                   percent_population_M =
                     # moe_prop function from tidycensus package
                     moe_prop(
                       population_E,
                       summary_all_population_E,
                       population_M,
                       summary_all_population_M),
                   # Calculate percent of population below poverty level and margin of error
                   percent_population_below_poverty_level_E = 
                     population_below_poverty_level_E / population_E, 
                   percent_population_below_poverty_level_M = 
                     moe_prop(
                       population_below_poverty_level_E,
                       population_E,
                       population_below_poverty_level_M,
                       population_M),
                   # Calculate coefficient of variation for percent of population below poverty
                   percent_population_below_poverty_level_CV =
                     (percent_population_below_poverty_level_M / 1.645) / 
                         percent_population_below_poverty_level_E,
                   # Calculate upper and lower bounds of percent of population below poverty
                   upper_bound_percent_population_below_poverty_level_E = 
                     percent_population_below_poverty_level_E + 
                        percent_population_below_poverty_level_M,
                   lower_bound_percent_population_below_poverty_level_E = 
                     percent_population_below_poverty_level_E - 
                        percent_population_below_poverty_level_M,
                   # Upper bound percentage cannot be greater than 100%
                   upper_bound_percent_population_below_poverty_level_E = 
                     if_else(upper_bound_percent_population_below_poverty_level_E > 1,
                             1, upper_bound_percent_population_below_poverty_level_E),
                   # Lower bound percentage cannot be less than 0%
                   lower_bound_percent_population_below_poverty_level_E = 
                     if_else(lower_bound_percent_population_below_poverty_level_E < 0,
                             0, lower_bound_percent_population_below_poverty_level_E),
                   # Round to three decimal places when column name meets conditions
                   across(where(is.numeric) & contains(c("percent", "CV")),
                          ~ round(., 3)),
                   # Apply group categorization functions to percent population below poverty values (E, upper_E, lower_E)
                   across(contains("percent_population_below_poverty_level_E"),
                          ~ fn_group_percent_population_below_poverty_level_E(.x),
                          .names = "group_{col}"),
                   group_percent_population_below_poverty_level_M =
                     fn_group_percent_population_below_poverty_level_M(percent_population_below_poverty_level_M),
                   # Apply sort functions to group values
                   across(starts_with("group") & ends_with("E"),
                          ~ fn_sort_group_percent_population_below_poverty_level_E(.x),
                          .names = "sort_order_{col}"),
                   sort_order_group_percent_population_below_poverty_level_M = 
                     fn_sort_group_percent_population_below_poverty_level_M(group_percent_population_below_poverty_level_M),
                   # Create measure to determine if upper and lower bound percent population below poverty is in same category
                   range_volatility_group_combined_bounds_percent_population_below_poverty_level_E = 
                     abs(sort_order_group_upper_bound_percent_population_below_poverty_level_E - 
                            sort_order_group_lower_bound_percent_population_below_poverty_level_E),
                   # Create measure to determine reliability of percent population below poverty estimate
                   reliability_percent_population_below_poverty_level_E = 
                     case_when(
                       group_percent_population_below_poverty_level_M == "Zero Population Estimate" ~ "Zero Population Estimate",
                       percent_population_below_poverty_level_M <= 0.1 |
                         percent_population_below_poverty_level_CV <= 0.25 |
                         range_volatility_group_combined_bounds_percent_population_below_poverty_level_E == 0 ~ "Reliable",
                       percent_population_below_poverty_level_M > 0.1 & 
                         percent_population_below_poverty_level_CV > 0.25 & 
                         range_volatility_group_combined_bounds_percent_population_below_poverty_level_E > 0 ~ "Unreliable"),
                   # Apply reliability sort function
                   sort_order_reliability_percent_population_below_poverty_level_E = 
                     fn_sort_reliability_percent_population_below_poverty_level_E(reliability_percent_population_below_poverty_level_E),
                 ) %>%
                 # Relocate columns
                 relocate(percent_population_E:percent_population_M, 
                          .after = population_M
                          ) %>% 
                 # Shorten column names (in case needed for shapefile)
                 rename(race_eth_f = race_ethnicity_full,
                        sore = sort_order_race_ethnicity,
                        sapE = summary_all_population_E,                                                       
                        sapM = summary_all_population_M,
                        pID = population_var_id,
                        pE = population_E,                                                                   
                        pM = population_M,                                                                   
                        ppE = percent_population_E,                                                           
                        ppM = percent_population_M,
                        pbpID = population_below_poverty_level_var_id,
                        pbpE = population_below_poverty_level_E,                                               
                        pbpM = population_below_poverty_level_M,
                        pbpCV = population_below_poverty_level_CV,
                        upbpE = upper_bound_population_below_poverty_level_E,                                   
                        lpbpE = lower_bound_population_below_poverty_level_E,                                   
                        ppbpE = percent_population_below_poverty_level_E,                                       
                        ppbpM = percent_population_below_poverty_level_M,
                        ppbpCV = percent_population_below_poverty_level_CV,
                        uppbpE = upper_bound_percent_population_below_poverty_level_E,                           
                        lppbpE = lower_bound_percent_population_below_poverty_level_E,                           
                        gppbpE = group_percent_population_below_poverty_level_E,                                 
                        guppbpE = group_upper_bound_percent_population_below_poverty_level_E,                     
                        glppbpE = group_lower_bound_percent_population_below_poverty_level_E,
                        gppbpM = group_percent_population_below_poverty_level_M,
                        sogppbpE = sort_order_group_percent_population_below_poverty_level_E,                      
                        soguppbpE = sort_order_group_upper_bound_percent_population_below_poverty_level_E,          
                        soglppbpE = sort_order_group_lower_bound_percent_population_below_poverty_level_E,
                        sogppbpM = sort_order_group_percent_population_below_poverty_level_M,
                        rvgcppbpE = range_volatility_group_combined_bounds_percent_population_below_poverty_level_E,
                        rppbpE = reliability_percent_population_below_poverty_level_E,
                        sorppbpE = sort_order_reliability_percent_population_below_poverty_level_E
                 )
               )
  ) %>%
  # Rename additional column separately since it is not nested within data column
  rename(race_eth_s = race_ethnicity_short)

# ──────────────────────────────────────────────────────────────────────────────
# FINAL U.S. DATA --------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

us_poverty_data <- nested_poverty_data %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  filter(geo_level == "us") %>%
  # Geometry column is not needed
  select(-geometry) %>%
  relocate(race_eth_s, .before = race_eth_f) %>%
  arrange(sore)

# ──────────────────────────────────────────────────────────────────────────────
# CALCULATED FIELDS (STATE & COUNTY ONLY) --------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Calculate relative population sizes (for states and counties) at national level
state_county_nested_poverty_data <- nested_poverty_data %>%
  unnest(cols = c(data)) %>% 
  ungroup() %>%
  filter(geo_level != "us") %>%
  # Group by geographic level and race/ethnicity
  group_by(geo_level, race_eth_s) %>%
  nest() %>%
  # Calculate national relative population sizes for each race/ethnicity
  ## States are compared to all other states
  ## Counties are compared to all other counties
  mutate(data = 
           map(data, ~.x %>%
                 # Calculate population/population below poverty E relative to the mean
                 ## n = national, s = size, m = mean
                 mutate(nsmpE = pE / mean(pE),
                        nsmpbpE = pbpE / mean(pbpE),
                        # Round to appropriate decimal places
                        across(where(is.numeric) & starts_with("nsm"), ~ round(., digits = 2)),
                        # Categorize relative population sizes using custom function
                        across(where(is.numeric) & starts_with("nsm"),
                               ~ fn_group_relative_population_size(.x),
                               .names = "g{col}"),
                        # Apply sort function to relative population size groups
                        across(starts_with("gnsm"),
                               ~ fn_sort_group_relative_population_size(.x),
                               .names = "so{col}")
                 )
           )
  )

# ──────────────────────────────────────────────────────────────────────────────
# FINAL STATE DATA -------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

state_poverty_data <- state_county_nested_poverty_data %>%
  unnest(cols = c(data)) %>% 
  ungroup() %>%
  filter(geo_level == "state_territory") %>%
  # Geometry column is not needed
  select(-geometry) %>%
  mutate(state_n = NAME) %>%
  relocate(race_eth_s, .before = race_eth_f) %>%
  relocate(state_n, .after = NAME) %>%
  arrange(GEOID, sore)

# ──────────────────────────────────────────────────────────────────────────────
# CALCULATED FIELDS (COUNTY ONLY) ----------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Calculate relative population sizes (for counties) at state level
county_nested_poverty_data <- state_county_nested_poverty_data %>%
  unnest(cols = c(data)) %>% 
  ungroup() %>%
  filter(geo_level == "county") %>%
  rename(county_f = NAME) %>%
  # Create county short and state name columns
  separate_wider_delim(county_f, 
                       delim = ", ", 
                       names = c("county_s", "state_n"),
                       cols_remove = FALSE) %>%
  mutate(NAME = county_s) %>%
  # Group by race/ethnicity and state
  group_by(race_eth_s, state_n) %>%
  nest() %>%
  mutate(data = 
           map(data, ~.x %>%
                 # Calculate population/population below poverty E relative to the mean
                 ## s = state replaces n = national
                 mutate(ssmpE = pE / mean(pE),
                        ssmpbpE = pbpE / mean(pbpE),
                        # Round to appropriate decimal places
                        across(where(is.numeric) & starts_with("ssm"), ~ round(., digits = 2)),
                        # Categorize relative population sizes using custom function
                        across(where(is.numeric) & starts_with("ssm"),
                               ~ fn_group_relative_population_size(.x),
                               .names = "g{col}"),
                        # Apply sort function to relative population size groups
                        across(starts_with("gssm"),
                               ~ fn_sort_group_relative_population_size(.x),
                               .names = "so{col}")
                 )
           )
  )

# ──────────────────────────────────────────────────────────────────────────────
# FINAL COUNTY DATA ------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

county_poverty_data <- county_nested_poverty_data %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  relocate(race_eth_s, .before = race_eth_f) %>%
  relocate(NAME, .before = county_s) %>%
  relocate(state_n, .before = country_n) %>%
  arrange(GEOID, sore)

# Add formatted/tooltip columns
county_poverty_data <- county_poverty_data %>%
  mutate(
    pE_fmt      = comma(pE),
    pbpE_fmt    = comma(pbpE),
    pbpM_fmt    = comma(pbpM),
    upbpE_fmt   = comma(upbpE),
    lpbpE_fmt   = comma(lpbpE),
    ppbpE_fmt   = percent(ppbpE, accuracy = 0.2),
    ppbpM_fmt   = percent(ppbpM, accuracy = 0.2),
    uppbpE_fmt  = percent(uppbpE, accuracy = 0.2),
    lppbpE_fmt  = percent(lppbpE, accuracy = 0.2),
    ptt = paste0(
      "<strong>County:</strong> ", county_s, "<br>",
      "<strong>State:</strong> ", state_n, "<br>",
      "<strong>Race/Ethnicity:</strong> ", race_eth_s, "<br>",
      "<strong>Population:</strong> ", pE_fmt, "<br>",
      "<strong>Population in Poverty:</strong> ", pbpE_fmt, "<br>",
      "<strong>% Population in Poverty:</strong> ", ppbpE_fmt, "<br>",
      "<strong>Margin of Error (%):</strong> ", ppbpM_fmt, "<br>",
      "<strong>Poverty Rate:</strong> ", gppbpE, "<br>",
      "<strong>Reliability:</strong> ", rppbpE
    )
  )

# Convert county_poverty_data to sf object to ensure geometry is sufficiently preserved
county_poverty_data <- st_as_sf(county_poverty_data, crs = 4269) %>%
  st_transform(4326) # Use 4326 CRS (for web mapping)

# ──────────────────────────────────────────────────────────────────────────────
# SAVE DATASETS ----------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Save as Parquet
st_write_parquet(county_poverty_data, "final_data/county_poverty_data.parquet")
# No geometry data for US and state data, so normal parquet is sufficient
write_parquet(us_poverty_data, "final_data/us_poverty_data.parquet")
write_parquet(state_poverty_data, "final_data/state_poverty_data.parquet")

# Save as CSV (may be useful for some users/applications)
## Drop geometry from county_poverty_data
write_csv(st_drop_geometry(county_poverty_data), "final_data/county_poverty_data.csv")
write_csv(us_poverty_data, "final_data/us_poverty_data.csv")
write_csv(state_poverty_data, "final_data/state_poverty_data.csv")
