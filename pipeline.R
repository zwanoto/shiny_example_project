library(DBI)
library(RMySQL)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggimage)
library(googleAuthR)
library(searchConsoleR)


# Load the configuration file
config <- readRDS("config.RDS")

# Set parameters
scope <- "https://www.googleapis.com/auth/webmasters"
countries <- c("AUT", "DEU", "CHE")
devices <- c("MOBILE", "DESKTOP")
website <- "sc-domain:statuno.com"
date_2_days_ago <- Sys.Date()

# Authentication
gar_auth_service(config$auth_file, scope = scope)

# Rest of the script remains the same...

# Create a connection
con <- dbConnect(RMySQL::MySQL(), dbname = "google_search_console", 
                 host = config$db_host, port = config$db_port, 
                 user = config$db_user, password = config$db_password)

website <- "sc-domain:statuno.com"
date_2_days_ago <- Sys.Date()

# Create a function to avoid code repetition
get_search_analytics <- function(dimension, filterExp = NULL) {
  search_analytics(siteURL = website, startDate = "2023-01-01", endDate = date_2_days_ago, dimensions = dimension, dimensionFilterExp = filterExp)
}


# Define a function to encapsulate the common logic
get_combined_analytics <- function(device = NULL, country = NULL) {
  
  filters <- c()
  if (!is.null(device)) filters <- c(filters, paste0("device==", device))
  if (!is.null(country)) filters <- c(filters, paste0("country==", country))
  
  df2 <- get_search_analytics('query', filters)
  top_10_queries <- df2 %>% arrange(-impressions) %>% pull(query) %>% head(10)
  
  combined_df <- map_df(top_10_queries, function(query) {
    df_temp <- get_search_analytics('date', c(paste0("query==", query)))
    mutate(df_temp, query = query)
  })
  
  combined_df$country <- ifelse(is.null(country), 'All', country)
  combined_df$device <- ifelse(is.null(device), 'All', device)
  
  return(combined_df)
}

# Define the sets of devices and countries
devices <- c("MOBILE", "DESKTOP", NULL)
countries <- c("AUT", "DEU", "CHE", NULL)

# Generate all combinations of devices and countries, including 'All'
combinations <- expand.grid(device = devices, country = countries)

# Use the purrr::map_df function to apply get_combined_analytics to each combination
combined <- map_df(seq_len(nrow(combinations)), function(i) {
  get_combined_analytics(combinations$device[i], combinations$country[i])
})
# Define a function to fetch search analytics and assign country and device
fetch_analytics <- function(device_type, country_code) {
  country_names <- c(AUT = "Austria", DEU = "Germany", CHE = "Switzerland")
  country_name <- ifelse(country_code %in% names(country_names), country_names[country_code], "All")
  
  # Create a list of filters based on the provided arguments
  filters <- c()
  if (device_type != "All") filters <- c(filters, paste0("device==", device_type))
  if (country_code != "All") filters <- c(filters, paste0("country==", country_code))
  
  df <- get_search_analytics('date', filters)
  df$country <- country_name
  df$device <- ifelse(device_type == "All", "All", device_type)
  
  return(df)
}

# Define combinations of country codes and device types
country_codes <- c("AUT", "DEU", "CHE", "All")
device_types <- c("MOBILE", "DESKTOP", "All")

# Generate all possible combinations and fetch analytics for each combination
analytics_list <- lapply(country_codes, function(country_code) {
  lapply(device_types, function(device_type) {
    fetch_analytics(device_type, country_code)
  })
})

# Flatten the list and bind all data frames together
df2 <- do.call(rbind, unlist(analytics_list, recursive = FALSE))

df2 %>% filter(country=='ALL',device=='ALL') %>% summarize()

write_df_to_sql <- function(connection, dataframe, table_name) {
  # Check if the table already exists
  if (dbExistsTable(connection, table_name)) {
    # If the table exists, append the data
    dbWriteTable(connection, table_name, dataframe, append = FALSE, overwrite=TRUE,row.names = FALSE)
  } else {
    # If the table doesn't exist, create a new table and write the data
    dbWriteTable(connection, table_name, dataframe, row.names = FALSE)
  }
}



# Call the write_df_to_sql function
write_df_to_sql(con, df2, 'df1')
write_df_to_sql(con, combined, 'df_all')


dbGetQuery(con,"SELECT COUNT(*) FROM df1 WHERE country='ALL' AND device='ALL'")

combined
