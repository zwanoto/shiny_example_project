# Load libraries
library(DBI)
library(RMySQL)
library(dplyr)
library(tidyverse)
library(googleAuthR)
library(searchConsoleR)

# Load configuration file
config <- readRDS("config.RDS")

# Set parameters
scope <- "https://www.googleapis.com/auth/webmasters"
countries <- c("AUT", "DEU", "CHE")
devices <- c("MOBILE", "DESKTOP")
website <- "sc-domain:statuno.com"
date_2_days_ago <- Sys.Date()

# Google Search Console Authentication
gar_auth_service(config$auth_file, scope = scope)

# Create a database connection
con <- dbConnect(RMySQL::MySQL(), dbname = "google_search_console",
                 host = config$db_host, port = config$db_port,
                 user = config$db_user, password = config$db_password)

# Function to fetch search analytics
get_search_analytics <- function(dimension, filterExp = NULL) {
  search_analytics(siteURL = website, startDate = "2023-01-01", endDate = date_2_days_ago, dimensions = dimension, dimensionFilterExp = filterExp)
}

# Function to get combined analytics
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

# Define combinations and fetch analytics
devices <- c("MOBILE", "DESKTOP", NULL)
countries <- c("AUT", "DEU", "CHE", NULL)
combinations <- expand.grid(device = devices, country = countries)

combined <- map_df(seq_len(nrow(combinations)), function(i) {
  get_combined_analytics(combinations$device[i], combinations$country[i])
})

# Fetch analytics based on different combinations and write to database
analytics_list <- lapply(countries, function(country) {
  lapply(devices, function(device) {
    df <- fetch_analytics(device, country)
    df
  })
})

df2 <- do.call(rbind, unlist(analytics_list, recursive = FALSE))

write_df_to_sql <- function(connection, dataframe, table_name) {
  if (dbExistsTable(connection, table_name)) {
    dbWriteTable(connection, table_name, dataframe, append = FALSE, overwrite = TRUE, row.names = FALSE)
  } else {
    dbWriteTable(connection, table_name, dataframe, row.names = FALSE)
  }
}

# Write data frames to SQL
write_df_to_sql(con, df2, 'df1')
write_df_to_sql(con, combined, 'df_all')

# Query the database
dbGetQuery(con, "SELECT COUNT(*) FROM df1 WHERE country='ALL' AND device='ALL'")
