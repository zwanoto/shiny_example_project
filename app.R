library(DBI)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(profvis)

ui <- dashboardPage(
  # Setting the title of the dashboard
  dashboardHeader(title = "ConsoleInsights - Your Google Search Console Analyzer"),
  
  # Defining the Sidebar with various Menu Items and conditional Panels for inputs
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "page4", icon = icon("info")),
      menuItem("Google Rankings", tabName = "page1", icon = icon("list-ol")),
      menuItem("Clicks and Impressions", tabName = "page2", icon = icon("chart-bar")),
      menuItem("Google Search Milestones", tabName = "page3", icon = icon("chart-bar"))
    ),
    # Inputs for Google Rankings Tab
    conditionalPanel(
      condition = "input.tabs === 'page1'",
      dateRangeInput("dates", "Select Date Range:", start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("query_type", "Query Type:", choices = c("Top Queries", "Improving Queries", "Declining Queries")),
      selectInput("country", "Country:", choices = c("All", "DACH-area")),
      selectInput("device", "Device Type:", choices = c("All", "Desktop", "Mobile"))
    ),
    # Inputs for Clicks and Impressions Tab
    conditionalPanel(
      condition = "input.tabs === 'page2'",
      dateRangeInput("dates2", "Select Date Range:", start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("country2", "Country:", choices = c("All", "DACH-area", "Austria", "Germany", "Switzerland")),
      selectInput("device2", "Device Type:", choices = c("All", "Desktop", "Mobile")),
      checkboxInput("addMovingAverage", "Add moving average", value = FALSE),
      # Additional Input if Moving Average is selected
      conditionalPanel(
        condition = "input.addMovingAverage === true",
        selectInput("movingAverageDays", "Moving Average Days:", choices = c("7 days", "15 days", "30 days", "90 days"))
      )
    )
  ),
  
  # Defining the Body of the dashboard with different Tab Items
  dashboardBody(
    tabItems(
      tabItem(tabName = "page4",
              h2("Summary Information"),
              fluidRow(
                box(title = "About", 
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    icon = icon("signal", lib = "glyphicon"),
                    "This Shiny app is designed to fetch and analyze data from the Google Search Console for a specific website, providing users with valuable insights into Google rankings and user behavior. The app showcases data from my own website, ", 
                    tags$a(href = "https://www.statuno.com", target = "_blank", "www.statuno.com"), 
                    ". If you are interested in utilizing this Shiny dashboard for your own website, feel free to contact me or visit ", 
                    tags$a(href = "https://github.com/zwanoto/shiny_example_project", target = "_blank", "github.com/zwanoto"), 
                    " for further instructions and source code.", 
                    tags$br(),
                    tags$br(),
                    "Below is an overview of the various functionalities and features of the dashboard:",
                    tags$ul(
                      tags$li("Visual representation of Google rankings for selected queries."),
                      tags$li("Insightful tables and graphs depicting user behavior and website performance."),
                      tags$li("Ability to filter and customize data views based on specific criteria."),
                      tags$li("Detailed milestones and achievements from Google Search Console.")
                    ),
                    tags$br(),
                    "Leverage the insights provided by this dashboard to optimize your website’s SEO and enhance its online presence!"
                )
              ),
              fluidRow(
                box(title = "Google Rankings for Selected Query", 
                    width = 4,
                    status = "primary",
                    solidHeader = TRUE,
                    icon = icon("signal", lib = "glyphicon"),
                    "Understanding your site's search ranking is pivotal for optimizing visibility. This visual provides a nuanced understanding by showcasing the monthly average Google rankings for chosen queries. Notably, users can select:",
                    tags$ul(
                      tags$li(icon("check-circle"), "Best-ranked queries to recognize top-performing content."),
                      tags$li(icon("arrow-up"), "Queries that have shown recent improvements, allowing users to identify what changes might have positively impacted SEO."),
                      tags$li(icon("arrow-down"), "Queries facing a decline, serving as a call-to-action to address and rectify potential issues.")
                    ),
                ),
                box(title = "Queries Overview", 
                    width = 4,
                    status = "warning",
                    solidHeader = TRUE,
                    icon = icon("search", lib = "glyphicon"),
                    "Deep diving into specific queries can spotlight areas of concern and excellence. This overview offers:",
                    tags$ul(
                      tags$li(icon("trophy"), "A glimpse at the current Google rank for the three highest-performing queries, offering an immediate understanding of standout content."),
                      tags$li(icon("exclamation-triangle"), "Insight into the queries that have witnessed the most significant decline. By juxtaposing the best rank with the current, one can quantify performance drop-offs.")
                    ),
                ),
                box(title = "Most Important Queries", 
                    width = 4,
                    status = "info",
                    solidHeader = TRUE,
                    icon = icon("star", lib = "glyphicon"),
                    "Assessing the significance of a query goes beyond its rank. This scatterplot elucidates this by plotting query rank against impressions. Here:",
                    tags$ul(
                      tags$li(icon("chart-bar"), "The x-axis depicts Google rank, informing about visibility."),
                      tags$li(icon("eye"), "The y-axis represents impressions, indicating user engagement."),
                      tags$li(icon("balance-scale"), "Dot size signifies the query's importance—a holistic measure derived by weighing impressions against rank.")
                    ),
                )
              ),
              fluidRow(
                box(title = "Clicks and Impressions Overview", 
                    width = 12,
                    status = "success",
                    solidHeader = TRUE,
                    icon = icon("mouse-pointer", lib = "glyphicon"),
                    "Engagement metrics are paramount to gauge site traction. This section:", 
                    tags$ul(
                      tags$li(icon("thumbs-up"), "Presents cumulative data on impressions and clicks—key metrics reflecting user interaction and interest."),
                      tags$li(icon("filter"), "Offers customizable views via filters like country and device type, ensuring a granular understanding of user demographics."),
                      tags$li(icon("chart-line"), "Facilitates trend analysis by allowing moving averages, useful to smoothen volatile data and perceive overarching trends.")
                    ),
                )
              ),
              fluidRow(
                box(title = "Google Milestones", 
                    width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    icon = icon("flag-checkered", lib = "glyphicon"),
                    "Recognizing achievements can bolster SEO efforts. Google Search Console's milestones are driven by click counts over thirty days. Through this visualization:",
                    tags$ul(
                      tags$li(icon("medal"), "Users can celebrate recent milestones, fostering a culture of achievement."),
                      tags$li(icon("rocket"), "It becomes transparent how distant the next milestone is, serving as a motivation to push for further optimization.")
                    ),
                )
              ),
              fluidRow(
                box(title = "About Me",
                    width = 12,
                    solidHeader = TRUE,
                    icon = icon("user", lib = "glyphicon"),
                    div(style = "padding:10px;", 
                        tags$img(src = "https://www.statuno.com/assets/images/Thomas.jpg", height = "120px", width = "120px", style = "padding-right:10px;"),
                        "Dipl. Ing. Thomas Zwanowetz",
                        tags$br(),
                        tags$a(href = "https://www.statuno.com", target = "_blank", "www.statuno.com")
                    )
                )
              )),
     # Google Rankings Page Section
     tabItem(tabName = "page1",
              h2("Google Rankings for selected queries"),
              fluidRow(
                box(title = "Google Ranking", plotOutput("scatter_plot"), width = 12)
              ),
              fluidRow(
                box(title = "Top Performing Queries", tableOutput("top_queries"), width = 6),
                box(title = "Queries Needing Attention", tableOutput("attention_queries"), width = 6)
              ),
              fluidRow(
                box(title = "Most important queries", plotOutput("query_plot"), width = 12)
              )
      ),
     # Google Search Console Milestones Page Section
     tabItem(tabName = "page2",
              fluidRow(
                box(
                  title = "Total Clicks", 
                  uiOutput("clicks_ui"),
                  width = 3,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  icon = icon("mouse-pointer")
                ),
                
                box(
                  title = "Total Impressions", 
                  uiOutput("impressions_ui"),
                  width = 3,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  icon = icon("eye")
                ),
                
                box(
                  title = "Average CTR", 
                  uiOutput("ctr_ui"),
                  width = 3,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  icon = icon("percent")
                )
              ),
              fluidRow(
                box(title = "Clicks over time", plotOutput("line_chart"), width = 6),
                box(title = "Impressions over time", plotOutput("line_chart2"), width = 6)
              ),
              fluidRow(
                box(title = "Clicks per weekday", plotOutput("bar_chart"), width = 6),
                box(title = "Impressions per weekday", plotOutput("bar_chart2"), width = 6)
              ),
              fluidRow(
                box(title = "Clicks per month", plotOutput("bar_chart3"), width = 6),
                box(title = "Impressions per month", plotOutput("bar_chart4"), width = 6)
              )
      ),
      tabItem(tabName = "page3",
              h2("Google Search Console Milestones"),
              
              fluidRow(
                box(title = "", plotOutput("milestones"), width = 12)
              )
      )
    )
  )
)



server <- function(input, output, session) {
  # Establish a connection to the database
  config <- readRDS("config.RDS")
  
  con <- dbConnect(RMySQL::MySQL(), dbname = "google_search_console", 
                   host = config$db_host, port = config$db_port, 
                   user = config$db_user, password = config$db_password)
  # Fetch data from the database tables
  df <- dbGetQuery(con,"SELECT * FROM df_all")
  df1 <- dbGetQuery(con, "SELECT * FROM df1")
  # Reactive wrapper around the data fetching and processing for df_all
  df_all_reactive <- reactive({
    # Filter and process df_all based on input device and dates
    # Handle country filter separately
    # After filtering, perform necessary calculations and grouping
    # Return the processed df_all
    df_all <- df %>%
      filter(device == input$device) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    if (input$country != 'All') {
      df_all <- df_all %>%
        filter(country != 'All')
    }
    else {
      df_all <- df_all %>% filter(country == 'All')
    }
    df_all[is.na(df_all)] <- 0
    df_all <- df_all %>%
      mutate(Keyword = query, month = month(date)) %>%
      group_by(query, month) %>%
      summarize(
        position = sum(position * impressions) / sum(impressions),
        impressions = sum(impressions),
        .groups = 'drop' 
      )
    return(df_all)
  })
  # Reactive wrapper for queries
  queries_reactive <- reactive({
    # Perform calculations and grouping on df.
    # Return the modified data frame.
    df_all = df_all_reactive()
    df_all[is.na(df_all)] = 0 
    df_all <- df_all %>% group_by(query) %>% summarize(position = sum(position*impressions)/sum(impressions),impressions = sum(impressions))
    return(df_all)
  })
  # Reactive wrapper for result
  result_reactive <- reactive({
    # Perform grouping, summarization, and ordering on df
    # Return the best performing queries
    best = df_all_reactive() 
    best = best[complete.cases(best),]
    best = best %>% group_by(query) %>% summarize(position = last(position)) %>% arrange(position)
    return(best)
  })
  # Reactive wrapper for plot data
  df_plot <- reactive({
    # Determine the top, best, and worst performing queries
    # Depending on input$query_type, filter accordingly
    # Return the df_plot for rendering
    best = df_all_reactive() 
    best = best[complete.cases(best),]
    best = best %>% group_by(query) %>% summarize(performance= last(position)/min(position)) %>% arrange(performance)
    worst = best %>% arrange(-performance)
    top = df_all_reactive() 
    top = top[complete.cases(top),]
    top = top %>% group_by(query) %>% summarize(performance= last(position)) %>% arrange(performance)
    top = top[1:3,]
    best = best[1:3,]
    worst = worst[1:3,]
    worst$query
    if(input$query_type =='Improving Queries'){
      df_plot = df_all_reactive() %>% filter(query %in% best$query)
    }
    else if(input$query_type =='Declining Queries'){
      df_plot = df_all_reactive() %>% filter(query %in% worst$query)
    }
    else {      df_plot = df_all_reactive() %>% filter(query %in% top$query)}
    
    return(df_plot)
  })

  # Reactive wrapper around the data fetching and processing for df2
  df2_reactive <- reactive({
    # Filter and process df1 based on input device2 and country2
    # Calculate moving averages based on input$movingAverageDays
    # Return the processed df1
    df1 = df1 %>% filter(device == input$device2)
    if (input$country2 == 'DACH-area'){
      df1 = df1 %>% filter(country != 'All')
      df1[is.na(df1)] = 0 
      df1 <- df1 %>% group_by(date) %>% summarize(clicks = sum(clicks),position = sum(position*impressions)/sum(impressions),impressions = sum(impressions))
      
    }
    else {
      df1 = df1 %>% filter(country == input$country2)
    }
    
    df1$date = as.Date(df1$date)
    df1 = df1 %>% filter(df1$date >=input$dates2[1], df1$date <= input$dates2[2])
    df1$month = month(df1$date)
    # Define a lookup table
    days_lookup <- c(
      `7 days` = 7,
      `15 days` = 15,
      `30 days` = 30,
      `90 days` = 90
    )
    
    # Get the number of days from the lookup table
    k <- days_lookup[[input$movingAverageDays]]
    
    # If a valid number of days is found, calculate the moving averages
    if (!is.null(k)) {
      df1$moving_avg_clicks <- zoo::rollmean(df1$clicks, k = k, fill = NA)
      df1$moving_avg_impressions <- zoo::rollmean(df1$impressions, k = k, fill = NA)
    }
    
    return(df1)
  })
  
  # Reactive wrapper around the summary calculations
  df1_summary_reactive <- reactive({
    df1 <- df2_reactive()
    df1 =  df1 %>%  summarize(clicks = sum(clicks), impressions_ = sum(impressions),position = sum(impressions*position)/sum(impressions), .groups = 'drop')
    })
  
  # Calculate summary metrics based on user's inputs
  output$clicks_ui <- renderUI({
    df1_summary <- df1_summary_reactive()
    clicks <- sum(df1_summary$clicks)
    tags$div(style = "font-size: 24px; padding: 0px;", clicks)
  })
  
  output$impressions_ui <- renderUI({
    df1_summary <- df1_summary_reactive()
    impressions <- sum(df1_summary$impressions_)
    tags$div(style = "font-size: 24px; padding: 0px;", impressions)
  })
  
  output$ctr_ui <- renderUI({
    df1_summary <- df1_summary_reactive()
    clicks <- sum(df1_summary$clicks)
    impressions <- sum(df1_summary$impressions_)
    ctr = sprintf("%.1f%%", round(clicks/impressions * 100, 2))
    tags$div(style = "font-size: 24px; padding: 0px;", ctr)
  })
  output$position_ui <- renderUI({
    df1_summary <- df1_summary_reactive()
    df1_summary[is.na(df1_summary)] = 0 
    clicks <- sum(df1_summary$clicks)
    position = sum(df1_summary$position*df1_summary$impressions_)/sum(df1_summary$impressions_)
    tags$div(style = "font-size: 24px; padding: 0px;", round(position))
  })
  
  # Render scatter plot based on user's inputs
  # Function to format labels
  label_format <- function(x) format(1/x, digits = 2)
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    df_all <- df_plot()
    df_all$Reciprocal_Position <- 1 / df_all$position
    ggplot(df_all, aes(x = month, y = Reciprocal_Position, col = query)) +
      geom_line() + 
      scale_y_continuous(labels = label_format) +
      labs(x = "Datum", y = "Position") +
      theme_minimal()
  })
  
  # Line Chart
  output$line_chart <- renderPlot({
    df1 = df2_reactive()
    p = ggplot(df1, aes(x = date, y = clicks)) +
      geom_area(aes(y = clicks), fill = 'steelblue', alpha = 0.4) +
      theme_minimal()
    if (input$addMovingAverage) {
      p = p + geom_line(aes(y = moving_avg_clicks), color = 'darkred', size = 1)
    } else {
      p = p + geom_point()
    }
    print(p)
  })
  
  # Second Line Chart
  output$line_chart2 <- renderPlot({
    df1 = df2_reactive()
    p = ggplot(df1, aes(x = date, y = impressions)) +
      geom_area(aes(y = impressions), fill = 'steelblue', alpha = 0.4) +
      theme_minimal()
    if (input$country2 == 'All') {
      p = p + scale_y_log10()
    }
    if (input$addMovingAverage) {
      p = p + geom_line(aes(y = moving_avg_impressions), color = 'darkred', size = 1)
    } else {
      p = p + geom_point()
    }
    print(p)
  })
  
  # Bar Chart 1
  output$bar_chart <- renderPlot({
    df1 <- df2_reactive()
    df1$date <- as.Date(df1$date)
    df1$weekday <- factor(weekdays(df1$date))
    levels(df1$weekday) = c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday")
    df1$weekday <- factor(df1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    df1_grouped <- df1 %>% 
      group_by(weekday) %>% 
      summarize(clicks = mean(clicks), .groups = 'drop')
    ggplot(df1_grouped, aes(x = weekday, y = clicks)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Average Clicks per Weekday", x = "Weekday", y = "Average Clicks")
  })
  
  # Bar Chart 2
  output$bar_chart2 <- renderPlot({
    df1 <- df2_reactive()
    df1$date <- as.Date(df1$date)
    df1$weekday <- factor(weekdays(df1$date))
    levels(df1$weekday) = c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday")
    df1$weekday <- factor(df1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    df1_grouped <- df1 %>% 
      group_by(weekday) %>% 
      summarize(impressions = mean(impressions), .groups = 'drop')
    ggplot(df1_grouped, aes(x = weekday, y = impressions)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Average Impressions per Weekday", x = "Weekday", y = "Average Impressions")
  })
  
  # Bar Chart 3
  output$bar_chart3 <- renderPlot({
    df1 <- df2_reactive()
    df1$date <- as.Date(df1$date)
    df1$month <- factor(month(df1$date))
    
    df1_grouped <- df1 %>% 
      group_by(month) %>% 
      summarize(clicks = mean(clicks), .groups = 'drop')
    
    ggplot(df1_grouped, aes(x = month, y = clicks)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Month", y = "Average Clicks")
  })
  
  # Bar Chart 4
  output$bar_chart4 <- renderPlot({
    df1 <- df2_reactive()
    df1$date <- as.Date(df1$date)
    df1$month <- factor(month(df1$date))
    df1_grouped <- df1 %>% 
      group_by(month) %>% 
      summarize(impressions = mean(impressions), .groups = 'drop')
    # Creating the plot with improved aesthetics
    ggplot(df1_grouped, aes(x = month, y = impressions)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Month", y = "Average Impressions")
  })
  
  output$query_plot <- renderPlot({
    df = queries_reactive() 
    p <- ggplot(df, aes(x = position, y = impressions, color = query)) +
      geom_point(aes(size = impressions/position)) +
      geom_text(aes(label = query), hjust = -0.1, vjust = 0.5, size = 5) +
      labs(title = "Most important queries", x = "Position", y = "Impressions") +
      theme_light() +
      theme(legend.position = "none") +
      scale_y_log10() 
    print(p)    
  })
  
  output$top_queries <- renderTable({
    result = result_reactive()
    head(result,3)  
  })
  
  output$attention_queries <- renderTable({
    best = df_all_reactive() 
    best = best[complete.cases(best),]
    best = best %>% group_by(query) %>% summarize(performance= 1/(last(position)/min(position))) %>% arrange(performance)
    best = best[1:3,]
    return(best)
  })
  
  output$milestones <- renderPlot({
    df1 <- df1 %>%
      filter(country == 'All', device == 'All')
    df1$date <- as.Date(df1$date)
    df1$last_30_days <- 30 * zoo::rollmean(df1$clicks, k = 30, align = 'right', fill = NA)
    # Initialize a variable to track if thresholds have been crossed
    cross_index20 <- which(df1$last_30_days >= 20)[1]
    cross_index30 <- which(df1$last_30_days >= 30)[1]
    cross_index40 <- which(df1$last_30_days >= 40)[1]
    df1$milestone = FALSE
    df1$milestone[c(cross_index20,cross_index30,cross_index40)]  = TRUE
    ggplot(df1, aes(x = date, y = last_30_days)) +
      geom_line() +  # Line plot
      geom_point(data = df1[df1$milestone, ], aes(color = milestone), size = 3, fill = "red") +  # Points for milestones
      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "black")) +  # Color the points
      labs(
        title = "Last 30 Days Clicks with Milestones",
        x = "Date",
        y = "Clicks"
      ) +
      theme(legend.position = "none") +  # Remove the legend
      geom_hline(yintercept = 50, linetype = "dashed", color = "blue") +  # Add dashed line at y = 50
      annotate("text", x = max(df1$date), y = 50, label = "Next Milestone", hjust = 1.1, vjust = 0.5)  # Add text annotation
  })
}

shinyApp(ui = ui, server = server)
