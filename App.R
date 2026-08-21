library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(scales)
library(lubridate)
library(htmltools)
library(knitr)
library(webshot2)

# 1. DEFAULT DATASET
# Simulated multi-region sales data so the app is useful out of the box.

make_default_data <- function() {
  set.seed(42)
  regions      <- c("North America", "Europe", "Asia Pacific", "Latin America")
  categories   <- c("Electronics", "Apparel", "Home & Garden", "Sports", "Beauty")
  products <- list(
    "Electronics"    = c("Wireless Earbuds", "Smart Watch", "Bluetooth Speaker", "Laptop Stand"),
    "Apparel"        = c("Running Shoes", "Denim Jacket", "Yoga Pants", "Wool Sweater"),
    "Home & Garden"  = c("Garden Hose", "LED Lamp", "Throw Pillow", "Plant Pot"),
    "Sports"         = c("Yoga Mat", "Dumbbell Set", "Tennis Racket", "Cycling Helmet"),
    "Beauty"         = c("Face Serum", "Lip Balm Set", "Hair Dryer", "Sunscreen"))
  salespeople <- c("Amara Chen", "Diego Ruiz", "Priya Nair", "Jonas Berg",
                   "Lena Kowalski", "Malik Osei", "Sofia Rossi", "Ethan Cole")
  
  dates <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
  n <- 6000
  
  cat_choice <- sample(categories, n, replace = TRUE)
  prod_choice <- vapply(cat_choice, function(c) sample(products[[c]], 1), character(1))
  
  df <- data.frame(
    Date        = sample(dates, n, replace = TRUE),
    Region      = sample(regions, n, replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10)),
    Category    = cat_choice,
    Product     = prod_choice,
    Salesperson = sample(salespeople, n, replace = TRUE),
    Units       = sample(1:20, n, replace = TRUE),
    stringsAsFactors = FALSE)
  
  unit_price <- round(runif(n, 8, 300), 2)
  df$Revenue <- round(df$Units * unit_price, 2)
  df$Cost    <- round(df$Revenue * runif(n, 0.45, 0.75), 2)
  df$Profit  <- round(df$Revenue - df$Cost, 2)
  
  df <- df[order(df$Date), ]
  rownames(df) <- NULL
  df
}

DEFAULT_DATA <- make_default_data()

# Standardize an uploaded / loaded dataset so downstream code can rely on it
standardize_data <- function(df) {
  names(df) <- trimws(names(df))
  # case-insensitive rename to canonical names where possible
  canonical <- c("Date","Region","Category","Product","Salesperson","Units","Revenue","Cost")
  for(cname in canonical){
    hit <- which(tolower(names(df)) == tolower(cname))
    if (length(hit) == 1) names(df)[hit] <- cname
  }
  
  required <- c("Date","Region","Category","Product","Units","Revenue")
  missing <- setdiff(required, names(df))
  if(length(missing) > 0){
    stop(paste0("Missing required column(s): ", paste(missing, collapse = ", ")))
  }
  
  df$Date   <- as.Date(df$Date)
  df$Units  <- suppressWarnings(as.numeric(df$Units))
  df$Revenue <- suppressWarnings(as.numeric(df$Revenue))
  
  if (!"Cost" %in% names(df))        df$Cost <- NA_real_
  if (!"Salesperson" %in% names(df)) df$Salesperson <- "Unassigned"
  
  df$Cost <- suppressWarnings(as.numeric(df$Cost))
  df$Profit <- ifelse(is.na(df$Cost), NA_real_, df$Revenue - df$Cost)
  
  df <- df[!is.na(df$Date) & !is.na(df$Revenue), ]
  df
}

# 2. UI
ui <- dashboardPage(dashboardHeader(title = "Sales & BI Dashboard"),
                    
                    dashboardSidebar(sidebarMenu(
                      id = "tabs",
                      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
                      menuItem("Data Explorer", tabName = "data", icon = icon("table"))),
                      tags$hr(),
                      h4(" Data Source", style = "padding-left:15px;"),
                      div(style = "padding: 0 15px;",
                          radioButtons("data_source", NULL,
                                       choices = c("Use default sample dataset" = "default",
                                                   "Upload my own dataset" = "custom"),
                                       selected = "default"),
                          conditionalPanel(condition = "input.data_source == 'custom'",
                                           fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".xlsx", ".xls")),
                                           helpText("Required columns: Date, Region, Category, Product,", 
                                                    "Units, Revenue. Optional: Salesperson, Cost."),
                                           actionButton("reset_data", "Reset to default", icon = icon("rotate-left"), class = "btn-sm"))),
                      tags$hr(),
                      div(style = "padding: 0 15px;",
                          uiOutput("date_filter_ui"),
                          uiOutput("region_filter_ui"),
                          uiOutput("category_filter_ui"),
                          uiOutput("salesperson_filter_ui"),
                          tags$hr(),
                          selectInput("report_type", "Download Report",
                                      choices = c("PDF" = "pdf",
                                                  "HTML" = "html",
                                                  "PNG Image" = "image"),
                                      selected = "pdf"),
                          downloadButton("download_report", "Download Report",
                                         class = "btn-primary btn-block"))),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML("
      .small-box h3 { font-size: 26px; }
      .box { border-top: 3px solid #3c8dbc; }
    "))),
                      tabItems(
                        tabItem(tabName = "overview",
                                fluidRow(
                                  valueBoxOutput("kpi_revenue", width = 3),
                                  valueBoxOutput("kpi_profit", width = 3),
                                  valueBoxOutput("kpi_units", width = 3),
                                  valueBoxOutput("kpi_aov", width = 3)),
                                fluidRow(box(title = "Revenue Trend", width = 8, status = "primary", solidHeader = TRUE, plotOutput("trend_plot", height = 300)),
                                         box(title = "Revenue by Region", width = 4, status = "primary", solidHeader = TRUE, plotOutput("region_plot", height = 300))),
                                fluidRow(box(title = "Revenue by Category", width = 6, status = "primary", solidHeader = TRUE,
                                             plotOutput("category_plot", height = 300)),
                                         box(title = "Top 10 Products by Revenue", width = 6, status = "primary", solidHeader = TRUE,
                                             plotOutput("top_products_plot", height = 300))),
                                fluidRow(box(title = "Top Salespeople by Revenue", width = 12, status = "primary", solidHeader = TRUE,
                                             plotOutput("salesperson_plot", height = 300)))),
                        tabItem(tabName = "data",
                                fluidRow(box(title = "Filtered Data", width = 12, status = "primary", solidHeader = TRUE,
                                             downloadButton("download_data", "Download filtered data (CSV)"),
                                             tags$br(), tags$br(),
                                             DTOutput("data_table")))))))

# 3. SERVER
server <- function(input, output, session) {
  
  # Raw dataset reactive: default or uploaded
  raw_data <- reactiveVal(DEFAULT_DATA)
  
  observeEvent(input$data_source, {
    if (input$data_source == "default") raw_data(DEFAULT_DATA)})
  
  observeEvent(input$reset_data, {
    updateRadioButtons(session, "data_source", selected = "default")
    raw_data(DEFAULT_DATA)})
  
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(read_excel(input$file$datapath))
      } else {
        stop("Unsupported file type. Please upload a .csv, .xlsx, or .xls file.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
      NULL
    })
    req(df)
    
    df_std <- tryCatch(standardize_data(df), error = function(e) {
      showNotification(paste("Error in dataset:", e$message), type = "error", duration = 8)
      NULL
    })
    req(df_std)
    
    raw_data(df_std)
    showNotification("Custom dataset loaded successfully.", type = "message")
  })
  
  # Dynamic filter UI
  output$date_filter_ui <- renderUI({
    df <- raw_data()
    dateRangeInput("date_range", "Date range",
                   start = min(df$Date, na.rm = TRUE),
                   end   = max(df$Date, na.rm = TRUE),
                   min   = min(df$Date, na.rm = TRUE),
                   max   = max(df$Date, na.rm = TRUE))})
  
  output$region_filter_ui <- renderUI({
    df <- raw_data()
    selectInput("region_sel", "Region", choices = sort(unique(df$Region)), 
                selected = sort(unique(df$Region)), multiple = TRUE)})
  
  output$category_filter_ui <- renderUI({
    df <- raw_data()
    selectInput("category_sel", "Category", choices = sort(unique(df$Category)),
                selected = sort(unique(df$Category)), multiple = TRUE)})
  
  output$salesperson_filter_ui <- renderUI({
    df <- raw_data()
    selectInput("salesperson_sel", "Salesperson", choices = sort(unique(df$Salesperson)), 
                selected = sort(unique(df$Salesperson)), multiple = TRUE)})
  
  # Filtered data
  filtered_data <- reactive({
    df <- raw_data()
    req(input$date_range, input$region_sel, input$category_sel, input$salesperson_sel)
    
    df %>%
      filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        Region %in% input$region_sel,
        Category %in% input$category_sel,
        Salesperson %in% input$salesperson_sel)})
  
  # KPI boxes
  output$kpi_revenue <- renderValueBox({
    valueBox(dollar(sum(filtered_data()$Revenue, na.rm = TRUE), largest_with_cents = 1e6),
             "Total Revenue", icon = icon("dollar-sign"), color = "green")})
  
  output$kpi_profit <- renderValueBox({
    p <- sum(filtered_data()$Profit, na.rm = TRUE)
    valueBox(dollar(p, largest_with_cents = 1e6), "Total Profit",
             icon = icon("chart-pie"), color = if (is.na(p) || p >= 0) "blue" else "red")})
  
  output$kpi_units <- renderValueBox({
    valueBox(comma(sum(filtered_data()$Units, na.rm = TRUE)), "Units Sold",
             icon = icon("boxes-stacked"), color = "purple")})
  
  output$kpi_aov <- renderValueBox({
    fd <- filtered_data()
    aov <- if (nrow(fd) > 0) sum(fd$Revenue, na.rm = TRUE) / nrow(fd) else 0
    valueBox(dollar(aov), "Avg Revenue / Order", icon = icon("receipt"), color = "yellow")})
  
  # Trend plot
  output$trend_plot <- renderPlot({
    fd <- filtered_data()
    validate(need(nrow(fd) > 0, "No data for the selected filters."))
    
    span_days <- as.numeric(diff(range(fd$Date)))
    fd_agg <- fd %>%
      mutate(period = if (span_days > 120) floor_date(Date, "month") else Date) %>%
      group_by(period) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")
    
    ggplot(fd_agg, aes(x = period, y = Revenue)) +
      geom_line(color = "#3c8dbc", linewidth = 1) +
      geom_point(color = "#3c8dbc") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
  })
  
  # Region plot
  output$region_plot <- renderPlot({
    fd <- filtered_data()
    validate(need(nrow(fd) > 0, "No data."))
    fd_agg <- fd %>% group_by(Region) %>% summarise(Revenue = sum(Revenue, na.rm = TRUE))
    
    ggplot(fd_agg, aes(x = reorder(Region, Revenue), y = Revenue, fill = Region)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
  })
  
  # Category plot
  output$category_plot <- renderPlot({
    fd <- filtered_data()
    validate(need(nrow(fd) > 0, "No data."))
    fd_agg <- fd %>% group_by(Category) %>% summarise(Revenue = sum(Revenue, na.rm = TRUE))
    
    ggplot(fd_agg, aes(x = "", y = Revenue, fill = Category)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void(base_size = 13) +
      theme(legend.title = element_blank())
  })
  
  # Top products
  output$top_products_plot <- renderPlot({
    fd <- filtered_data()
    validate(need(nrow(fd) > 0, "No data."))
    fd_agg <- fd %>%
      group_by(Product) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(Revenue)) %>%
      slice_head(n = 10)
    
    ggplot(fd_agg, aes(x = reorder(Product, Revenue), y = Revenue)) +
      geom_col(fill = "#605ca8") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
  })
  
  # Salesperson plot
  output$salesperson_plot <- renderPlot({
    fd <- filtered_data()
    validate(need(nrow(fd) > 0, "No data."))
    fd_agg <- fd %>%
      group_by(Salesperson) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(Revenue))
    
    ggplot(fd_agg, aes(x = reorder(Salesperson, Revenue), y = Revenue)) +
      geom_col(fill = "#00a65a") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
  })
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      if (input$report_type == "pdf") {
        paste0("sales_report_", Sys.Date(), ".pdf")
      } else if (input$report_type == "html") {
        paste0("sales_report_", Sys.Date(), ".html")
      } else {
        paste0("sales_report_", Sys.Date(), ".png")
      }
    },
    content = function(file) {
      
      fd <- filtered_data()
      validate(need(nrow(fd) > 0, "No data available."))
      
      total_revenue <- sum(fd$Revenue, na.rm = TRUE)
      total_profit <- sum(fd$Profit, na.rm = TRUE)
      total_units <- sum(fd$Units, na.rm = TRUE)
      avg_order <- total_revenue / nrow(fd)
      
      region_summary <- fd %>%
        group_by(Region) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE),
                  Units = sum(Units, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(Revenue))
      
      category_summary <- fd %>%
        group_by(Category) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE),
                  Units = sum(Units, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(Revenue))
      
      product_summary <- fd %>%
        group_by(Product) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE),
                  Units = sum(Units, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(Revenue)) %>%
        slice_head(n = 10)
      
      salesperson_summary <- fd %>%
        group_by(Salesperson) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE),
                  Units = sum(Units, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(Revenue))
      
      report <- tags$html(
        tags$head(
          tags$title("Sales & Business Intelligence Report"),
          tags$style(HTML("
            body {
              font-family: Arial, sans-serif;
              margin: 40px;
              color: #333;
              background: white;
            }

            h1 {
              color: #3c8dbc;
              border-bottom: 3px solid #3c8dbc;
              padding-bottom: 10px;
            }

            h2 {
              color: #3c8dbc;
              margin-top: 30px;
            }

            .summary {
              display: flex;
              gap: 15px;
              margin: 20px 0;
            }

            .card {
              padding: 15px;
              background: #f5f5f5;
              border-left: 4px solid #3c8dbc;
              flex: 1;
            }

            table {
              width: 100%;
              border-collapse: collapse;
              margin-top: 10px;
            }

            th {
              background: #3c8dbc;
              color: white;
              padding: 8px;
              text-align: left;
            }

            td {
              padding: 8px;
              border-bottom: 1px solid #ddd;
            }
          "))
        ),
        
        tags$body(
          tags$h1("Sales & Business Intelligence Report"),
          tags$p(paste("Generated:", format(Sys.time(), "%d %B %Y, %I:%M %p"))),
          tags$p(paste("Reporting Period:", format(min(fd$Date), "%d %b %Y"), "to", format(max(fd$Date), "%d %b %Y"))),
          tags$h2("Business Summary"),
          
          tags$div(
            class = "summary",
            tags$div(class = "card", tags$strong("Total Revenue"), tags$br(), dollar(total_revenue)),
            tags$div(class = "card", tags$strong("Total Profit"), tags$br(), dollar(total_profit)),
            tags$div(class = "card", tags$strong("Units Sold"), tags$br(), comma(total_units)),
            tags$div(class = "card", tags$strong("Avg Revenue / Order"), tags$br(), dollar(avg_order))),
          
          tags$h2("Revenue by Region"),
          
          HTML(as.character(knitr::kable(region_summary, format = "html", digits = 2))),
          tags$h2("Revenue by Category"),
          HTML(as.character(knitr::kable(category_summary, format = "html", digits = 2))),
          
          tags$h2("Top 10 Products by Revenue"),
          HTML(as.character(knitr::kable(product_summary, format = "html", digits = 2))),
          tags$h2("Salesperson Performance"),
          HTML(as.character(knitr::kable(salesperson_summary, format = "html", digits = 2)))))
      temp_html <- tempfile(fileext = ".html")
      htmltools::save_html(report, temp_html)
      if (input$report_type == "html") {
        file.copy(temp_html, file, overwrite = TRUE)
      } else if (input$report_type == "pdf") {
        
        webshot2::webshot(
          url = temp_html, file = file, vwidth = 1400, vheight = 1200, zoom = 1)
      } else if (input$report_type == "image") {
        
        webshot2::webshot(
          url = temp_html, file = file, vwidth = 1600, vheight = 1400, zoom = 1)}})
  
  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 15, scrollX = TRUE))})
  
  # Download
  output$download_data <- downloadHandler(
    filename = function() paste0("filtered_sales_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE))}

shinyApp(ui, server)