library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(plotly)
library(forcats)

ui = dashboardPage(
  dashboardHeader(title = "Transaction Analyzer"),
  dashboardSidebar(uiOutput("sidebarpanel")), 
  dashboardBody(uiOutput("body")))

server = function(input, output, sesion) {
  
  output$sidebarpanel <- renderUI({
    
    sidebarMenu(id="tabs", 
                menuItem("Overview", tabName = "Overview", icon = icon("search-dollar")),
                menuItem("Dashboard", tabName = "Dashboard", icon = icon("chart-line")),
                menuItem("Alert", tabName = "Alert", icon = icon("exclamation-circle")))})
  
  output$body <- renderUI({ 
    tabItems(tabItem(tabName = "Overview",
                     uiOutput("overview_tab")),
             tabItem(tabName = "Dashboard",
                     uiOutput("dashboard_tab")),
             tabItem(tabName = "Alert",
                     uiOutput("alert_tab")))})
  
  ###Overview tab###
  output$overview_tab <- renderUI({
    fluidRow(box(width = 12,
              fileInput("file1", "Please input your data file in csv format.",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons("disp", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head"),
    # Horizontal line ----
    tags$hr(),
    
    # Table
    DT::dataTableOutput("contents"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
  )
          
  })
  
  output$contents <- DT::renderDataTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  ###Dashboard tab###
  output$dashboard_tab <- renderUI({
    fluidRow(
      h4("Origin of customers"),
      plotlyOutput("ori"),
      h4("Currency used in transactions"),
      plotlyOutput("cur"),
      h4("Total number of transactions by month"),
      plotlyOutput("mon"),
      h4("Total number of transactions by day"),
      plotlyOutput("day"),
      h4("Transaction Time"),
      plotlyOutput("tim"),
      h4("Transfer Type"),
      plotlyOutput("typ"),
      h4("Transaction Amount"),
      plotlyOutput("amo")
    )
  })
  
  output$tot <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    n <- data %>% nrow()
    valueBox(n, icon = "swap_horizontal_line")
    
  })
  
  output$ori <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    plot_ly(data, labels=~country, type="pie")
    
  })
  
  output$cur <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    plot_ly(data, labels=~currency, type="pie")
    
  })
  
  output$mon <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    data %>% count(month) %>% 
      mutate(month = fct_reorder(month, n, .desc = TRUE)) %>%
      plot_ly(x = ~month, y = ~n) %>% add_bars()
    
  })
  
  output$day <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    data %>% count(weekday) %>% 
      mutate(weekday = fct_reorder(weekday, n, .desc = TRUE)) %>%
      plot_ly(x = ~weekday, y = ~n) %>% add_bars()
    
  })
  
  output$tim <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    data %>% count(time_range) %>% 
      mutate(time_range = fct_reorder(time_range, n, .desc = TRUE)) %>%
      plot_ly(x = ~time_range, y = ~n) %>% add_bars()
    
  })
  
  output$typ <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    data %>% count(transfer_type) %>% 
      mutate(transfer_type = fct_reorder(transfer_type, n, .desc = TRUE)) %>%
      plot_ly(x = ~transfer_type, y = ~n) %>% add_bars()
    
  })
  
  
  output$amo <- renderPlotly({
    
    data <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
    
    plot_ly(data, x=~amount, type="histogram", xbins = list(start = 0, size = 500))
  
  })
  
  ###Alert tab###
  output$alert_tab <- renderUI({
    
    data <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    
    n <- data %>% nrow()
    minday <- data %>% summarise(minday = min(weekday))
    mintime <- data %>% summarise(mintime = min(time_range))
    
    fluidRow(
      infoBox("Total Transactions", n, icon = icon("credit-card")),
      infoBox("Next system update day", minday, icon = icon("calendar-day")),
      infoBox("Next system update time", mintime, icon = icon("clock"))
    )
  })

}  
  
shinyApp(ui, server)
