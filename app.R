library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("Data Visualization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      selectInput("plot_type", "Select Plot Type",
                  choices = c("Scatter Plot", "Bar Chart", "Box Plot", "Line Plot")),
      
      uiOutput("var_x"),
      uiOutput("var_y"),
      
      checkboxInput("show_trend", "Show Trend Line", FALSE)
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$var_x <- renderUI({
    req(data())
    selectInput("x_var", "X Variable", choices = names(data()))
  })
  
  output$var_y <- renderUI({
    req(data())
    selectInput("y_var", "Y Variable", choices = names(data()))
  })
  
  output$plot <- renderPlot({
    req(data(), input$x_var, input$y_var)
    
    p <- ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) +
      theme_minimal()
    
    switch(input$plot_type,
           "Scatter Plot" = {
             p <- p + geom_point(alpha = 0.6)
             if(input$show_trend) p <- p + geom_smooth(method = "lm")
           },
           "Bar Chart" = p + geom_bar(stat = "identity"),
           "Box Plot" = p + geom_boxplot(),
           "Line Plot" = p + geom_line()
    )
    
    p + labs(title = paste(input$plot_type, "of", input$y_var, "vs", input$x_var),
             x = input$x_var,
             y = input$y_var)
  })
  
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
}

shinyApp(ui = ui, server = server)