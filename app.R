#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

###TUGAS KELOMPOK TEKNIK SIMULASI

# Load Libraries
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

# Function to generate data with normal distribution
generate_normal_data <- function(miu, sigma, n) {
  rnorm(n, mean = miu, sd = sigma)
}

home_page <- tabPanel(
  title = "Home",
  br(),
  h3("Analisis Regresi Sederhana dengan Distribusi Normal", 
     style = "text-align:center"),
  br(),br(),
  h5("Nama Kelompok 7:", style= "text-align:center"),
  h5("Kamalina Rosyida Supriyanti (B2A020062)", style= "text-align:center"),
  h5("Shella Heidy Permatasari (B2A020067)", style= "text-align:center"),
  h5("Choirunnisa Hasna (B2A020071)", style= "text-align:center"),
  br(),br(),
  h5("Department of Statistics", style= "text-align:center"),
  h5("Faculty of Science and Mathematics", style= "text-align:center"),
  h5("Muhammadiyah University of Semarang", style= "text-align:center"),
  h5("2023", style= "text-align:center")
)

main_page <- tabPanel(
  title = "Regresi",
  titlePanel("Model Regresi"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      h4('Variabel'),
      selectInput("vardep", "Variabel Dependen", choices = list("Y" = "Y")),
      selectInput("varindep", "Variabel Independen", choices = list("X" = "X")),
      h4('Parameter'),
      numericInput("miu", "Rata-rata (miu):", value = 0),
      numericInput("sigma", "Deviasi Standar (sigma):", value = 1),
      numericInput("n", "Jumlah Observasi (n):", value = 100),
      actionButton("generate", "Generate Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Ringkasan Model",
          verbatimTextOutput("summary")
        ),
        tabPanel(
          title = "Data",
          tableOutput("datatable")
        ),
        tabPanel(
          title = "Scatter Plot",
          plotOutput("scatterplot")
        ),
      )
    )
  )
)

ui <- navbarPage(
  title = "Aplikasi R-Shiny untuk Analisis Regresi Sederhana dengan Distribusi Normal",
  theme = shinytheme('cerulean'),
  home_page,
  main_page
)

# Define the server function
server <- function(input, output){
  # Reactive function to generate data with normal distribution
  data <- reactive({
    req(input$miu, input$sigma, input$n)
    data.frame(
      X = generate_normal_data(input$miu, input$sigma, input$n),
      Y = generate_normal_data(input$miu, input$sigma, input$n)
    )
  })
  
  
  # Perform linear regression and render the plot and summary
  observeEvent(input$generate, {
    output$scatterplot <- renderPlot({
      ggplot(data(), aes(x = X, y = Y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = "Variabel Independen (X)", y = "Variabel Dependen (Y)", title = "Scatter Plot with Linear Regression")
    })
    
    output$datatable<-renderTable({
      data()
    })
    
    # Fit the linear regression model
    model <- lm(Y ~ X, data = data())
    
    # Print summary of the regression model
    output$summary <- renderPrint({
      summary(model)
    })
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)