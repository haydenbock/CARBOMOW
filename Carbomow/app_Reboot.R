#install.packages("shiny")
#install.packages("plotly")
#install.packages("dplyr")

library(shiny)
library(plotly)
library(dplyr)



ui <- fluidPage(
  titlePanel("Golf Course Carbon Stocks Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grassSpecies", "Grass Species", 
                  choices = c("Creeping Bentgrass", "Bermudagrass", "Tall Fescue", "Kentucky Bluegrass")),
      sliderInput("mowHeight", "Mow Height (Millimeters)", min = 1, max = 12, value = 3),
      sliderInput("mowInterval", "Mow Interval (days)", min = 1, max = 7, value = 3),
      sliderInput("irrigationFrequency", "Irrigation Frequency (days)", min = 1, max = 7, value = 3),
      sliderInput("irrigationQuantity", "Irrigation Quantity (inches)", min = 0.1, max = 1.0, step = 0.1, value = 0.5),
      actionButton("simulate", "Simulate"),
      checkboxInput("includeWeather", "Include Weather Data", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("carbonPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$simulate, {
    # Generate monthly data for three years
    start_date <- as.Date("2023/01/01")
    end_date <- as.Date("2026/01/01")
    dates <- seq(start_date, end_date, by = "week")
    
    carbon_data <- data.frame(Date = dates, CarbonStock = rep(0, length(dates)))
    
    # Set starting carbon stock based on grass species
    species_base_carbon <- switch(input$grassSpecies,
                                  "Creeping Bentgrass" = 100,
                                  "Bermudagrass" = 120,
                                  "Tall Fescue" = 110,
                                  "Kentucky Bluegrass" = 105)
    
    previous_year_carbon <- species_base_carbon
    
    for (i in 1:length(dates)) {
      month <- as.numeric(format(dates[i], "%m"))
      year_number <- as.numeric(format(dates[i], "%Y")) - 2022  # Year number since start
      year_factor <- (1 + 0.1) ^ (year_number - 1)  # 10% increase each year
      
      # Growth factors based on user inputs
      mow_height_factor <- 1 - (input$mowHeight - 1) * 0.02
      mow_interval_factor <- 1 - (input$mowInterval - 1) * 0.01
      irrigation_frequency_factor <- 1 - (7 - input$irrigationFrequency) * 0.015
      irrigation_quantity_factor <- -4 * (input$irrigationQuantity - 0.5)^2 + 1
      
      # Calculate growth or decay factor
      if (month >= 3 && month <= 10) {  # Growing season
        growth_factor <- runif(1, min = 1.01, max = 1.05) * mow_height_factor * mow_interval_factor * irrigation_frequency_factor * irrigation_quantity_factor
        carbon_data$CarbonStock[i] <- species_base_carbon * growth_factor * year_factor
      } else {  # Dormant season
        decay_factor <- runif(1, min = 0.99, max = 1.00)
        carbon_data$CarbonStock[i] <- previous_year_carbon * decay_factor
      }
      previous_year_carbon <- carbon_data$CarbonStock[i]  # Update for next iteration
    }
    
    # Incorporate weather data
    if (input$includeWeather) {
      carbon_data$WeatherEffect <- runif(length(dates), min = 0.8, max = 1.2)
      carbon_data$AdjustedCarbon <- carbon_data$CarbonStock * carbon_data$WeatherEffect
    }
    
    # Plotting the data
    output$carbonPlot <- renderPlotly({
      p <- plot_ly(carbon_data, x = ~Date, y = ~CarbonStock, type = 'scatter', mode = 'lines+markers', name = "Carbon Stock")
      if (input$includeWeather) {
        p <- add_trace(p, x = ~Date, y = ~AdjustedCarbon, type = 'scatter', mode = 'lines+markers', name = "Adjusted for Weather")
      }
      p
    })
  })
}


                         

shinyApp(ui, server)

