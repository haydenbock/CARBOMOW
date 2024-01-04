library(shiny)
library(deSolve)
library(ggplot2)

# Define the differential equations model
carbon_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Differential equations representing the flow between the pools
    d_shoot <- NPP * 0.5 - shoot * (clipping_rate + thatch_rate)
    d_root <- NPP * 0.5 - root * root_decay
    d_clipping <- shoot * clipping_rate - clipping_decay * clipping
    d_thatch <- shoot * thatch_rate - thatch_decay * thatch
    d_structural_litter <- clipping * clipping_decay + thatch * thatch_decay - structural_litter * structural_decay
    d_metabolic_litter <- root * root_decay - metabolic_litter * metabolic_decay
    d_microbe <- structural_litter * structural_decay * microbe_efficiency - microbe * microbe_decay
    d_slow_som <- microbe * microbe_decay * slow_som_efficiency - slow_som * slow_som_decay
    d_passive_som <- slow_som * slow_som_decay * passive_som_efficiency - passive_som * passive_som_decay
    
    # Return the rate of change
    list(c(d_shoot, d_root, d_clipping, d_thatch, d_structural_litter, d_metabolic_litter, d_microbe, d_slow_som, d_passive_som))
  })
}

ui <- fluidPage(
  titlePanel("Carbon Pools Simulation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Sliders for NPP and decay rates of each pool
      sliderInput("NPP", "Net Primary Productivity (NPP)", min = 0, max = 1000, value = 500),
      sliderInput("clipping_rate", "Clipping Rate", min = 0, max = 1, value = 0.1),
      sliderInput("thatch_rate", "Thatch Rate", min = 0, max = 1, value = 0.1),
      sliderInput("root_decay", "Root Decay Rate", min = 0, max = 1, value = 0.1),
      # ... Add sliders for other rates ...
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("carbonPoolsPlot")
    )
  )
)

server <- function(input, output) {
  output$carbonPoolsPlot <- renderPlot({
    input$simulate
    isolate({
      # Initial state for the carbon pools
      state_initial <- c(shoot = 100, root = 100, clipping = 100, thatch = 100, 
                         structural_litter = 100, metabolic_litter = 100, 
                         microbe = 100, slow_som = 100, passive_som = 100)
      
      # Parameters (decay rates and efficiencies)
      parameters <- c(clipping_rate = input$clipping_rate, thatch_rate = input$thatch_rate,
                      root_decay = input$root_decay, NPP = input$NPP,
                      clipping_decay = 0.1, thatch_decay = 0.1, structural_decay = 0.1,
                      metabolic_decay = 0.1, microbe_efficiency = 0.5,
                      microbe_decay = 0.1, slow_som_efficiency = 0.5,
                      slow_som_decay = 0.1, passive_som_efficiency = 0.5,
                      passive_som_decay = 0.1)
      
      # Time sequence for the simulation
      times <- seq(0, 100, by = 1)  # Simulate from time 0 to 100
      
      # Running the ODE solver
      out <- ode(y = state_initial, times = times, func = carbon_model, parms = parameters)
      
      # Convert output to a data frame for ggplot
      out_df <- as.data.frame(out)
      
      # Plotting the results with ggplot2
      ggplot(data = out_df, aes(x = time)) +
        geom_line(aes(y = shoot, color = "Shoot")) +
        geom_line(aes(y = root, color = "Root")) +
        geom_line(aes(y = clipping, color = "Clipping")) +
        geom_line(aes(y = thatch, color = "Thatch")) +
        geom_line(aes(y = structural_litter, color = "Structural Litter")) +
        geom_line(aes(y = metabolic_litter, color = "Metabolic Litter")) +
        geom_line(aes(y = microbe, color = "Microbe")) +
        geom_line(aes(y = slow_som, color = "Slow SOM")) +
        geom_line(aes(y = passive_som, color = "Passive SOM")) +
        labs(y = "Carbon Stock", color = "Carbon Pools") +
        theme_minimal()
    })
  })
}

shinyApp(ui, server)
