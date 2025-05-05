
#data <- readRDS("all_data_wem.rds")

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Population Density, Energy Service, and GDP Over Years by Top 20 Countries"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Year Range:",
                  min = 1960, max = 2023, value = c(1960, 2023))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Population Density", plotlyOutput("popDensityPlot")),
        tabPanel("Energy Service", plotlyOutput("energyServicePlot")),
        tabPanel("GDP_pcap", plotlyOutput("gdpPlot")),
        tabPanel("GINI", plotlyOutput("giniPlot")),
        tabPanel("GDP_pcap_ES", plotlyOutput("gdpESplot"))
      )
    )
  )
)

server <- function(input, output) {
  # Filter data based on selected year range
  filtered_data <- reactive({
    all_data_wem %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2] & country_id <= 10)
  })
  
  # Population Density Plot
  output$popDensityPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = year, y = EN.POP.DNST, colour = country_name)) +
      geom_line() +
      labs(title = "Population Density Over Years by Top 20 Countries",
           x = "Year",
           y = "Population Density",
           colour = "Country") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Energy Service Plot
  output$energyServicePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = year, y = energy_service, colour = country_name)) +
      geom_line() +
      labs(title = "Energy Service Over Years by Top 20 Countries",
           x = "Year",
           y = "Energy Service",
           colour = "Country") +
      theme_minimal()
    ggplotly(p)
  })
  
  # GDP Plot
  output$gdpPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = year, y = GDP_PPP_pcap, colour = country_name)) +
      geom_line() +
      labs(title = "GDP Over Years by Top 20 Countries",
           x = "Year",
           y = "GDP_pcap",
           colour = "Country") +
      theme_minimal()
    ggplotly(p)
  })
  
  
  
  # GINI Plot
  output$giniPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = year, y = SI.POV.GINI, colour = country_name)) +
      geom_line() +
      labs(title = "GINI Over Years by Top 20 Countries",
           x = "Year",
           y = "GINI Coefficient",
           colour = "Country") +
      theme_minimal()
    ggplotly(p)
  })
  
  # # GDP/cap vs. Energy Service
  # 
  # output$gdpESPlot <- renderPlotly({
  #   p <- ggplot(filtered_data(), aes(x = GDP_PPP_pcap, y = energy_service, colour = country_name)) +
  #     geom_line() +
  #     labs(title = "Energy Service vs GDP per Capita",
  #          x = "GDP_pcap",
  #          y = "Energy Service",
  #          colour = "Country") +
  #     theme_minimal()
  #   ggplotly(p)
  # })
}

shinyApp(ui = ui, server = server)
