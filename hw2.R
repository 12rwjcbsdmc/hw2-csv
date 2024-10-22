---
  title: "436 hw 2"
output:
  html_document: default
pdf_document: default
date: "2024-10-17"
runtime: shiny
---
```{r}
knitr::opts_chunk$set(warnings = FALSE, message = FALSE)
```

```{r}
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)

df <- read.csv('https://raw.githubusercontent.com/tommy04132002/0041/main/NY-House-Dataset.csv', stringsAsFactors = FALSE)

df$PRICE <- as.numeric(gsub("[^0-9]", "", df$PRICE))

df$STATE <- sapply(strsplit(df$LOCALITY, ","), function(x) trimws(x[1]))
unique_states <- unique(df$STATE)

data_preprocess_price_by_region <- function(data, regions) {
  data |>
    filter(STATE %in% regions) |>
    group_by(STATE) |>
    summarise(
      avg_price = round(mean(PRICE, na.rm = TRUE), 2),
      num_houses = n()
    ) |>
    filter(!is.na(avg_price) & !is.infinite(avg_price))
}

data_preprocess_house_type <- function(data, house_type) {
  data |>
    filter(TYPE == house_type) |>
    filter(!is.na(LATITUDE) & !is.na(LONGITUDE))
}

ui <- fluidPage(
  titlePanel("House Price Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Average House Price by Region"),
      checkboxGroupInput("regions", "Select Regions:", 
                         choices = unique_states),
      hr(),
      
      h4("House Type Distribution and Map"),
      selectInput("houseType", "Select House Type:", 
                  choices = unique(df$TYPE)),
      hr(),
      
      h5("Instructions:"),
      p("Use the checkboxes to select regions for the average house price analysis."),
      p("Select the house type to view its distribution and map information.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Average Price Bar Plot", 
                 plotOutput("avgPriceBarPlot"), 
                 dataTableOutput("avgPriceTable")),
        tabPanel("House Status Map", 
                 leafletOutput("houseTypeMap"),
                 dataTableOutput("houseTypeTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_avg_price_data <- reactive({
    data_preprocess_price_by_region(df, input$regions)
  })
  
  output$avgPriceBarPlot <- renderPlot({
    data <- filtered_avg_price_data()
    if (nrow(data) == 0) {
      return()
    }
    
    ggplot(data, aes(x = reorder(STATE, -avg_price), y = avg_price, fill = num_houses)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Houses") +
      labs(title = "Average House Price by Region", 
           x = "Region", y = "Average Price") +
      coord_cartesian(ylim = c(0, max(data$avg_price)))
  })
  
  output$avgPriceTable <- renderDataTable({
    filtered_avg_price_data()
  })
  
  filtered_house_type_data <- reactive({
    data_preprocess_house_type(df, input$houseType)
  })
  
  output$houseTypeTable <- renderDataTable({
    filtered_house_type_data() |>
      select(ADDRESS, TYPE, PRICE, STATE, BATH, BEDS, PROPERTYSQFT)
  })
  
  output$houseTypeMap <- renderLeaflet({
    data <- filtered_house_type_data()
    if (nrow(data) == 0) {
      return()
    }
    
    leaflet(data) |>
      addTiles() |>
      addCircleMarkers(
        ~LONGITUDE, ~LATITUDE,
        radius = 5, # Set point size
        color ="red",
        fillColor ="red",
        fillOpacity = ~scales::rescale(PRICE, to = c(0.5, 1)),
        stroke = FALSE, 
        popup = ~paste("House Type:", TYPE, 
                       "Price: $", PRICE, 
                       "Address:", ADDRESS, 
                       "Baths:", BATH, 
                       "Beds:", BEDS,
                       "Property SqFt:", PROPERTYSQFT)
      )
  })
}

shinyApp(ui, server)
```