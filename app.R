library(shiny)
library(tidyverse)
library(DT)

#Load the data
housing_raw <- read_csv(
  "data/Melbourne_housing_FULL.csv",
  show_col_types = FALSE
)

#Prepare variables in the same way as the exploration
housing <- housing_raw |>
  mutate(
    property_type = factor(
      Type,
      levels = c("h", "t", "u"),
      labels = c("House", "Townhouse", "Unit/apartment")
    ),
    region = factor(
      na_if(na_if(str_trim(Regionname), ""), "#N/A")
    ),
    Price = if_else(Price > 0, Price, NA_real_),
    BuildingArea = if_else(BuildingArea > 0, BuildingArea, NA_real_)
  )

#Numeric variables available for the optional filters
numeric_choices <- c(
  "No numeric filter" = "",
  "Price (AUD)" = "Price",
  "Distance from city centre (km)" = "Distance",
  "Number of rooms" = "Rooms",
  "Building area (square metres)" = "BuildingArea"
)

property_types <- levels(housing$property_type)
regions <- levels(housing$region)

ui <- fluidPage(
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filter the data"),
      
      selectInput(
        "type",
        "Property type",
        choices = c("All property types" = "All", property_types),
        selected = "All"
      ),
      
      selectInput(
        "region",
        "Region",
        choices = c("All regions" = "All", regions),
        selected = "All"
      ),
      
      selectInput(
        "numeric_var1",
        "First numeric variable",
        choices = numeric_choices,
        selected = ""
      ),
      
      uiOutput("numeric_slider1"),
      
      selectInput(
        "numeric_var2",
        "Second numeric variable",
        choices = numeric_choices,
        selected = ""
      ),
      
      uiOutput("numeric_slider2"),
      
      actionButton("apply", "Apply Filters"),
      
      br(), br(),
      
      helpText(
        "Choose your filters and click Apply Filters to update the data."
      ),
      
      helpText(
        "Selecting a specific category or numeric range excludes records ",
        "missing that variable."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          h3("About this app"),
          p(
            "Explore Melbourne housing records by property type, region, ",
            "and property characteristics."
          ),
          p(
            "Use the sidebar to select filters, then click Apply Filters. ",
            "The Data Download tab displays the selected records and lets ",
            "you download them. The Data Exploration tab will provide ",
            "tables and graphs."
          ),
          p(
            "Data source and an image will be added here."
          )
        ),
        
        tabPanel(
          "Data Download",
          h3("Selected housing records"),
          textOutput("record_count"),
          br(),
          downloadButton("download_data", "Download CSV"),
          br(), br(),
          DTOutput("housing_table")
        ),
        
        tabPanel(
          "Data Exploration",
          h3("Explore the data"),
          p("Numerical summaries and plots will be added here.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Create a range slider for the first selected numeric variable
  output$numeric_slider1 <- renderUI({
    req(input$numeric_var1 != "")
    
    values <- housing[[input$numeric_var1]]
    values <- values[is.finite(values)]
    req(length(values) > 0)
    
    limits <- range(values)
    
    if (limits[1] == limits[2]) {
      return(helpText("This variable has only one observed value."))
    }
    
    sliderInput(
      "range1",
      "Select the first range",
      min = limits[1],
      max = limits[2],
      value = limits
    )
  })
  
  #Create a range slider for the second selected numeric variable
  output$numeric_slider2 <- renderUI({
    req(input$numeric_var2 != "")
    
    values <- housing[[input$numeric_var2]]
    values <- values[is.finite(values)]
    req(length(values) > 0)
    
    limits <- range(values)
    
    if (limits[1] == limits[2]) {
      return(helpText("This variable has only one observed value."))
    }
    
    sliderInput(
      "range2",
      "Select the second range",
      min = limits[1],
      max = limits[2],
      value = limits
    )
  })
  
  #Update the selected data only when Apply Filters is pressed.
  #The full dataset is displayed when the app first opens.
  filtered_data <- eventReactive(input$apply, {
    data <- housing
    
    if (input$type != "All") {
      data <- data |>
        filter(property_type == input$type)
    }
    
    if (input$region != "All") {
      data <- data |>
        filter(region == input$region)
    }
    
    if (input$numeric_var1 != "") {
      req(input$range1)
      
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var1]]),
          between(
            .data[[input$numeric_var1]],
            input$range1[1],
            input$range1[2]
          )
        )
    }
    
    if (input$numeric_var2 != "") {
      req(input$range2)
      
      data <- data |>
        filter(
          !is.na(.data[[input$numeric_var2]]),
          between(
            .data[[input$numeric_var2]],
            input$range2[1],
            input$range2[2]
          )
        )
    }
    
    data
  }, ignoreNULL = FALSE)
  
  output$record_count <- renderText({
    paste(
      "Number of selected records:",
      format(nrow(filtered_data()), big.mark = ",")
    )
  })
  
  output$housing_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("melbourne_housing_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)