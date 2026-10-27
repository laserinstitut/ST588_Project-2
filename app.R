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
    BuildingArea = if_else(
      BuildingArea > 0,
      BuildingArea,
      NA_real_
    )
  )

#Numeric variables for the optional sidebar filters
numeric_choices <- c(
  "No numeric filter" = "",
  "Price (AUD)" = "Price",
  "Distance from city centre (km)" = "Distance",
  "Number of rooms" = "Rooms",
  "Building area (square metres)" = "BuildingArea"
)

#Categorical variables for the exploration tab
categorical_choices <- c(
  "Property type" = "property_type",
  "Region" = "region",
  "Sale method" = "Method",
  "Council area" = "CouncilArea"
)

#Convert variable names into readable labels
categorical_label <- function(variable) {
  labels <- c(
    property_type = "Property type",
    region = "Region",
    Method = "Sale method",
    CouncilArea = "Council area"
  )
  
  unname(labels[variable])
}

property_types <- levels(housing$property_type)
regions <- levels(housing$region)


ui <- fluidPage(
  
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Filter the data"),
      
      selectInput(
        inputId = "type",
        label = "Property type",
        choices = c(
          "All property types" = "All",
          property_types
        ),
        selected = "All"
      ),
      
      selectInput(
        inputId = "region",
        label = "Region",
        choices = c(
          "All regions" = "All",
          regions
        ),
        selected = "All"
      ),
      
      selectInput(
        inputId = "numeric_var1",
        label = "First numeric variable",
        choices = numeric_choices,
        selected = ""
      ),
      
      uiOutput("numeric_slider1"),
      
      selectInput(
        inputId = "numeric_var2",
        label = "Second numeric variable",
        choices = numeric_choices,
        selected = ""
      ),
      
      uiOutput("numeric_slider2"),
      
      actionButton(
        inputId = "apply",
        label = "Apply Filters"
      ),
      
      br(),
      br(),
      
      helpText(
        "Choose your filters and click Apply Filters to update the data."
      ),
      
      helpText(
        "Selecting a specific category or numeric range excludes ",
        "records missing that variable."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        # About tab
        tabPanel(
          "About",
          
          h3("About this app"),
          
          p(
            "Explore Melbourne housing records by property type, ",
            "region, and property characteristics."
          ),
          
          p(
            "Use the sidebar to select filters, then click Apply Filters. ",
            "The Data Download tab displays the selected records and ",
            "lets you download them. The Data Exploration tab provides ",
            "tables and graphs."
          ),
          
          p("Data source and an image will be added here.")
        ),
        
        #Data Download tab
        tabPanel(
          "Data Download",
          
          h3("Selected housing records"),
          
          textOutput("record_count"),
          
          br(),
          
          downloadButton(
            outputId = "download_data",
            label = "Download CSV"
          ),
          
          br(),
          br(),
          
          DTOutput("housing_table")
        ),
        
        #Data Exploration tab
        tabPanel(
          "Data Exploration",
          
          tabsetPanel(
            
            # Categorical subtab
            tabPanel(
              "Categorical",
              
              h3("Categorical summaries"),
              
              div(
                class = "filter-summary",
                strong("Applied filters"),
                textOutput("applied_filters"),
                textOutput("exploration_count")
              ),
              
              selectInput(
                inputId = "cat_var1",
                label = "First categorical variable",
                choices = categorical_choices,
                selected = "property_type"
              ),
              
              selectInput(
                inputId = "cat_var2",
                label = "Second categorical variable",
                choices = c(
                  "None" = "",
                  categorical_choices
                ),
                selected = ""
              ),
              
              p(
                "Choose one variable for a one-way table, or two ",
                "variables for a two-way table. Missing values are ",
                "excluded."
              ),
              
              h4("Contingency table"),
              
              DTOutput("categorical_table"),
              
              h4("Bar chart"),
              
              plotOutput(
                "categorical_plot",
                height = "500px"
              )
            ),
            
            #Numeric subtab
            tabPanel(
              "Numeric",
              
              h3("Numeric summaries"),
              
              p("Numeric summaries and plots will be added here next.")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  #Dynamic slider for the first numeric variable
  output$numeric_slider1 <- renderUI({
    
    req(input$numeric_var1 != "")
    
    variable <- input$numeric_var1
    values <- housing[[variable]]
    values <- values[is.finite(values)]
    
    req(length(values) > 0)
    
    limits <- range(values)
    
    if (limits[1] == limits[2]) {
      return(
        helpText(
          "This variable has only one observed value. ",
          "Please choose another variable."
        )
      )
    }
    
    sliderInput(
      inputId = paste0("range1_", variable),
      label = "Select the first range",
      min = limits[1],
      max = limits[2],
      value = limits
    )
  })
  
  #Dynamic slider for the second numeric variable
  output$numeric_slider2 <- renderUI({
    
    req(input$numeric_var2 != "")
    
    variable <- input$numeric_var2
    values <- housing[[variable]]
    values <- values[is.finite(values)]
    
    req(length(values) > 0)
    
    limits <- range(values)
    
    if (limits[1] == limits[2]) {
      return(
        helpText(
          "This variable has only one observed value. ",
          "Please choose another variable."
        )
      )
    }
    
    sliderInput(
      inputId = paste0("range2_", variable),
      label = "Select the second range",
      min = limits[1],
      max = limits[2],
      value = limits
    )
  })
  
  #Capture the filters and selected records when the button is clicked.
  #With ignoreNULL = FALSE, the full dataset is shown when the app opens.
  applied_selection <- eventReactive(input$apply, {
    
    data <- housing
    
    selected_type <- input$type
    selected_region <- input$region
    selected_var1 <- input$numeric_var1
    selected_var2 <- input$numeric_var2
    
    descriptions <- c(
      paste("Property type:", selected_type),
      paste("Region:", selected_region)
    )
    
    #Filter by property type
    if (selected_type != "All") {
      data <- data |>
        filter(property_type == selected_type)
    }
    
    #Filter by region
    if (selected_region != "All") {
      data <- data |>
        filter(region == selected_region)
    }
    
    #Apply the first numeric filter
    if (selected_var1 != "") {
      
      range1 <- input[[paste0("range1_", selected_var1)]]
      
      validate(
        need(
          !is.null(range1),
          "Wait for the first slider to appear, then click Apply Filters."
        )
      )
      
      data <- data |>
        filter(
          !is.na(.data[[selected_var1]]),
          between(
            .data[[selected_var1]],
            range1[1],
            range1[2]
          )
        )
      
      label1 <- names(numeric_choices)[
        match(selected_var1, numeric_choices)
      ]
      
      descriptions <- c(
        descriptions,
        paste0(
          label1, ": ",
          format(range1[1], big.mark = ","),
          " to ",
          format(range1[2], big.mark = ",")
        )
      )
    }
    
    #Apply the second numeric filter
    if (selected_var2 != "") {
      
      range2 <- input[[paste0("range2_", selected_var2)]]
      
      validate(
        need(
          !is.null(range2),
          "Wait for the second slider to appear, then click Apply Filters."
        )
      )
      
      data <- data |>
        filter(
          !is.na(.data[[selected_var2]]),
          between(
            .data[[selected_var2]],
            range2[1],
            range2[2]
          )
        )
      
      label2 <- names(numeric_choices)[
        match(selected_var2, numeric_choices)
      ]
      
      descriptions <- c(
        descriptions,
        paste0(
          label2, ": ",
          format(range2[1], big.mark = ","),
          " to ",
          format(range2[2], big.mark = ",")
        )
      )
    }
    
    list(
      data = data,
      description = paste(descriptions, collapse = " | ")
    )
    
  }, ignoreNULL = FALSE)
  
  #Make the filtered data available to all outputs
  filtered_data <- reactive({
    applied_selection()$data
  })
  
  #Display the filters that were actually applied
  output$applied_filters <- renderText({
    applied_selection()$description
  })
  
  #Display the number of matching records
  output$exploration_count <- renderText({
    
    n <- nrow(filtered_data())
    
    if (n == 0) {
      paste(
        "0 matching records.",
        "Try widening a range or choosing another region."
      )
    } else {
      paste(
        format(n, big.mark = ","),
        "matching records"
      )
    }
  })
  
  #Record count in the Data Download tab
  output$record_count <- renderText({
    
    paste(
      "Number of selected records:",
      format(nrow(filtered_data()), big.mark = ",")
    )
  })
  
  #Display the selected data
  output$housing_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  #Download the selected data
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "melbourne_housing_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  #One-way or two-way categorical table
  output$categorical_table <- renderDT({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the applied filters."
      )
    )
    
    if (input$cat_var2 == "") {
      
      # One-way table
      result <- data |>
        filter(!is.na(.data[[input$cat_var1]])) |>
        count(
          .data[[input$cat_var1]],
          name = "Count"
        )
      
      validate(
        need(
          nrow(result) > 0,
          "No nonmissing values are available."
        )
      )
      
      result <- result |>
        mutate(
          Percent = round(100 * Count / sum(Count), 1)
        )
      
    } else {
      
      validate(
        need(
          input$cat_var1 != input$cat_var2,
          "Please select two different categorical variables."
        )
      )
      
      #Two-way table
      result <- data |>
        filter(
          !is.na(.data[[input$cat_var1]]),
          !is.na(.data[[input$cat_var2]])
        ) |>
        count(
          .data[[input$cat_var1]],
          .data[[input$cat_var2]],
          name = "Count"
        ) |>
        pivot_wider(
          names_from = all_of(input$cat_var2),
          values_from = Count,
          values_fill = 0
        )
      
      validate(
        need(
          nrow(result) > 0,
          "No complete records are available."
        )
      )
    }
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  #One-way or grouped bar chart
  output$categorical_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the applied filters."
      )
    )
    
    if (input$cat_var2 == "") {
      
      data <- data |>
        filter(!is.na(.data[[input$cat_var1]]))
      
      validate(
        need(
          nrow(data) > 0,
          "No nonmissing values are available."
        )
      )
      
      ggplot(
        data,
        aes(x = .data[[input$cat_var1]])
      ) +
        geom_bar(fill = "#0072B2") +
        coord_flip() +
        labs(
          title = paste(
            "Records by",
            categorical_label(input$cat_var1)
          ),
          x = categorical_label(input$cat_var1),
          y = "Number of records"
        ) +
        theme_minimal(base_size = 12)
      
    } else {
      
      validate(
        need(
          input$cat_var1 != input$cat_var2,
          "Please select two different categorical variables."
        )
      )
      
      data <- data |>
        filter(
          !is.na(.data[[input$cat_var1]]),
          !is.na(.data[[input$cat_var2]])
        )
      
      validate(
        need(
          nrow(data) > 0,
          "No complete records are available."
        )
      )
      
      ggplot(
        data,
        aes(
          x = .data[[input$cat_var1]],
          fill = .data[[input$cat_var2]]
        )
      ) +
        geom_bar(position = "dodge") +
        coord_flip() +
        labs(
          title = paste(
            categorical_label(input$cat_var1),
            "by",
            categorical_label(input$cat_var2)
          ),
          x = categorical_label(input$cat_var1),
          y = "Number of records",
          fill = categorical_label(input$cat_var2)
        ) +
        theme_minimal(base_size = 12)
    }
  })
}

shinyApp(ui = ui, server = server)