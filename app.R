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

# Numeric variables available for exploration
numeric_exploration_choices <- c(
  "Price (AUD)" = "Price",
  "Distance from city centre (km)" = "Distance",
  "Number of rooms" = "Rooms",
  "Building area (square metres)" = "BuildingArea",
  "Number of bathrooms" = "Bathroom",
  "Number of car spaces" = "Car",
  "Land size (square metres)" = "Landsize"
)

# Readable label for a numeric variable
numeric_label <- function(variable) {
  names(numeric_exploration_choices)[
    match(variable, numeric_exploration_choices)
  ]
}
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
          
          h3("Melbourne Housing Explorer"),
          
          p(
            "This app lets users explore Melbourne housing records ",
            "and compare prices, locations, and property characteristics."
          ),
          
          tags$img(
            src = paste0(
              "https://upload.wikimedia.org/wikipedia/commons/",
              "b/be/Melbourne_Yarra_River.jpg"
            ),
            alt = "Melbourne city skyline and the Yarra River",
            style = paste(
              "width: 100%;",
              "max-width: 850px;",
              "height: auto;",
              "border-radius: 6px;"
            )
          ),
          
          p(
            tags$small(
              "Photo: ",
              tags$a(
                "Donaldytong / Wikimedia Commons",
                href = paste0(
                  "https://commons.wikimedia.org/wiki/",
                  "File:Melbourne_Yarra_River.jpg"
                ),
                target = "_blank",
                rel = "noopener"
              ),
              ", ",
              tags$a(
                "CC BY-SA 3.0",
                href = "https://creativecommons.org/licenses/by-sa/3.0/",
                target = "_blank",
                rel = "noopener"
              ),
              ". Displayed without cropping."
            )
          ),
          
          h4("About the data"),
          
          p(
            "The data come from Anthony Pino's Melbourne Housing Market ",
            "dataset on Kaggle. This app uses Melbourne_housing_FULL.csv, ",
            "which contains 34,857 records and 21 original variables."
          ),
          
          p(
            "The variables describe property prices, types, rooms, ",
            "building and land areas, regions, and distance from the ",
            "city centre. Prices are reported in Australian dollars."
          ),
          
          p(
            tags$a(
              "View the dataset and its documentation on Kaggle",
              href = paste0(
                "https://www.kaggle.com/datasets/",
                "anthonypino/melbourne-housing-market"
              ),
              target = "_blank",
              rel = "noopener"
            )
          ),
          
          h4("How to use the app"),
          
          tags$ul(
            tags$li(
              tags$strong("Sidebar filters: "),
              "Select a property type, region, and up to two numeric ranges. ",
              "Choose All to include every category. Click Apply Filters ",
              "to update the selected records."
            ),
            
            tags$li(
              tags$strong("Data Download: "),
              "View the selected records and download them as a CSV file."
            ),
            
            tags$li(
              tags$strong("Categorical exploration: "),
              "Choose one or two categorical variables to create ",
              "contingency tables and bar charts."
            ),
            
            tags$li(
              tags$strong("Numeric exploration: "),
              "Calculate summary statistics overall or by category. ",
              "Create histograms, box plots, scatter plots, and heatmaps, ",
              "with options for coloring and faceting."
            )
          ),
          
          p(
            "Exploration controls update the summaries and plots using ",
            "the currently applied subset. Changes to sidebar selections ",
            "take effect only after you click Apply Filters."
          ),
          
          h4("Missing values and interpretation"),
          
          p(
            "Each summary or plot excludes records missing the variables ",
            "it uses. Nonpositive prices and building areas are treated ",
            "as missing. Logarithmic scales use only positive values."
          ),
          
          p(
            "Some groups contain few records, and some measurements ",
            "include unusually large or small values. These results ",
            "describe the available records and do not establish ",
            "cause-and-effect relationships."
          )
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
              
              selectInput(
                "summary_var",
                "Numeric variable",
                choices = numeric_exploration_choices,
                selected = "Price"
              ),
              
              selectInput(
                "summary_group",
                "Summarize by",
                choices = c("Overall" = "", categorical_choices),
                selected = "property_type"
              ),
              
              p(
                "Summaries use available values in the applied subset. ",
                "Observed n may differ between variables."
              ),
              
              DTOutput("numeric_summary"),
              
              hr(),
              
              h3("Numeric plots"),
              
              p(
                "These controls use the data selected in the sidebar. ",
                "To change the records included, adjust the sidebar and ",
                "click Apply Filters."
              ),
              
              selectInput(
                "numeric_plot_type",
                "Plot type",
                choices = c(
                  "Histogram" = "histogram",
                  "Box plot by category" = "boxplot",
                  "Scatter plot" = "scatter",
                  "Heatmap of medians" = "heatmap"
                )
              ),
              
              selectInput(
                "plot_y",
                "Numeric variable to plot or summarize",
                choices = numeric_exploration_choices,
                selected = "Price"
              ),
              
              conditionalPanel(
                condition = "input.numeric_plot_type == 'scatter'",
                
                selectInput(
                  "plot_x",
                  "Horizontal-axis variable",
                  choices = numeric_exploration_choices,
                  selected = "Distance"
                ),
                
                selectInput(
                  "scatter_color",
                  "Color points by",
                  choices = c(
                    "No coloring" = "",
                    categorical_choices,
                    numeric_exploration_choices
                  ),
                  selected = "property_type"
                )
              ),
              
              conditionalPanel(
                condition = "input.numeric_plot_type == 'boxplot'",
                
                selectInput(
                  "box_group",
                  "Category for box plots",
                  choices = categorical_choices,
                  selected = "property_type"
                )
              ),
              
              conditionalPanel(
                condition = "input.numeric_plot_type == 'heatmap'",
                
                selectInput(
                  "heatmap_x",
                  "Heatmap columns",
                  choices = categorical_choices,
                  selected = "property_type"
                ),
                
                selectInput(
                  "heatmap_y",
                  "Heatmap rows",
                  choices = categorical_choices,
                  selected = "region"
                ),
                
                p(
                  "Each tile shows the median of the numeric variable ",
                  "and the number of records used."
                )
              ),
              
              conditionalPanel(
                condition = "input.numeric_plot_type != 'heatmap'",
                
                selectInput(
                  "plot_facet",
                  "Facet by",
                  choices = c("No faceting" = "", categorical_choices),
                  selected = ""
                ),
                
                checkboxInput(
                  "log_numeric",
                  "Use a logarithmic scale for the numeric variable",
                  value = FALSE
                )
              ),
              
              plotOutput(
                "numeric_plot",
                height = "650px"
              ),
              
              p(
                "Missing values are excluded for the variables used in each plot. ",
                "Logarithmic scales use only positive values. ",
                "Heatmap medians based on few records should be interpreted cautiously."
              )
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
  #Numerical summaries: overall or by a category
  output$numeric_summary <- renderDT({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the applied filters."
      )
    )
    
    variable <- input$summary_var
    
    data <- data |>
      filter(is.finite(.data[[variable]]))
    
    validate(
      need(
        nrow(data) > 0,
        "No observed values are available for this variable."
      )
    )
    
    if (input$summary_group == "") {
      
      result <- data |>
        summarise(
          `Observed n` = n(),
          Mean = mean(.data[[variable]]),
          Median = median(.data[[variable]]),
          SD = sd(.data[[variable]]),
          Minimum = min(.data[[variable]]),
          Maximum = max(.data[[variable]])
        )
      
    } else {
      
      group_var <- input$summary_group
      
      data <- data |>
        filter(!is.na(.data[[group_var]]))
      
      validate(
        need(
          nrow(data) > 0,
          "No complete records are available for this grouping variable."
        )
      )
      
      result <- data |>
        group_by(.data[[group_var]]) |>
        summarise(
          `Observed n` = n(),
          Mean = mean(.data[[variable]]),
          Median = median(.data[[variable]]),
          SD = sd(.data[[variable]]),
          Minimum = min(.data[[variable]]),
          Maximum = max(.data[[variable]]),
          .groups = "drop"
        )
      
      names(result)[1] <- categorical_label(group_var)
    }
    
    #Round summary statistics while preserving the count
    result <- result |>
      mutate(
        across(
          any_of(c("Mean", "Median", "SD", "Minimum", "Maximum")),
          ~ round(.x, 1)
        )
      )
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  
  #Heatmap, histogram, box plot, and scatter plot
  output$numeric_plot <- renderPlot({
    
    data <- filtered_data()
    
    req(input$numeric_plot_type, input$plot_y)
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the applied filters."
      )
    )
    
    y_var <- input$plot_y
    plot_type <- input$numeric_plot_type
    
    data <- data |>
      filter(is.finite(.data[[y_var]]))
    
    validate(
      need(
        nrow(data) > 0,
        "No observed values are available for this numeric variable."
      )
    )
    
    #Heatmap
    if (plot_type == "heatmap") {
      
      req(input$heatmap_x, input$heatmap_y)
      
      x_group <- input$heatmap_x
      y_group <- input$heatmap_y
      
      validate(
        need(
          x_group != y_group,
          "Choose two different categorical variables for the heatmap."
        )
      )
      
      heatmap_data <- data |>
        filter(
          !is.na(.data[[x_group]]),
          !is.na(.data[[y_group]])
        ) |>
        group_by(
          .data[[x_group]],
          .data[[y_group]]
        ) |>
        summarise(
          median_value = median(.data[[y_var]]),
          n = n(),
          .groups = "drop"
        ) |>
        mutate(
          tile_label = paste0(
            scales::number(
              median_value,
              accuracy = 0.1,
              big.mark = ","
            ),
            "\nn = ",
            scales::comma(n)
          )
        )
      
      validate(
        need(
          nrow(heatmap_data) > 0,
          "No complete records are available for this heatmap."
        )
      )
      
      plot <- ggplot(
        heatmap_data,
        aes(
          x = .data[[x_group]],
          y = .data[[y_group]],
          fill = median_value
        )
      ) +
        geom_tile(
          color = "white",
          linewidth = 0.5
        ) +
        geom_text(
          aes(label = tile_label),
          size = 3
        ) +
        scale_fill_gradient(
          low = "#F7FBFF",
          high = "#6BAED6",
          labels = scales::label_comma()
        ) +
        scale_x_discrete(
          labels = function(x) stringr::str_wrap(x, width = 18)
        ) +
        scale_y_discrete(
          labels = function(x) stringr::str_wrap(x, width = 22)
        ) +
        labs(
          title = paste("Median", numeric_label(y_var)),
          subtitle = "Tiles show the median and number of observed values",
          x = categorical_label(x_group),
          y = categorical_label(y_group),
          fill = paste("Median", numeric_label(y_var))
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          )
        )
      
      return(plot)
    }
    
    #Settings for histogram, box plot, and scatter plot
    use_log <- isTRUE(input$log_numeric)
    
    if (use_log) {
      data <- data |>
        filter(.data[[y_var]] > 0)
    }
    
    facet_var <- input$plot_facet
    
    if (!is.null(facet_var) && facet_var != "") {
      data <- data |>
        filter(!is.na(.data[[facet_var]]))
    }
    
    validate(
      need(
        nrow(data) > 0,
        "No suitable values remain. Try another variable or turn off the log scale."
      )
    )
    
    #Histogram
    if (plot_type == "histogram") {
      
      plot <- ggplot(
        data,
        aes(x = .data[[y_var]])
      ) +
        geom_histogram(
          bins = 30,
          fill = "#0072B2",
          color = "white"
        ) +
        labs(
          title = paste("Distribution of", numeric_label(y_var)),
          x = numeric_label(y_var),
          y = "Number of records"
        )
      
      if (use_log) {
        plot <- plot +
          scale_x_log10(labels = scales::label_comma()) +
          labs(
            x = paste(numeric_label(y_var), "(log scale)")
          )
      } else {
        plot <- plot +
          scale_x_continuous(labels = scales::label_comma())
      }
      
      #Box plot
    } else if (plot_type == "boxplot") {
      
      req(input$box_group)
      
      data <- data |>
        filter(!is.na(.data[[input$box_group]]))
      
      validate(
        need(
          nrow(data) > 0,
          "No complete records are available for this box plot."
        )
      )
      
      plot <- ggplot(
        data,
        aes(
          x = .data[[input$box_group]],
          y = .data[[y_var]],
          fill = .data[[input$box_group]]
        )
      ) +
        geom_boxplot(outlier.alpha = 0.25) +
        scale_x_discrete(
          labels = function(x) stringr::str_wrap(x, width = 16)
        ) +
        labs(
          title = paste(
            numeric_label(y_var),
            "by",
            categorical_label(input$box_group)
          ),
          x = categorical_label(input$box_group),
          y = numeric_label(y_var)
        ) +
        guides(fill = "none")
      
      if (use_log) {
        plot <- plot +
          scale_y_log10(labels = scales::label_comma()) +
          labs(
            y = paste(numeric_label(y_var), "(log scale)")
          )
      } else {
        plot <- plot +
          scale_y_continuous(labels = scales::label_comma())
      }
      
      #Scatter plot
    } else {
      
      req(input$plot_x)
      
      x_var <- input$plot_x
      color_var <- input$scatter_color
      
      validate(
        need(
          x_var != y_var,
          "Choose two different numeric variables."
        )
      )
      
      data <- data |>
        filter(is.finite(.data[[x_var]]))
      
      #Complete values for the selected color variable
      if (!is.null(color_var) && color_var != "") {
        
        if (is.numeric(data[[color_var]])) {
          data <- data |>
            filter(is.finite(.data[[color_var]]))
        } else {
          data <- data |>
            filter(!is.na(.data[[color_var]]))
        }
      }
      
      validate(
        need(
          nrow(data) > 0,
          "No complete records are available for these variables."
        )
      )
      
      plot <- ggplot(
        data,
        aes(
          x = .data[[x_var]],
          y = .data[[y_var]]
        )
      )
      
      if (is.null(color_var) || color_var == "") {
        
        plot <- plot +
          geom_point(
            color = "#0072B2",
            alpha = 0.3,
            size = 1
          )
        
      } else {
        
        plot <- plot +
          geom_point(
            aes(color = .data[[color_var]]),
            alpha = 0.3,
            size = 1
          )
        
        if (is.numeric(data[[color_var]])) {
          
          plot <- plot +
            scale_color_viridis_c() +
            labs(color = numeric_label(color_var))
          
        } else {
          
          plot <- plot +
            labs(color = categorical_label(color_var))
        }
      }
      
      plot <- plot +
        scale_x_continuous(labels = scales::label_comma()) +
        labs(
          title = paste(
            numeric_label(y_var),
            "and",
            numeric_label(x_var)
          ),
          x = numeric_label(x_var),
          y = numeric_label(y_var)
        )
      
      if (use_log) {
        plot <- plot +
          scale_y_log10(labels = scales::label_comma()) +
          labs(
            y = paste(numeric_label(y_var), "(log scale)")
          )
      } else {
        plot <- plot +
          scale_y_continuous(labels = scales::label_comma())
      }
    }
    
    #User-selected faceting
    if (!is.null(facet_var) && facet_var != "") {
      plot <- plot +
        facet_wrap(
          vars(.data[[facet_var]]),
          labeller = label_wrap_gen(width = 20)
        )
    }
    
    plot +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui = ui, server = server)