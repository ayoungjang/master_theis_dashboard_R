library(shiny)
library(bslib)
library(tictoc)
options(shiny.port = 3388)

# Define UI
ui <- page_sidebar(
  title = "Dashboard",
  sidebar = sidebar(
    selectInput("test_type", "Select Test Type", choices = c("strip", "disk")),
    fileInput("data", "Data", accept = ".xlsx"),
    fileInput("reference", "Reference", accept = ".xlsx"),
    actionButton("execute", "Execute"),
    uiOutput("type_selector") # For checkbox input to select types
  ),
  mainPanel(
    tags$style(HTML("
      .card {
        height: 500px;
      }
    ")),
    uiOutput("plot_cards")
  )
)

# Define server logic
server <- function(input, output, session) {
  result_data <- reactiveVal(list())

  observeEvent(input$execute, {
    req(input$data, input$reference)
    data_path <- input$data$datapath
    reference_path <- input$reference$datapath
    test_type <- input$test_type

    tryCatch(
      {
        # Initialize cumulative execution time
        cumulative_time <- 0

        # Source the R script dynamically based on test type
        source(paste0("./Rscript/", tolower(test_type), "/index.R"), local = TRUE)
        results <- get_data(data_path, reference_path)

        lapply(names(results), function(type) {
          cat("\n+--------------------+\n")
          cat("| Processing Type:   |\n")
          cat("|", sprintf("%-18s", type), "|\n")
          cat("+--------------------+\n")

          # Time each type
          start_time <- Sys.time()
          source(paste0("./Rscript/", tolower(test_type), "/", tolower(test_type), "_plot.R"), local = TRUE)
          draw_plot(results[[type]], type)
          end_time <- Sys.time()

          execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
          cumulative_time <<- cumulative_time + execution_time

          # Print execution time for the current type
          cat("+-------------------+-----------------+\n")
          cat("| Metric           | Value           |\n")
          cat("+-------------------+-----------------+\n")
          cat(sprintf("| %-17s | %-15s |\n", "Execution Time", sprintf("%.2f seconds", execution_time)))
          cat("+-------------------+-----------------+\n")
        })

        # Print cumulative execution time after all types are processed
        cat("+-------------------+-----------------+\n")
        cat("| Metric           | Value           |\n")
        cat("+-------------------+-----------------+\n")
        cat(sprintf("| %-17s | %-15s |\n", "Execution Time", sprintf("%.2f seconds", cumulative_time)))
        # cat(sprintf("| %-26s |\n", sprintf("%.2f seconds", cumulative_time)))
        cat("+-------------------+-----------------+\n")

        result_data(results)

        output$type_selector <- renderUI({
          checkboxGroupInput("selected_types", "Select Types",
            choices = names(result_data())
          )
        })
      },
      error = function(e) {
        cat("Error in sourcing or function execution:", e$message, "\n")
      }
    )
  })

  output$plot_cards <- renderUI({
    req(result_data())
    selected_types <- input$selected_types
    if (is.null(selected_types) || length(selected_types) == 0) {
      return(HTML("<p>Please select at least one type to display the plots.</p>"))
    }

    plot_card_list <- lapply(selected_types, function(type) {
      card_id <- paste("card_plot_", type, sep = "")
      card(
        title = paste("Data Plot for", type),
        plotOutput(outputId = card_id)
      )
    })

    do.call(tagList, plot_card_list)
  })

  observe({
    req(input$selected_types)
    lapply(input$selected_types, function(type) {
      output[[paste("card_plot_", type, sep = "")]] <- renderPlot({
        req(result_data()[[type]])
        selected_data <- result_data()[[type]]
        source(paste0("./Rscript/", tolower(input$test_type), "/", tolower(input$test_type), "_plot.R"), local = TRUE)
        draw_plot(selected_data, type)
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
