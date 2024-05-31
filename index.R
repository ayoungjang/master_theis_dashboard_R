library(shiny)
library(bslib)
library(shinycssloaders)
options(shiny.host='0.0.0.0')
options(shiny.port=3388)

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
    test_type <- input$test_type # Capture the selected test type

    tryCatch(
      {
        source(paste0("./Rscript/", tolower(test_type), "/index.R"), local = TRUE)
        result_data(get_data(data_path, reference_path)) # Assuming disk_data function is appropriate for both or is dynamically defined based on source

        output$type_selector <- renderUI({
          checkboxGroupInput("selected_types", "Select Types",
            choices = names(result_data())
          )
        })
      },
      error = function(e) {
        print(paste("Error in sourcing or function execution:", e$message))
      }
    )
  })

  output$plot_cards <- renderUI({
    req(result_data())
    selected_types <- input$selected_types
    print(selected_types)
    if (is.null(selected_types) || length(selected_types) == 0) {
      return(HTML("<p>Please select at least one type to display the plots.</p>")) # Message to prompt selection
    }

    plot_card_list <- lapply(selected_types, function(type) {
      card_id <- paste("card_plot_", type, sep = "")
      card(
        title = paste("Data Plot for", type),
        plotOutput(outputId = card_id)
      )
    })

    do.call(tagList, plot_card_list) # Combine all cards into a tag list
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
