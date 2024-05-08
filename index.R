library(shiny)
library(bslib)

# Define UI
ui <- page_sidebar(
     title="Dashboard",
      sidebar=sidebar(


      fileInput("data", "Data", accept = ".xlsx"),
      fileInput("reference", "Reference", accept = ".xlsx"),
      actionButton("execute", "Execute"),

        uiOutput("type_selector") 

      ),
      card(
        card_header(textOutput("card_title")),
        plotOutput("plot")  # To display the generated plot

      )
)

# Define server logic
server <- function(input, output, session) {
    result_data <- reactiveVal(list())  #reset

  observeEvent(input$execute, {
    req(input$data, input$reference)
    
    data_path <- input$data$datapath
    reference_path <- input$reference$datapath

    tryCatch({
      source("./Rscript/disk/index.R", local = TRUE)
      result_data <- disk_data(data_path, reference_path)

      output$type_selector <- renderUI({
        selectInput("selected_type", "Select Type",
                    choices = names(result_data))
      })

      observeEvent(input$selected_type, {
    req(result_data()) #check result data is existed
    selected_data <- result_data()[[input$selected_type]]

     output$plot <- renderPlot({
        req(input$selected_type) 
        selected_data <- result_data[[input$selected_type]]
        card_type <- input$selected_type
        source("./Rscript/disk/disk_plot.R",local=TRUE)
                output$card_title <- renderText({card_type})
                output$plot <- renderPlot({
                   draw_plot(selected_data,input$selected_type)
                })
            
      })
  })
    }, error = function(e) {
      print(paste("Error in sourcing or function execution:", e$message))
    })
  });
}



# Run the application
shinyApp(ui = ui, server = server)


