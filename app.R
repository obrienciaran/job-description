#Checks if the required packages are installed. If not, installs them.
packages = c("shiny","tidyverse","DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Load libraries
library(shiny)
library(tidyverse)
library(DT)

# Reading the main_data which the shiny app depends on.
main_data <- read_csv("dummyData.csv")

ui <- fluidPage(
  fluidRow(column(12, tags$h2("Assignment Details"))),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(
        align = "center",
        tags$img(src = "logo.png", width = "200", height = "97.8")
      ),
      fluidRow(
        column(12, align = "center", tags$br(), tags$b("Filter data")),
        column(12, selectInput("field_filter", "Field", unique(main_data$Field), multiple = TRUE)),
        column(12, selectInput("client_filter", "Client", unique(main_data$`Client Name`), multiple = TRUE)),
        column(12, selectInput("service_filter", "Service", unique(main_data$Service), multiple = TRUE)),
        column(12, selectInput("cost_filter", "Cost", unique(main_data$`Cost (Ex-Vat)`), multiple = TRUE)),
        column(12, align = "center", actionLink("reset_filters", "Clear Filters/Reset", style = "color: #008000"))
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Assignment Description",
          uiOutput("assignment_description")
        ),
        tabPanel(
          "Data Table",
          DTOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Creating a new empty tibble (which is basically a data.frame) for filtering based on the filters selected
  filtered_data <- tibble()
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "field_filter", selected = "")
    updateSelectInput(session, "client_filter", selected = "")
    updateSelectInput(session, "service_filter", selected = "")
    updateSelectInput(session, "cost_filter", selected = "")
  })
  # The observe code block will be triggered everytime any reactive object from the UI is changed (In this case our filters)
  observe({
    # If all the inputs are empty, We will just send the whole data without the filters. Else we filter
    field_filter_values <- input$field_filter
    client_filter_values <- input$client_filter
    service_filter_values <- input$service_filter
    cost_filter_values <- input$cost_filter
    if (is.null(input$field_filter)) {
      field_filter_values <- unique(main_data$Field)
    }
    if (is.null(input$client_filter)) {
      client_filter_values <- unique(main_data$`Client Name`)
    }
    if (is.null(input$service_filter)) {
      service_filter_values <- unique(main_data$Service)
    }
    if (is.null(input$cost_filter)) {
      cost_filter_values <- unique(main_data$`Cost (Ex-Vat)`)
    }

    filtered_data <<- main_data %>%
      filter(Field %in% field_filter_values, `Client Name` %in% client_filter_values,
          Service %in% service_filter_values, `Cost (Ex-Vat)` %in% cost_filter_values)
    # This is where the assignment description will be rendered
    output$assignment_description <- renderUI({
      filtered_data$title <- paste0(filtered_data$`Client Name`, " - ", filtered_data$`Assignment Name`)
      HTML(
        paste0(
          "<br><span style='color: #008000'>", filtered_data$title,
          "</span><br>", filtered_data$`Assignment Description`, "<br>"
        )
      )
    })
    # This is where the table is rendered. To customise the table visit here https://rstudio.github.io/DT/
    output$data_table <- renderDT({
      datatable(
        filtered_data %>% select(`Client Name`, `Assignment Name`, `Field`, `Service`, `Cost (Ex-Vat)`)
      )
    })
  })
  # Whenever a row from the table is selected the Assignment Description must change regardless of the filters selected
  observeEvent(input$data_table_rows_selected, {
    filtered_data_from_table <- filtered_data[input$data_table_rows_selected, ]
    output$assignment_description <- renderUI({
      filtered_data_from_table$title <- paste0(filtered_data_from_table$`Client Name`, " - ", filtered_data_from_table$`Assignment Name`)
      HTML(
        paste0(
          "<br><span style='color: #008000'>", filtered_data_from_table$title,
          "</span><br>", filtered_data_from_table$`Assignment Description`, "<br>"
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)