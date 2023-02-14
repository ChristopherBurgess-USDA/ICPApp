#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)

library(writexl)

library(gt)
library(ggplot2)
library(scales)
library(cowplot)


library(shiny)
library(shinydashboard)
library(shinyjs)


app_server <- function( input, output, session ) {
  # Your application server logic 
  output$menu = renderMenu({
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload"))
    )
  })
  
  output$tabs = renderUI({
    tabItems(
      tabItem(
        tabName = "upload",
        box(
          title = h3("Upload and Choose Analysis Type"),
          selectInput(
            label = h4("Choose Analysis Type"), inputId = "method",
            choices = list(
              " " = "",
              "Mehlich" = "Mehlich",
              "Ammonium acetate" = "AmmAc",
              "Dry Ash" = "Dry Ash",
              "DTPA" = "DTPA",
              "MWD" = "MWD"
            ),
            selected = ""
          ),
          selectInput(
            label = h4("Choose Standard"), inputId = "std_type",
            choices = list(" " = ""),
            selected = ""
          ),
          fileInput(
            inputId = "upload_std",
            label = h4("Upload Standards"),
            multiple = F,
            placeholder = "Browse",
            accept = c(".xlsx")
          ),
          fileInput(
            inputId = "upload",
            label = h4("Upload ICP Data"),
            multiple = F,
            placeholder = "Browse",
            accept = c(".csv")
          )
        ),
        box(
          h3("Blank Correction?"),
          helpText("Would you like to subtract blank values from samples"),
          radioButtons(
            inputId = "blank",
            label = NULL,
            choices = list("Yes" = 1, "No" = 0),
            selected = 1
          )
        ),
      ),
      tabItem(
        tabName = "std_qc", h3("Control Plots"),
        div(style = "margin: auto; width: 80%", uiOutput("date_slider")),
        selectInput(
          label = h3("Control Plots by Element"), inputId = "std_element",
          choices = NULL, selected = NULL
        ),
          plotOutput(outputId = "std_graph", width = "90%")
      ),
      tabItem(
        tabName = "explore",
        box(gt_output(outputId = "std_tab")),
        box(
          selectInput(
            label = h3("Data Exploration by Element"), inputId = "element",
            choices = NULL, selected = NULL
          ),
          plotOutput(outputId = "sam_graph", click = "sam_click", width = "90%"),
          verbatimTextOutput(outputId = "sam_info")
        )
      ),
      tabItem(
        tabName = "download", h3("Download Data"),
        box(
          downloadButton(outputId = "download", label = "Download Data")
        ),
        box(
          downloadButton(outputId = "download_std", label = "Download Updated Standards")
        )
      )
    )
  })


  observeEvent(input$method, {
    vals <- switch(
      input$method,
      "Mehlich" = c("CAL"),
      "AmmAc" = c("CAL"),
      "Dry Ash" = c("CTFS"),
      "DTPA" = c("CAL"),
      "MWD" = c("CAL", "CTFS")
    )

    updateSelectInput(
      session = session,
      inputId = "std_type",
      choices = vals,
      selected = vals[1]
    )
  })

  import_std_data = eventReactive(input$upload_std, {
    read_excel(input$upload_std$datapath)
  })
  
  std_data = reactive({
    req(import_std_data(), input$std_type)
    import_std_data() %>%
      filter(std_type == input$std_type) %>%
      pivot_longer(
        -c(date_time, std_type),
        names_to = "element", values_to = "value"
      ) %>%
      return()
  })
  
  std_cutoff = reactive({
    req(std_data)
    std_data() %>%
      group_by(std_type, element) %>%
      summarise(
        mean = mean(value, na.rm = T),
        stderr = sd(value, na.rm = T)
      ) %>%
      mutate(
        act_low = mean-3*stderr,
        warn_low = mean-2*stderr,
        warn_high = mean+2*stderr,
        act_high = mean+2*stderr
      ) %>%
      return()
  })
  
  input_data = eventReactive(input$upload, {
    ext = tools::file_ext(input$upload$name)
    validate(need(
      ext %in% c("csv"),
      "Please upload raw ICP comma-delimateted (csv)"
    ))
    icp_load(input$upload$datapath) 
  })
  
  data = reactive({
    req(input_data(), std_cutoff())
    parsing_data(input_data(), input$blank, std_cutoff())
  })
  
  
  updated_std <- reactive({
    req(data())
    temp <- data() %>%
      select(element, std_data) %>%
      unnest(std_data) %>%
      select(date_time, std_type, element, Value) %>%
      pivot_wider(names_from = element, values_from = Value)
    
    bind_rows(import_std_data(), temp) 
  })
  
  observe({
    req(input$method, input$upload, input$upload_std)
    output$menu = renderMenu({
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Control Plots", tabName = "std_qc", icon = icon("chart-line")),
        menuItem("Data QC", icon = icon("heartbeat"), tabName = "explore")
      )
    })
  })
  
  output$date_slider <- renderUI({
    req(updated_std())
    dates <- updated_std()$date_time
    sliderInput(
      "date_range", label = h4("Select Date Range to Plot"),
      min = min(dates), max = max(dates),
      value = c(min(dates), max(dates)), width = "100%"
    )
  })
  
  observe({
    req(data())
    ele_list = data()$element
    updateSelectInput(
      session,
      inputId = "element",
      choices = ele_list,
      selected = NULL
    )
  })
  
  observe({
    req(data())
    ele_list = data()$element
    updateSelectInput(
      session,
      inputId = "std_element",
      choices = ele_list,
      selected = NULL
    )
  })
  
  sam_data= eventReactive(
    input$sam_click,
    {
      data() %>%
        select(element, data) %>%
        filter(element == input$element) %>%
        unnest(data) %>%
        nearPoints(input$sam_click, xvar = "date_time", yvar = "value")
    }
  )
  observeEvent(input$std_element, {
    req(updated_std())
    output$std_graph = renderPlot({
      std_graph_format(
        updated_std(), std_cutoff(),
        input$std_element, input$date_range, input$method, input$std_type
      )
    })
  })
  
  observeEvent(input$element, {
    output$sam_graph = renderPlot({sam_graph_format(data(), input$element)})
  })
  
  output$std_tab = render_gt(std_table_create(data()))
  
  # Allows the user to download data.

  output$download = downloadHandler(
    filename = paste0(
      format(Sys.Date(), "%Y%m%d"),
      "_parsed_ICP_", input$method, "_data.xlsx"
    ),
    content = function(file){
      download_data(data()) %>%
        write_xlsx(file)
    }
  )
  
  output$download_std = downloadHandler(
    filename = paste0(
      format(Sys.Date(), "%Y%m%d"),
      "_ICP_STD_", input$method, "_",  input$std_type, ".xlsx"
    ),
    content = function(file){
      updated_std() %>%
        write_xlsx(file)
    }
  )
}
