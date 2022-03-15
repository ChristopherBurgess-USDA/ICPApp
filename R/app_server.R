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

library(googlesheets4)
library(writexl)

library(gt)
library(ggplot2)
library(scales)
library(cowplot)


library(shiny)
library(shinydashboard)
library(shinyjs)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

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
              "DTPA" = "DTPA"
            ),
            selected = ""
          ),
          fileInput(
            inputId = "upload",
            label = h4("Upload"),
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
        tabName = "std_qc", h3("Quality Check of Standards"),
        box(width=12, gt_output(outputId = "std_tab")),
        uiOutput("g_update")
      ),
      tabItem(
        tabName = "explore",
        selectInput(
          label = h3("Data Exploration by Element"), inputId = "element",
          choices = NULL, selected = NULL
        ),
        box(plotOutput(outputId = "std_graph", width = "90%")),
        box(plotOutput(outputId = "sam_graph", click = "sam_click", width = "90%"),
            verbatimTextOutput(outputId = "sam_info"))
      ),
      tabItem(
        tabName = "download", h3("Download Data"),
        box(
          helpText("Download data as excel file."),
          downloadButton(outputId = "download", label = "Download")
        )
      )
    )
  })
  output$g_update = renderUI({
    box(
      width=12,
      h3("Would you like to add this data to Standard tracking?"),
      actionButton(
        inputId = "update_click",
        "Update"
      ),
      align = "center"
    )
  })
  
  std_data = eventReactive(input$method, {
    read_google_std(input$method) %>%
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
    ipc_load(input$upload$datapath) 
  })
  
  data = reactive({
    req(input_data(), std_cutoff())
    parsing_data(input_data(), input$blank, std_cutoff())
  })
  
  
  observeEvent(input$update_click, {
    upload_google(data(), input$method)
    toggle("g_update")
  })
  
  observe({
    ## The observe is like ta reactive accept it doesn't return anything
    ## Since we do not know how many or which files the user will be uploading the observe
    ## is constantly updating the multiple choice list with the name of the files.
    ele_list = data()$element
    updateSelectInput(
      session,
      inputId = "element",
      choices = ele_list,
      selected = NULL
    )
    output$menu = renderMenu({
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Standard QC", tabName = "std_qc", icon = icon("heartbeat")),
        menuItem("Data Explore", icon = icon("search"), tabName = "explore")
      )
    })
    
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
  observeEvent(input$element, {
    output$std_graph = renderPlot({std_graph_format(data(), input$element, std_data())})
    output$sam_graph = renderPlot({sam_graph_format(data(), input$element)})
    output$sam_info = renderPrint({
      sam_data() %>%
        print.data.frame()
    })
    
  })
  
  output$std_tab = render_gt(std_table_create(data()))
  
  # Allows the user to download data.
  output$download = downloadHandler(
    filename = "parsed_icp_data.xlsx",
    content = function(file){
      download_data(data()) %>%
        write_xlsx(file)
    }
  )
}
