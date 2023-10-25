# Information -------------------------------------------------------------
# Written by Erin M. Buchanan
# Dashboard for ViSe

# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ViSe)
library(shinyWidgets)
library(rio)

# Load Pages --------------------------------------------------------------
source("data_tab.R")
source("effect_tab.R")
source("stats_tab.R")

# User Interface ----------------------------------------------------------
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Visualizing Sensitivity"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("From Data",
                                     tabName = "data_tab",
                                     icon = icon("table")),
                            menuItem("From Summary Statistics",
                                     tabName = "stats_tab",
                                     icon = icon("bar-chart")),
                            menuItem("From Effect Size",
                                     tabName = "effect_tab",
                                     icon = icon("gears"))
                        ) #close menu
                    ), #close sidebar
                    dashboardBody(
                      tabItems(
                             data_tab,
                             stats_tab,
                             effect_tab
                            ) #close tabs
                        ) #close body
                    ) #close dashboard

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {


  # Data Tab ----------------------------------------------------------------

  # upload the data
  DF <- reactive({
    inFile <- input$upload_file
    if (is.null(inFile))
      return(NULL)
    master <- import(inFile$datapath)
    master
  })

  output$file_view <- renderDataTable({
    datatable(DF())
  })

  # calculate d function
  data_effect_size <- eventReactive(input$calculate_d_data, {
    temp <- DF()
    d <- calculate_d(df = temp,
                     x_col = as.character(input$label_column),
                     y_col = as.character(input$dependent_column))
    d
  })

  # update columns
  observeEvent(DF(), {
    updateVarSelectInput(session, "label_column", data = DF())
    updateVarSelectInput(session, "dependent_column", data = DF())
  })

  # convert_d_data plot
  output$convert_d_data <- renderPlot({
    visualize_effects(d = data_effect_size()$d)$graph
  })
  # visualize_c_data plot
  output$visualize_c_data <- renderPlot({
    visualize_c(d = data_effect_size()$dlow)$graph
  })
  # estimate_d_data plot
  output$estimate_d_data <- renderPlot({
    estimate_d(d = input$enter_d_data)$graph
  })
  # estimate_r_data plot
  output$estimate_r_data <- renderPlot({
    estimate_r(r = input$enter_r_data)$graph
  })
  # visual_c_map_data plot
  output$visual_c_map_data <- renderPlot({
    visualize_c_map(d = data_effect_size()$dlow,
                    dvalues = na.omit(as.numeric(unlist(strsplit(input$d_values_data, ",")))),
                    rvalues = na.omit(as.numeric(unlist(strsplit(input$r_values_data, ","))))
                    )$graph
  })

  # Effect Tab --------------------------------------------------------------

  output$estimate_d_effect <- renderPlot({
    estimate_d(d = input$enter_d_effect)$graph
  })
  output$convert_d_effect <- renderPlot({
    visualize_effects(d = input$enter_d_effect)$graph
  })
  output$estimate_r_effect <- renderPlot({
    estimate_r(r = input$enter_r_effect)$graph
  })
  output$visual_c_map_effect <- renderPlot({
    visualize_c_map(d = input$enter_d_effect,
                    dvalues = na.omit(as.numeric(unlist(strsplit(input$d_values_effect, ",")))),
                    rvalues = na.omit(as.numeric(unlist(strsplit(input$r_values_effect, ","))))
    )$graph
  })

  # stats tab ---------------------------------------------------------------

  output$visualize_c_stats <- renderPlot({
    d_calc <- calculate_d(
      m1 = input$enter_m1,
      m2 = input$enter_m2,
      sd1 = input$enter_sd1,
      sd2 = input$enter_sd2,
      n1 = input$enter_n1,
      n2 = input$enter_n2,
      a = input$enter_alpha,
      lower = input$enter_lower,
      t = input$enter_t
    )

    visualize_c(dlow = d_calc$dlow)$graph
  })
  output$convert_d_stats <- renderPlot({
    d_calc <- calculate_d(
      m1 = input$enter_m1,
      m2 = input$enter_m2,
      sd1 = input$enter_sd1,
      sd2 = input$enter_sd2,
      n1 = input$enter_n1,
      n2 = input$enter_n2,
      a = input$enter_alpha,
      lower = input$enter_lower,
      t = input$enter_t
    )
    visualize_effects(d = d_calc$d)$graph
  })

  output$estimate_d_stats <- renderPlot({
    estimate_d(d = input$enter_d_stats)$graph
  })
  output$estimate_r_stats <- renderPlot({
    estimate_r(r = input$enter_r_stats)$graph
  })
  output$visual_c_map_stats <- renderPlot({
    visualize_c_map(d = input$enter_d_stats,
                    dvalues = na.omit(as.numeric(unlist(strsplit(input$d_values_stats, ",")))),
                    rvalues = na.omit(as.numeric(unlist(strsplit(input$r_values_stats, ","))))
    )$graph
  })


}


# Run the Application -----------------------------------------------------
shinyApp(ui = ui, server = server)


