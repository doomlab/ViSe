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
                            menuItem("From Effect Size",
                                     tabName = "effect_tab",
                                     icon = icon("gears")),
                            menuItem("From Summary Statistics",
                                     tabName = "stats_tab",
                                     icon = icon("bar-chart"))
                        ) #close menu
                    ), #close sidebar
                    dashboardBody(
                      tabItems(
                             data_tab,
                             effect_tab,
                             stats_tab
                            ) #close tabs
                        ) #close body
                    ) #close dashboard

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {

  # Effect Tab --------------------------------------------------------------

  output$visualize_c_effect <- renderPlot({
    visualize_c(dlow = input$enter_d)$graph
    })
  output$estimate_d_effect <- renderPlot({
    estimate_d(d = input$enter_d)$graph
  })
  output$convert_d_effect <- renderPlot({
    visualize_effects(d = input$enter_d)$graph
  })


}


# Run the Application -----------------------------------------------------
shinyApp(ui = ui, server = server)


