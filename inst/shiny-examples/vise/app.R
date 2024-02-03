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
library(tidyr)
library(plotly)
library(ggplot2)
library(scales)
library(cowplot)

# Load Pages --------------------------------------------------------------
source("calculate_tab.R")
source("convert_tab.R")
source("visualize_tab.R")

options(shiny.reactlog=TRUE)

# User Interface ----------------------------------------------------------
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Visualizing Sensitivity"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Calculate Effects",
                                     tabName = "calculate_tab",
                                     icon = icon("table")),
                            menuItem("Convert Effects",
                                     tabName = "stats_tab",
                                     icon = icon("gears")),
                            menuItem("Visualize Effects",
                                     tabName = "effect_tab",
                                     icon = icon("bar-chart"))
                        ) #close menu
                    ), #close sidebar
                    dashboardBody(
                      tabItems(
                             calculate_tab,
                             convert_tab,
                             visualize_tab
                            ) #close tabs
                        ) #close body
                    ) #close dashboard

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {

  # calculate tab ----

    # calculate summary ----
    output$summary_d <- renderValueBox({

      if (!isTruthy(input$enter_t)) {
        t_enter <- NULL
      } else { t_enter <- input$enter_t}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter
      )

      valueBox(
        paste0(format(round(d_calc$d, 2), nsmall = 2)),
        "d",
        color = "green"
      )
    })

    output$summary_d_low_one_central <- renderValueBox({
      if (!isTruthy(input$enter_t)) {
        t_enter <- NULL
      } else { t_enter <- input$enter_t}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter
      )

      valueBox(
        paste0(format(round(d_calc$done_low_central, 2), nsmall = 2)),
        "d Lower One Tail Central",
        color = "green"
      )
    })

    output$summary_d_low_two_central <- renderValueBox({
      if (!isTruthy(input$enter_t)) {
        t_enter <- NULL
      } else { t_enter <- input$enter_t}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter
      )

      valueBox(
        paste0(format(round(d_calc$dlow_central, 2), nsmall = 2)),
        "d Lower Two Tail Central",
        color = "green"
      )
    })

    output$summary_d_low_one_non <- renderValueBox({
      if (!isTruthy(input$enter_t)) {
        t_enter <- NULL
      } else { t_enter <- input$enter_t}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter
      )

      valueBox(
        paste0(format(round(d_calc$done_low, 2), nsmall = 2)),
        "d Lower One Tail Non-Central",
        color = "green"
      )
    })

    output$summary_d_low_two_non <- renderValueBox({
      if (!isTruthy(input$enter_t)) {
        t_enter <- NULL
      } else { t_enter <- input$enter_t}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter
      )

      valueBox(
        paste0(format(round(d_calc$dlow, 2), nsmall = 2)),
        "d Lower Two Tail Non-Central",
        color = "green"
      )
    })

    # calculate data ----
    # get data
    DF <- reactive({
      inFile <- input$upload_file
      if (is.null(inFile))
        return(NULL)
      master <- import(inFile$datapath)
      master
    })

    # view data
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

    output$data_d <- renderValueBox({
      valueBox(
        paste0(format(round(data_effect_size()$d, 2), nsmall = 2)),
        "d",
        color = "green"
      )
    })

    output$data_d_low_one_central <- renderValueBox({
      valueBox(
        paste0(format(round(data_effect_size()$done_low_central, 2), nsmall = 2)),
        "d Lower One Tail Central",
        color = "green"
      )
    })

    output$data_d_low_two_central <- renderValueBox({
      valueBox(
        paste0(format(round(data_effect_size()$dlow_central, 2), nsmall = 2)),
        "d Lower Two Tail Central",
        color = "green"
      )
    })

    output$data_d_low_one_non <- renderValueBox({
      valueBox(
        paste0(format(round(data_effect_size()$done_low, 2), nsmall = 2)),
        "d Lower One Tail Non-Central",
        color = "green"
      )
    })

    output$data_d_low_two_non <- renderValueBox({
      valueBox(
        paste0(format(round(data_effect_size()$dlow, 2), nsmall = 2)),
        "d Lower Two Tail Non-Central",
        color = "green"
      )
    })


    ### input: observed d and sample sizes n1 n2
    d_obs = 0.1
    n1 = 5
    n2 = 5

    ### computing scale factor n and degrees of freedom
    n  = n1*n2/(n1+n2)
    nu = n1+n2-2


    ### a suitable grid 'ds' for a grid search
    ### based on
    var_est <- n^-1 + d_obs^2/2/nu
    ds <- seq(d_obs-4*var_est^0.5,d_obs+4*var_est^0.5,var_est^0.5/10^4)


    ### boundaries based on limits of t-distributions with ncp parameter
    ### for which the observed d will be in the 2.5% left or right tail
    upper <- min(ds[which(pt(d_obs*sqrt(n),nu,ds*sqrt(n))<0.025)])*sqrt(n)    # t-distribution boundary
    upper/sqrt(n)                                                             # scaled boundary
    lower <- max(ds[which(pt(d_obs*sqrt(n),nu,ds*sqrt(n))>0.975)])*sqrt(n)
    lower/sqrt(n)




}


# Run the Application -----------------------------------------------------
shinyApp(ui = ui, server = server)
