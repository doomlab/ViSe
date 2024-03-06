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
library(ggplot2)
library(scales)
library(cowplot)
# library(plotly)
library(knitr)
library(kableExtra)

# Load Pages --------------------------------------------------------------
source("example_tab.R")
source("calculate_tab.R")
source("convert_tab.R")
source("visualize_tab.R")

options(shiny.reactlog=TRUE)

# User Interface ----------------------------------------------------------
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Visualizing Sensitivity"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Example Walkthrough",
                                   tabName = "example_tab",
                                   icon = icon("file")),
                            menuItem(HTML("Calculate standardized <br/>
                                          group difference"),
                                     tabName = "calculate_tab",
                                     icon = icon("table")),
                            menuItem(HTML("Convert between effect <br />
                                          size measures"),
                                     tabName = "convert_tab",
                                     icon = icon("gears")),
                            menuItem(HTML("Visualize sensitivity of <br/>
                                          effect size to bias"),
                                     tabName = "visualize_tab",
                                     icon = icon("bar-chart"))
                        ) #close menu
                    ), #close sidebar
                    dashboardBody(
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      tabItems(
                        example_tab,
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
      }  else { t_enter <- input$enter_t}

      if (!isTruthy(input$enter_d)) {
        d_enter <- NULL
      }  else { d_enter <- input$enter_d}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter,
        d = d_enter
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
      }  else { t_enter <- input$enter_t}

      if (!isTruthy(input$enter_d)) {
        d_enter <- NULL
      }  else { d_enter <- input$enter_d}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter,
        d = d_enter
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
      }  else { t_enter <- input$enter_t}

      if (!isTruthy(input$enter_d)) {
        d_enter <- NULL
      }  else { d_enter <- input$enter_d}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter,
        d = d_enter
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
      }  else { t_enter <- input$enter_t}

      if (!isTruthy(input$enter_d)) {
        d_enter <- NULL
      }  else { d_enter <- input$enter_d}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter,
        d = d_enter
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
      }  else { t_enter <- input$enter_t}

      if (!isTruthy(input$enter_d)) {
        d_enter <- NULL
      }  else { d_enter <- input$enter_d}

      d_calc <- calculate_d(
        m1 = input$enter_m1,
        m2 = input$enter_m2,
        sd1 = input$enter_sd1,
        sd2 = input$enter_sd2,
        n1 = input$enter_n1,
        n2 = input$enter_n2,
        a = input$enter_alpha,
        lower = input$enter_lower,
        t = t_enter,
        d = d_enter
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
                       y_col = as.character(input$dependent_column),
                       a = input$data_enter_alpha,
                       lower = input$data_enter_lower)
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

    # visualization of convert tab
    output$visualize_conversion <- renderPlot({

      if(input$convert_select_effect == "d"){
        d <- input$convert_enter_effect
      }

      if(input$convert_select_effect == "f"){
        d <- other_to_d(f = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "f squared"){
        d <- other_to_d(f2 = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Number Needed to Treat nnt"){
        d <- other_to_d(nnt = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Correlation Coefficient r"){
        d <- other_to_d(r = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Probability of Superiority"){
        d <- other_to_d(prob = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Proportion Overlap U1"){
        d <- other_to_d(prop_u1 = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Proportion Overlap U2"){
        d <- other_to_d(prop_u2 = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Proportion Overlap U3"){
        d <- other_to_d(prop_u3 = input$convert_enter_effect)
      }

      if(input$convert_select_effect == "Proportional Overlap of Distributions"){
        d <- other_to_d(prop_overlap = input$convert_enter_effect)
      }

      visualize_effects(d = d)$graph

    })

    # visualization page ----
    output$estimate_d_effect <- renderPlot({
      estimate_d(d = input$enter_d_effect)$graph
    })

    output$estimate_r_effect <- renderPlot({
      estimate_r(r = input$enter_r_effect)$graph
    })

    output$visual_c_map_stats <- renderPlot({

      if (input$visualize_d_values != ""){
        d_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_d_values, ","))))
      } else { d_values <- NULL }

      if (input$visualize_f_values != ""){
        f_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_f_values, ","))))
      } else { f_values <- NULL }

      if (input$visualize_f2_values != ""){
        f2_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_f2_values, ","))))
      } else { f2_values <- NULL }

      if (input$visualize_nnt_values != ""){
        nnt_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_nnt_values, ","))))
      } else { nnt_values <- NULL }

      if (input$visualize_prob_values != ""){
        prob_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_prob_values, ","))))
      } else { prob_values <- NULL }

      if (input$visualize_u1_values != ""){
        prop_u1_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_u1_values, ","))))
      } else { prop_u1_values <- NULL }

      if (input$visualize_u2_values != ""){
        prop_u2_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_u2_values, ","))))
      } else { prop_u2_values <- NULL }

      if (input$visualize_u3_values != ""){
        prop_u3_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_u3_values, ","))))
      } else { prop_u3_values <- NULL }

      if (input$visualize_overlap_values != ""){
        prop_overlap_values <- na.omit(as.numeric(unlist(strsplit(input$visualize_overlap_values, ","))))
      } else { prop_overlap_values <- NULL }

      temp <- visualize_c_map(
        dlow = input$visualize_d_lower,
        lower = input$visualize_enter_lower,
        r_values = na.omit(as.numeric(unlist(strsplit(input$visualize_r_values, ",")))),
        d_values = d_values,
        f_values = f_values,
        f2_values = f2_values,
        nnt_values = nnt_values,
        prob_values = prob_values,
        prop_u1_values = prop_u1_values,
        prop_u2_values = prop_u2_values,
        prop_u3_values = prop_u3_values,
        prop_overlap_values = prop_overlap_values)$graph

      temp
    })

    # example information -----------------------------------------------------
    output$data_kable <- function() {
      DF <- data.frame(
        "Internalising Score Unadjusted" = c(3.68, 1.73, 5.62),
        "Internalising Score Adjusted" = c(2.73, 0.77, 4.69),
        "Internalising d Unadjusted" = c(0.44, 0.20, 0.68),
        "Internalising d Adjusted" = c(0.33, 0.09, 0.57)

      )

      rownames(DF) <- c("Mean Difference", "Lower Bound", "Upper Bound")

      DF %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }



}


# Run the Application -----------------------------------------------------
shinyApp(ui = ui, server = server)
