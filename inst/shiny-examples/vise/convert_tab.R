convert_tab <-
  tabItem(tabName = "convert_tab",
          fluidRow(
            # Instructions for this tab ----
            box(
              title = tags$b("Convert Between Effect Sizes"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("On this page, you can convert between effect sizes
                to help you calculate effect size values for the Visualization
                tab. You should enter your known effect size and which
                effect size you are entering. All values will appear below
                in a chart format. Note that the chart tries to give
                a visualization of how each relates, but the plots
                are just to help you get an idea for the size of the effect.
                Some effect sizes do not have bounds (i.e., no real upper or
                lower limit), so the plot uses common sense upper and lower
                bounds."),
              p("We can use ", strong("visualize_effects()"), " to visualize the relationship
              between the same effect size in different metrics. These are calculated
              from functions you can use in the package:"),
              br("1) d_to_f2: Cohen’s f and f2"),
              br("2) d_to_nnt: Number needed to treat"),
              br("3) d_to_r: correlation coefficient"),
              br("4) probability_superiority: The probability of superiority"),
              br("5) proportion_overlap: u1 represents the proportion of non-overlap
            across both group distributions, u2 indicates the proportion that one
            group is more than the same proportion in the other group, u3 shows
            the proportion of one group that is smaller than the median of the
            other group, and p_o is the proportional overlap of groups."),
              br("6) other_to_d: A function to convert from other effect sizes
                  to d."),
              numericInput(
                inputId = "convert_enter_effect",
                label = "Effect Size:",
                value = 0,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              selectInput(
                inputId = "convert_select_effect",
                label = "Select your entered effect size:",
                choices = c("d", "f", "f squared",
                            "Number Needed to Treat nnt",
                            "Correlation Coefficient r",
                            "Probability of Superiority",
                            "Proportion Overlap U1",
                            "Proportion Overlap U2",
                            "Proportion Overlap U3",
                            "Proportional Overlap of Distributions"),
                selected = NULL,
                width = NULL
              ),
              plotOutput("visualize_conversion")
            ) # close box
          ) # fluid row
        ) #close page

