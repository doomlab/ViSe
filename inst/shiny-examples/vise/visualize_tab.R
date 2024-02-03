visualize_tab <-
  tabItem(tabName = "visualize_tab",
          fluidRow(
            # Instructions for this tab ----
            box(
              title = tags$b("Visualization Instructions"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
            ), # close box

            # Visualize d ----
            box(
              title = tags$b("Visualize d Effect Size"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("In this section, you can enter an effect size to visualize the
              difference between two separate groups. Please note: we assume you
                are entering a ", em("d"), " value for between subjects comparisons."),
              p("We can use ", strong("estimate_d()"), " to visualize the differences between
                  groups if we are unsure of what the effect size might be. "),
              numericInput(
                inputId = "enter_d_effect",
                label = "Proposed d value:",
                value = .3,
                min = -5,
                max = 5,
                step = .01,
                width = NULL
              ),
              plotOutput("estimate_d_effect")
            ), # close box

            # Visualize r ----
            box(
              title = tags$b("Visualization of Correlation Coefficient"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("estimate_r()"), " to visualize the
                correlation between our other variables. You would enter your
                correlation coefficient to receive a visualization of the
                potential pattern of data in a scatterplot. You can use this
                visualization to know what correlation to estimate for the
                sensitivity calculation."),
              numericInput(
                inputId = "enter_r_effect",
                label = "Proposed r value:",
                value = 0,
                min = -1,
                max = 1,
                step = NA,
                width = NULL
              ),
              plotOutput("estimate_r_effect")
            ), # close box

            # Visualize all ----
            box(
              title = tags$b("Visualization of Potential Options"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("visualize_c_map()"), "what values might show
                an effect after controlling for the correlation between another
                variable and your outcome."),
              p("First, enter your lower bound of the effect size, which can be
                calculated on the Calculate Effects tab."),

              # enter here
              numericInput(
                inputId = "enter_d_lower",
                label = "Lower confidence interval of d:",
                value = 0,
                min = -3,
                max = 3,
                step = NA,
                width = NULL
              ),

              # do you expect it to be negative?
              selectInput(
                inputId = "visualize_enter_lower",
                label = "Should the effect size be positive:",
                choices = c(TRUE, FALSE),
                selected = TRUE,
                width = NULL
              ),
              p("Next, use the two plots above or the Convert Effects
                tab to enter the potential effect sizes", em("d or others"), "and the
                correlation between outcome and another variable ",
                em("r"), "below ", strong("separated by commas.")),
              p("Each of the entered effect sizes will appear on the graph, converted to d
                values, so you can see how they compare. You must enter the correlation
                coefficient and at least one other effect size to see the
                dots on the plot."),
              p(""),
              textInput(inputId = "d_values_stats",
                        label = "Enter d values",
                        value = ""),
              textInput(inputId = "r_values_stats",
                        label = "Enter r values",
                        value = ""),
              p(""),
              plotlyOutput("visual_c_map_stats")
            ) # close box
          ) # fluid row
        ) #close page
