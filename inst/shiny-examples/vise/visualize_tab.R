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
              p("The visualization tab includes several options for you
                to visualize and determine the values that would indicate
                a causal effect. ",
                HTML("<ul><li>1) The first box includes a visualization
                     of the overlap between two group distributions.
                     You can use this box to help you imagine the size
                     of the standardized group differences that may
                     occur and what size effect size that corresponds to.
                     </li>
                     <li>2) The second box includes a visualization of
                     the correlation between the outcome and potential
                     variable to control for. You can use this visualization
                     to imagine the amount of variance overlap between
                     the two variables and what correlation that would
                     correspond to. </li>
                     <li>3) Once these values are determined, you can
                     use the final box to create a sensitivity plot. This
                     plot will shade the areas that indicate a combination
                     of correlation and effect size that indicates a causal
                     effect. You can enter different types of effect sizes,
                     and these will be converted to d to show how they
                     correspond to combinations that may support a causal
                     effect.</li></ul>"))
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
              p("Second, enter the possible values of the correlation
                between your outcome and other variables. You can visualize
                this difference using ", strong("visualize_r()"),
                " or using the box above."),
              p("Last, enter the possible values of the effect size found in the
              study. You can visualize this difference using ",
                strong("visualize_d()"), " or using the box above.
                You can use other effect sizes, and these will be converted
                to d to map them onto the graph below. Each is labeled with
                the original statistic and size. This app uses plotly to allow
                you to hover over points to determine their exact coordinates. "),

              # enter here
              numericInput(
                inputId = "visualize_d_lower",
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
              textInput(inputId = "visualize_r_values",
                        label = "Enter r values:",
                        value = ""),
              textInput(inputId = "visualize_d_values",
                        label = "Enter d values:",
                        value = ""),
              textInput(inputId = "visualize_f_values",
                        label = "Enter f values:",
                        value = ""),
              textInput(inputId = "visualize_f2_values",
                        label = "Enter f squared values:",
                        value = ""),
              textInput(inputId = "visualize_nnt_values",
                        label = "Enter number needed to treat values:",
                        value = ""),
              textInput(inputId = "visualize_prob_values",
                        label = "Enter probability of superiority values:",
                        value = ""),
              textInput(inputId = "visualize_u1_values",
                        label = "Enter proportion overlap U1 values:",
                        value = ""),
              textInput(inputId = "visualize_u2_values",
                        label = "Enter proportion overlap U2 values:",
                        value = ""),
              textInput(inputId = "visualize_u3_values",
                        label = "Enter proportion overlap U3 values:",
                        value = ""),
              textInput(inputId = "visualize_overlap_values",
                        label = "Enter proportion of distribution overlap values:",
                        value = ""),
              p(""),
              plotlyOutput("visual_c_map_stats")
            ) # close box
          ) # fluid row
        ) #close page
