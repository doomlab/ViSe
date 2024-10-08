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
              p("The visualization tab provides several options for visualizing
                and determining which combinations of two bias quantities
                would support a causal effect. The quantities are about the relation of
                the predisposition to the outcome with 1) the factor and 2) the true
                outcome (some variables related both to factor and outcome may have
                already been adjusted for. In this case, the predisposition beyond
                these variables is meant.)",
                HTML("<ul><li>1) <b>First quantity:</b>
                The first box includes a visualization of the distributions
                of the predispositions to the outcome in both groups
                and their overlap. In a perfect experiment they would completely overlap.
                You can use this box to help you
                imagine how much non-overlap there might be and what
                standardized effect size d that corresponds to. Or enter
                the d that you obtained by converting another effect
                size measure via the 'Convert between effect size measures'
                tab.
                 </li>
                 <li>2) <b>Second quantity</b>: The second box includes a
                 visualization of the correlation between the predisposition
                 and the outcome. You can use this visualization to imagine
                 the amount of variance overlap between the two
                 variables, <i>R<sup>2</sup></i>.
                 </li>
                 <li>3) Once these values are determined, the final box
                     creates a sensitivity plot for you. This plot will
                     shade the areas that indicate a combination of effect size
                     <b>d</b> and
                     correlation that supports a causal effect. You can
                     also enter other effect size measures that will be
                     converted to <b>d</b>.</li></ul>"))
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
                are entering an effect size of ",
                em("d"), " value for between subjects comparisons."),
              p("We can use ", strong("estimate_d()"),
              " to visualize the differences between
                  groups to narrow down plausible values of the first quantity."),
              numericInput(
                inputId = "enter_d_visualize",
                label = "Proposed d value:",
                value = .3,
                min = -5,
                max = 5,
                step = .01,
                width = NULL
              ),
              textInput(
                inputId = "enter_d_visualize_fill_1",
                label = "Fill color 1:",
                value = "blue",
                width = NULL
              ),
              textInput(
                inputId = "enter_d_visualize_fill_2",
                label = "Fill color 2:",
                value = "red",
                width = NULL
              ),
              textInput(
                inputId = "enter_d_visualize_text_color",
                label = "Text color:",
                value = "black",
                width = NULL
              ),
              plotOutput("estimate_d_visualize")
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
                visualization to specify what correlation to enter into the
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
              p("We can use ", strong("visualize_c_map()"), "what values support
              an effect with the values specified for both bias quantities."),
              p("First, enter your lower bound of the effect size found in a study,
              which can be
                calculated on the Calculate Effects tab."),
              p("Second, enter the plausible values of the correlation
                between your outcome and the predisposition. You can visualize
                this difference using ", strong("visualize_r()"),
                " or using the box above."),
              p("Last, enter the plausible values of the effect size between
                the factor and the predisposition. You can visualize this
                difference using ",
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
                tab to enter the potential effect sizes ",
                em("d or others"),
                " and the correlation between outcome and another variable ",
                em("r"),
                " below ", strong("separated by commas.")),
              p("Each of the entered effect sizes will appear on the
              graph, converted to effect size d
                values, so you can see how they compare.",
                strong(em("You must enter the correlation
                coefficient and at least one other effect size to see the
                dots on the plot."))),
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
              p("This next section allows you to edit the colors,
                shapes, and the shaded area color."),
              textInput(inputId = "visualize_point_colors",
                        label = "Enter a set of colors you want to use
                        for the points, separated by commas (note: you need
                        as many color names/codes as the largest numbers
                        of effect sizes): ",
                        value = "red, blue, green"),
              textInput(inputId = "visualize_ribbon_color",
                        label = "Enter color name you would like the shaded
                        region to be:",
                        value = "lightblue"),
              numericInput(
                inputId = "visualize_size",
                label = "Enter the size of the shapes: ",
                value = 1,
                min = -3,
                max = 3,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "visualize_shape_1",
                label = "Enter a number for the first shape size:",
                value = 1,
                min = 0,
                max = 25,
                step = 1,
                width = NULL
              ),
              numericInput(
                inputId = "visualize_shape_2",
                label = "Enter a number for the second shape size (note:
                we think it's easier to read if you have two shapes together.
                If you want them to be the same, use the same number): ",
                value = 1,
                min = 0,
                max = 25,
                step = 1,
                width = NULL
              ),
              p(""),
              plotOutput("visual_c_map_stats")
            ) # close box
          ) # fluid row
        ) #close page
