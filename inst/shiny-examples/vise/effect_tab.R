effect_tab <-
  tabItem(tabName = "effect_tab",

          # Enter Data ---------------------------------------------------
          fluidRow(
            box(
              title = tags$b("Effect Size Input"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("In this section, you can enter an effect size to see the
                all the functions that can be calculated with just the effect
                size. Please note: we assume you are entering an effect size ",
                em("d"), " value for between subjects comparisons."),
              numericInput(
                inputId = "enter_d_effect",
                label = "Proposed d value:",
                value = 0,
                min = -5,
                max = 5,
                step = .01,
                width = NULL
              ),
              textInput(
                inputId = "enter_d_effect_fill_1",
                label = "Fill color 1:",
                value = "blue",
                width = NULL
              ),
              textInput(
                inputId = "enter_d_effect_fill_2",
                label = "Fill color 2:",
                value = "red",
                width = NULL
              ),
              textInput(
                inputId = "enter_d_effect_text_color",
                label = "Text color:",
                value = "black",
                width = NULL
              ),
              p("We can use ", strong("estimate_d()"), " to visualize the
              differences between groups if we are unsure of what the
                effect size might be."),
              plotOutput("estimate_d_effect")
            ), #box

            # Visualize r -------------------------------------------------------------
            box(
              title = tags$b("Visualization of r"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("estimate_r()"), " to visualize the
              correlation between our other variables."),
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
            ), #box

            # Visualize converted effects -------------------------------------------------------------
            box(
              title = tags$b("Visualization of converted effect sizes"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("visualize_effects()"),
              " to visualize the relationship between the same effect size
              in different metrics. These are calculated
              from functions you can use in the package:"),
              br("1) d_to_f2: Cohen’s f and f2"),
              br("2) d_to_nnt: Number needed to treat"),
              br("3) d_to_r: correlation coefficient"),
              br("4) probability_superiority: The probability of superiority"),
              br("5) proportion_overlap: u1 represents the proportion of
              non-overlap across both group distributions,
              u2 indicates the proportion that one group is more than the
              same proportion in the other group, u3 shows the proportion of
              one group that is smaller than the median of the other group,
              and p_o is the proportional overlap of groups."),
              plotOutput("convert_d_effect")
            ), #box

          # Visualize c map -------------------------------------------------------------
          box(
            title = tags$b("Visualization of Potential Options"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(""),
            p("We can use ", strong("visualize_c_map()"),
            " what values might show an effect. After using the two
            plots above, values for effect size ",
              em("d"), " and correlation", em("r"),
            " below ", strong("separated by commas.")),
            textInput(inputId = "d_values_effect",
                      label = "Enter d values",
                      value = ""),
            textInput(inputId = "r_values_effect",
                      label = "Enter r values",
                      value = ""),
            plotlyOutput("visual_c_map_effect")
          ) #box


      ) #fluidrow
  ) #close page

