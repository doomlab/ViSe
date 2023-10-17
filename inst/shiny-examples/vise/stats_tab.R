stats_tab <-
  tabItem(tabName = "stats_tab",

          # Enter Data ---------------------------------------------------
          fluidRow(
            box(
              title = tags$b("Summary Statistics Input"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("In this section, you can calculate the effect size,
                lower bounds, and more by entering summary statistics.
                Please note: we assume you are entering numbers to calculate a ",
                em("d"), " value for between subjects comparisons."),

              p("Enter Sample Size Values, Alpha, and Lower (required):"),
              numericInput(
                inputId = "enter_n1",
                label = "Group 1 N:",
                value = 0,
                min = 0,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_n2",
                label = "Group 2 N:",
                value = 0,
                min = 0,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_alpha",
                label = "Alpha:",
                value = .05,
                min = 0,
                max = 1,
                step = .01,
                width = NULL
              ),
              selectInput(
                inputId = "lower",
                label = "Do you expect d to be negative?",
                selected = "FALSE",
                choices = c("TRUE", "FALSE")
              ),
              p("Enter Summary Statistics (see below if you only have ",
                em("t"), " values)."),
              numericInput(
                inputId = "enter_m1",
                label = "Group 1 Mean:",
                value = 0,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_m2",
                label = "Group 2 Mean:",
                value = 0,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_sd1",
                label = "Group 1 Standard Deviation:",
                value = 0.01,
                min = 0.0000000001,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_sd1",
                label = "Group 2 Standard Deviation:",
                value = 0.01,
                min = 0.0000000001,
                max = NA,
                step = NA,
                width = NULL
              ),
              p("Enter ", em("t"), "-test statistic (optional)."),
              numericInput(
                inputId = "enter_t",
                label = "t-Statistic:",
                value = 0,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              )

            ), #box

            # Visualize c -------------------------------------------------------------
            box(
              title = tags$b("Visualization of c"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("visualize_c()"), " to visualize the combinations of
                effect size and correlation that would support an effect. "),
              plotOutput("visualize_c_stats")
            ), #box

            # Visualize d -------------------------------------------------------------
            box(
              title = tags$b("Visualization of d"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("We can use ", strong("estimate_d()"), " to visualize the differences between
                groups if we are unsure of what the effect size might be."),
              plotOutput("estimate_d_stats")
          ), #box

          # Visualize d -------------------------------------------------------------
          box(
            title = tags$b("Visualization of converted effect sizes"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(""),
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
            plotOutput("convert_d_stats")
          ) #box
      ) #fluidrow
    ) #close page
