calculate_tab <-
  tabItem(tabName = "calculate_tab",
          fluidRow(
            # Instructions for this tab ----
            box(
              title = tags$b("Calculate Your Effect Size Instructions"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("On this tabe, we give you two options to calculate your
                effect size and the lower confidence interval bound. ",
                HTML("<ul>
                     <li>
                      Box 1: From an uploaded dataset.
                     </li>
                     <li>
                      Box 2: From summary statistics output or a research paper:
                      including descriptive statistics only, the t-test statistic,
                      or the effect size with sample size.
                     </li>
                     </ul>"),
                "You would only need to use one of the following boxes based on
                what type of data you have.",
                "In each of these scenarios, we can use the ", strong("calculate_d()"),
                "function to calculate these values. ", "You can close these boxes to help keep
                all the information on one page. "),

              p("If you need to switch between effect sizes, please use the
                Convert Effects tab.")
            ), # close box

            # Upload Data and Calculate ----
            box(
              title = tags$b("Data Input"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("In this section, you can upload your data to calculate
                statistics based on that dataframe. You may upload most data
                formats including csv, tsv, zip, gz, SAS, SPSS, Excel, JSON,
                Matlab, Minitab, and more."),
              fileInput(inputId = "upload_file",
                        label = "Choose File:",
                        multiple = FALSE,
                        accept = c(".csv", ".zip",
                                   ".tsv", ".sas7bdat",
                                   ".sav", ".dta",
                                   ".xls", ".xlsx",
                                   ".mtp", ".dbf", ".fwf",
                                   ".gz", ".feather",
                                   ".json", ".mat", ".xml")),
              tags$hr(style="border-color: blue;"),
              p("Once you have uploaded your dataset, please select the column
              with the group labels (i.e., the independent variable). This column
              can be a character, numeric, or factor column. Only two group labels
              or categories should be present in this column. Then select the
              dependent variable column. This column should be numeric.
                "),
              varSelectInput(
                inputId = "label_column",
                label = "Select indepedent variable column:",
                data = NULL,
                selected = NULL,
                multiple = FALSE
              ),
              varSelectInput(
                inputId = "dependent_column",
                label = "Select dependent variable column:",
                data = NULL,
                selected = NULL,
                multiple = FALSE
              ),
              numericInput(
                inputId = "data_enter_alpha",
                label = "Alpha:",
                value = .05,
                min = 0,
                max = 1,
                step = .01,
                width = NULL
              ),
              selectInput(
                inputId = "data_enter_lower",
                label = "Should the effect size be positive:",
                choices = c(TRUE, FALSE),
                selected = TRUE,
                width = NULL
              ),
              actionBttn(
                inputId = "calculate_d_data",
                label = "Calculate",
                icon = NULL,
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
              ),
              p(""),
              tags$hr(style="border-color: blue;"),
              p("View your data to ensure it uploaded correctly:"),
              DTOutput("file_view"),

              p(""),
              p("In the first box, the value of d is provided. The other four boxes
                include the lower tail of a one or two-tailed confidence interval.
                If you indicated that d should be positive, this value is the left-hand
                side of the confidence interval (note: it may be negative, if d is close
                to zero or sample size is small). If you indicated that d should be
                negative, then it is the right-hand side of the confidence interval.
                The 'lower' half of the confidence interval is always the one closer to
                zero. The one- and two-tailed outputs are provided given the alpha value
                you entered at the top. The central confidence interval assumes a normal
                distribution of d and uses the traditional formula for confidence intervals.
                The non-centralized versions assumes that effect sizes are skewed and uses
                non-centralized t-distribution to calculate the confidence interval.
                At larger sample sizes, these two values get closer together. "),
              valueBoxOutput("data_d"),
              valueBoxOutput("data_d_low_one_central"),
              valueBoxOutput("data_d_low_two_central"),
              valueBoxOutput("data_d_low_one_non"),
              valueBoxOutput("data_d_low_two_non")

            ), # box close

            # Enter Data and Calculate ----
            box(
              title = tags$b("Summary Statistics Input"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("This box helps you calculate the effect size ", em("d"),
                "using summary statistics from your output or a research paper.
                Please note that we assume that you are entering numbers for two
                groups using a between subjects comparison."),

              p("You must enter at least the group sample sizes, an alpha value
              for confidence interval
                (usually .05, but you could decide to use different values), and if
                you expect the second group mean to be lower (i.e., smaller) than the
                first group mean."),

              p("You can then enter:",
                HTML("<ul>
                     <li> The summary statistics: means and standard deviations
                     </li>
                     <li> The t-value from the test statistic
                     </li>
                     <li> The effect size d found from the study
                     </li>
                     </ul>
                     ")),
              p("Enter Sample Size Values, Alpha, and Lower (required):"),
              numericInput(
                inputId = "enter_n1",
                label = "Group 1 N:",
                value = 0,
                min = 0,
                max = NA,
                step = 1,
                width = NULL
              ),
              numericInput(
                inputId = "enter_n2",
                label = "Group 2 N:",
                value = 0,
                min = 0,
                max = NA,
                step = 1,
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
                inputId = "enter_lower",
                label = "Should the effect size be positive:",
                choices = c(TRUE, FALSE),
                selected = TRUE,
                width = NULL
              ),
              p("Enter Summary Statistics (see below if you only have ",
                em("t"), " or ", em("d"), " values)."),
              numericInput(
                inputId = "enter_m1",
                label = "Group 1 Mean:",
                value = NULL,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_m2",
                label = "Group 2 Mean:",
                value = NULL,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_sd1",
                label = "Group 1 Standard Deviation:",
                value = NULL,
                min = 0.0000000001,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "enter_sd2",
                label = "Group 2 Standard Deviation:",
                value = NULL,
                min = 0.0000000001,
                max = NA,
                step = NA,
                width = NULL
              ),
              p("Enter ", em("t"), "-test statistic (optional)."),
              numericInput(
                inputId = "enter_t",
                label = "t-Statistic:",
                value = NULL,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),
              p("Enter ", em("d"), "effect size (optional)."),
              numericInput(
                inputId = "enter_d",
                label = "Effect size (d):",
                value = NULL,
                min = NA,
                max = NA,
                step = NA,
                width = NULL
              ),

              p(""),
              p("In the first box, the value of the effect size d is provided.
              The other four boxes include the lower tail of a one or
              two-tailed confidence interval.  If you indicated that the effect
              size d should be positive, this value is the left-hand
              side of the confidence interval (note: it may be negative,
              if effect size d is close to zero or sample size is small).
              If you indicated that effect size d should be negative,
              then it is the right-hand side of the confidence interval.
              The 'lower' half of the confidence interval is always the
              one closer to zero. The one- and two-tailed outputs are provided
              given the alpha value you entered at the top. The central
              confidence interval assumes a normal distribution of effect
              size d and uses the traditional formula for confidence
              intervals. The non-centralized versions assumes that effect
              sizes are skewed and uses non-centralized t-distribution to
              calculate the confidence interval. At larger sample sizes,
              these two values get closer together. "),

              p(HTML("Note: if you calculate only from an effect size
              <i>d</i> value, the non-centralized confidence interval
              approximation may not be correct. The package converts
              this value to a <i>t</i> score assuming you have used the
              traditional formula with standard deviation pooled between groups
              on the bottom. If the score you entered uses a
              correction like Hedges' <i>g</i> or a different formula
              for effect size <i>d</i>, you may have a confidence
              interval that does not contain the effect size.")),
              p(strong(em("You will not see anything here until you
              enter enough of the required information."))),
              valueBoxOutput("summary_d"),
              valueBoxOutput("summary_d_low_one_central"),
              valueBoxOutput("summary_d_low_two_central"),
              valueBoxOutput("summary_d_low_one_non"),
              valueBoxOutput("summary_d_low_two_non")

            ) # box close
          ) # fluid row close
  ) # tab close
