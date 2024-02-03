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
              p("On this tabe, we give you three options to calculate your
                effect size and the lower confidence interval bound. ",
                HTML("<ul>
                     <li>
                      Box 1: From an uploaded dataset
                     </li>
                     <li>
                      Box 2: From summary statistics output or a research paper
                     </li>
                     <li>
                      Box 3: From the effect size and sample sizes
                     </li>
                     </ul>"),
                "You would only need to use one of the following boxes based on
                what type of data you have. You can close these boxes to help keep
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
              p("Select the columns that your labels and dependent variables are in."),
              varSelectInput(
                inputId = "label_column",
                label = "Select label column:",
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
                groups using a between subjects comparison. You must enter
                at least the group sample sizes, an alpha value for confidence interval
                (usually .05, but you could decide to use different values), and if
                you expect the second group mean to be lower (i.e., smaller) than the
                first group mean. You can then enter either the summary statistics
                using means and standard deviations or the t-test value. "),
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
                inputId = "enter_lower",
                label = "Should the effect size be positive:",
                choices = c(TRUE, FALSE),
                selected = TRUE,
                width = NULL
              ),
              p("Enter Summary Statistics (see below if you only have ",
                em("t"), " values)."),
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
              valueBoxOutput("summary_d"),
              valueBoxOutput("summary_d_low_one_central"),
              valueBoxOutput("summary_d_low_two_central"),
              valueBoxOutput("summary_d_low_one_non"),
              valueBoxOutput("summary_d_low_two_non")

            ), # box close

            # Enter Effect Size and SE ----
            box(
              title = tags$b("d and Sample Size Input"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(""),
              p("In this box, you can calculate the confidence interval
                of a d value you already know. You will need to enter the
                sample sizes for both groups, the d-value, and the alpha
                value, and if you expect d to be positive
                for the confidence interval."),
              numericInput(
                inputId = "d_enter_d",
                label = "Effect size (d):",
                value = 0,
                min = 0,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "d_enter_n1",
                label = "Group 1 N:",
                value = 0,
                min = 0,
                max = NA,
                step = NA,
                width = NULL
              ),
              numericInput(
                inputId = "d_enter_n2",
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
                inputId = "d_enter_lower",
                label = "Should the effect size be positive:",
                choices = c(TRUE, FALSE),
                selected = TRUE,
                width = NULL
              ),

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
              valueBoxOutput("summary_d"),
              valueBoxOutput("summary_d_low_one_central"),
              valueBoxOutput("summary_d_low_two_central"),
              valueBoxOutput("summary_d_low_one_non"),
              valueBoxOutput("summary_d_low_two_non")
            ) # box close
          ) # fluid row close
  ) # tab close
