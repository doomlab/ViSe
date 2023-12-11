data_tab <-
  tabItem(tabName = "data_tab",

          # Enter Data ---------------------------------------------------
          fluidRow(
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
              DTOutput("file_view")
            ), #box

            # Visualize effect sizes -------------------------------------------------------------
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
              plotOutput("convert_d_data")
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
              p(""),
              h3(strong(textOutput("visualize_c_data_warning"),
                   style = "color:red; font-size:20px; font-family:arial; font-style:italic;")),
              plotOutput("visualize_c_data")
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
                groups if we are unsure of what the effect size might be in future
                data."),
              numericInput(
                inputId = "enter_d_data",
                label = "Proposed d value:",
                value = 0,
                min = -3,
                max = 3,
                step = NA,
                width = NULL
              ),
              plotOutput("estimate_d_data")
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
              inputId = "enter_r_data",
              label = "Proposed r value:",
              value = 0,
              min = -1,
              max = 1,
              step = NA,
              width = NULL
            ),
            plotOutput("estimate_r_data")
          ), #box

          # Visualize c map -------------------------------------------------------------
          box(
            title = tags$b("Visualization of Potential Options"),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            p(""),
            p("We can use ", strong("visualize_c_map()"), "what values might show
              an effect. After using the two plots above, values for ",
              em("d"), " and ,", em("r"), "below ", strong("separated by commas.")),
            p(""),
            h3(strong(textOutput("visualize_c_map_data_warning"),
                      style = "color:red; font-size:20px; font-family:arial; font-style:italic;")),
            textInput(inputId = "d_values_data",
                      label = "Enter d values",
                      value = ""),
            textInput(inputId = "r_values_data",
                      label = "Enter r values",
                      value = ""),
            plotlyOutput("visual_c_map_data")
          ), #box


      ) #fluidrow
    ) #close page
