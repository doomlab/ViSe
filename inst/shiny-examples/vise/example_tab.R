example_tab <-
  tabItem(tabName = "example_tab",
          fluidRow(
            # Instructions for this tab ----
            box(
              title = tags$b("Sensitivity Example WalkThrough"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("Example coming soon!"),

            ) # close box
          ) # fluid row
        ) #close page
