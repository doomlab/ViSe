example_tab <-
  tabItem(tabName = "example_tab",
          fluidRow(
            # Instructions for this tab ----
            box(
              title = tags$b("Sensitivity Example Walk Through"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("To demonstrate the use of visual sensitivity analysis,
                we use the effect of child maltreatment on the extent of
                mental health problems in terms of internalising and
                externalising behaviour. The study of Kisely and colleagues
                (Kisely et al., 2018) was based on a general population
                sample in Brisbane, Australia, and compared 3554 mother-child
                pairs without 'substantiated child maltreatment' to, for
                example, 73 pairs with child neglect. Note that the results
                vary across different types of maltreatment assessed, we choose
                child neglect because its results (a smaller but still
                considerable l after adjustment) are particularly suited to
                illustrating sensitivity analysis. Maltreatment was assessed
                'by linkage to state child protection agency data'.
                Internalising and externalising behaviours were measured
                using the Youth Self-Report (YSR) scale (Achenbach & Rescorla,
                2001) at around the age of 21. The study reports unadjusted
                mean differences and mean differences adjusted for ‘gender,
                parental ethnicity, maternal age, family income, maternal
                relationship status, maternal education, youth income level,
                youth education level, youth marital status’ (e.g., likely
                based on ordinary least squares regression, but the paper
                does not specify)."),

            ), # close box

            box(
              title = tags$b("Sensitivity Example Walk Through"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(HTML("To obtain the estimates and two-tailed 95% confidence intervals
                     on the <i>d</i> scale
                     (via d = Mdifference / SDTotal , similar to Glass’ Δ), we
                     divided the reported unadjusted and adjusted mean differences
                     for internalising and externalising by the respective standard
                     deviations of the total sample"))

            ) # close box

          ) # fluid row
        ) #close page
