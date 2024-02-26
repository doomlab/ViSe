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
              p("Information on this page is static with pictures from other
                tabs to show you how to work the app. Please click on the other
                tabs to enter your own values for calculation.")

            ), # close box

            box(
              title = tags$b("Calculate the Effect Size and Confidence Interval"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(HTML("To obtain the estimates and two-tailed 95% confidence intervals
                     on the <i>d</i> scale
                     (via <i>d</i> = Mdifference / SDTotal , similar to Glass' Δ), we
                     divided the reported unadjusted and adjusted mean differences
                     for internalising and externalising by the respective standard
                     deviations of the total sample (8.29 in this example).")),
              tableOutput("data_kable"),

              p(HTML("We can use the lower bound here as the reported lower confidence
                interval <b>l</b> for our sensitivity plot. However, we could also
                     use the calculate standardized group difference tab to give
                     us the lower limit of the effect size using a one-tailed
                     confidence interval. Pictures of the required data entry
                     are shown below.")),
              p("You must enter sample sizes, alpha, and if you expect the
                effect size to be positive:"),
              img(src = "shiny_fig_1.png", width = "100%"),
              p("In this example, we have the effect size, so we enter that
                value into the box for d and see the output that indicates
                we could use .20 for a two-tailed interval or .24 for a
                one-tailed interval."),
              img(src = "shiny_fig_2.png", width = "100%")
              ), # close box

            box(
              title = tags$b("Convert between effect sizes and make a specification plot"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(HTML("On the convert between effect sizes tab, you can
                     enter different effect sizes and their names to
                     show how different effect size values are related to
                     each other, if you are not familar with one specific
                     effect size. Here we can see that our effect size converts
                     to a correlation of .21 or 4 participants for Number
                     Needed to Treat.")),
              img(src = "shiny_fig_3.png", width = "100%")
            ), # close box

            box(
              title = tags$b("Visualize effect size sensitivity to bias"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(HTML("On the last tab, we can create several visualization measures
                     including the overlap of two distributions for <i>d</i>, the
                     correlation between our predictor and measured variable <i>r</i>,
                     and the sensitivity plot of where our effects might denote
                     a causal effect. To create this image, you must enter the
                     following: lower bound of effect size d, proposed correlation
                     values, and at least one other effect size measure set of values:")),
              img(src = "shiny_fig_4.png", width = "100%"),
              p(HTML("Here the plot includes the intersection of d values and r
                     values as points to determine if they are in the range of
                     expected values that would denote a causal effect. The shaded
                     region shows the areas that would support the effect and
                     the points are the combinations of effect size and
                     correlation you noted. We can see all our combinations
                     would support a causal effect by being in the shaded region.")),
              img(src = "shiny_fig_5.png", width = "100%"),
            ), # close box

          ) # fluid row
        ) #close page
