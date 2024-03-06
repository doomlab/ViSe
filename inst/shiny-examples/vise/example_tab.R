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
                     (via <i>d</i> = Mdifference / SDTotal), we
                     divided the reported unadjusted and adjusted mean differences
                     for internalising by the respective standard
                     deviations of the total sample (8.29 in this example). It may be preferable to
                     use the adjusted <i>d</i> for sensitivity analysis if it
                     already accounts for some common causes. The difference between
                     the unadjusted and the adjusted <i>d</i> helps to narrow down
                     how large the bias due to other common sources might be.")),
              tableOutput("data_kable"),

              p(HTML("The sensitivity plot only depends on lower bound here as the
              reported lower confidence
                interval <b>l</b>. However, we could also
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
              p(HTML("The bias due to unadjusted common causes must be less than l.
              To determine bias, two unknown quantities must be specified. The first
              is the difference in predisposition to internalising problems between
              individuals with and without child neglect. This quantity can also be
              specified by <i>d</i> or several other measures of effect size. <p>
              On the convert between effect sizes tab, you can
                     enter different effect sizes and their names to
                     show how different effect size values are related to
                     each other, if you are not familar with one specific
                     effect size. Here we can see that our effect size converts
                     to a correlation of .16 or 6 participants for Number
                     Needed to Treat.")),
              img(src = "shiny_fig_3.png", width = "100%")
            ), # close box

            box(
              title = tags$b("Visualize effect size sensitivity to bias"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p(HTML("On the last tab we enter both quantities: 1. the above <i>d</i> (or any
                     other measure of effect into which <i>d</i> can be converted), and 2.
                     the correlation <i>r</i> between the predisposition to internalising
                     problems in childhood and the internalising problems (observed at
                     age 21). ('Bias' is simply the product <i>d</i> * <i>r</i>). Enter
                     plausible values for both quantities to see in the final sensitivity
                     plot which combinations of them support a causal effect of child
                     neglect on internalising problems at age 21:")),
              img(src = "shiny_fig_4.png", width = "100%"),
              p(HTML("Here the plot includes the intersection of <i>d</i> values and <i>r</i>
                     values as points to determine if they are in the range of
                     expected values that would denote a causal effect. The shaded
                     region shows the areas that would support the effect and
                     the points are the combinations of effect size and
                     correlation you noted.")),

              img(src = "shiny_fig_5.png", width = "100%"),
            ), # close box

          ) # fluid row
        ) #close page
