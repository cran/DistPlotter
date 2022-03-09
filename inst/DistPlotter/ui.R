
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(DT)
library(stringi)
library(colourpicker)


navbarPage("Dist Plotter",

  ##############################################################################
  ################################ Home tab panel ##############################
  ##############################################################################

  tabPanel("Home", icon = icon("home"),

    useShinyjs(),


    # calculate browser width and height
    tags$script(
      "var dimension = [0, 0];
      $(document).on('shiny:connected', function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
      });
      $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange('dimension', dimension);
      });"
    ),


    # tweak style of popup modals
    tags$head(tags$style(HTML(
      ".modal-lg {width: 40%; margin-top: 0.1%; margin-left: 0.1%;}"
    ))),

    tags$head(tags$style(HTML(
      ".modal-sm {width: 40%; margin-top: 0.5%; margin-left: 0.5%;}"
    ))),

    tags$head(tags$style(HTML(".modal-header {display: none;}"))),

    #tags$head(tags$style(HTML(".modal-footer {display: none;}"))),


    # update hr() border and color
    tags$head(tags$style(HTML(
      "hr {border-top: 1px solid; margin-top: 1px; margin-bottom: 10px;}"
    ))),


    # remove progress bar for fileInput()
    tags$style(".shiny-file-input-progress {display: none;}"),


    # delay textAreaInput reactives; in 'delay,' 1000 = 1 second
    tags$script(
      "var slowTextAreaInputBinding = new Shiny.InputBinding();
        $.extend(slowTextAreaInputBinding, {
          find: function(scope) {
            return $(scope).find('textArea');
          },
          getId: function(el) {
            return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
          },
          getValue: function(el) {
            return el.value;
          },
          setValue: function(el, value) {
            el.value = value;
          },
          subscribe: function(el, callback) {
            $(el).on('keyup.textAreaInputBinding input.textAreaInputBinding',
              function(event) {
                callback(true);
              });
              $(el).on('change.textAreaInputBinding', function(event) {
                callback(false);
              });
          },
          unsubscribe: function(el) {
            $(el).off('.textAreaInputBinding');
          },
          getRatePolicy: function() {
            return {
              policy: 'debounce',
              delay: 1000
            };
          }
        });
      Shiny.inputBindings.register(slowTextAreaInputBinding, 'slowTextAreaInput');"
    ),


    # remove arrow increments from numeric inputs
    tags$style(HTML(
      "input[type=number]::-webkit-inner-spin-button,
       input[type=number]::-webkit-outer-spin-button {
         -webkit-appearance: none;
         -moz-appearance: none;
         appearance: none;
         margin: 0;
      }"
    )),


    # set padding on left side of bulleted list
    tags$head(tags$style(HTML("ul {padding-left: 14px;}"))),


    column(12,
      h1(style = "color: #0073e6; font-weight: bold; text-align: center;",
        "Description of Tabs"
      ),
      br()
    ),


    column(4,
      column(1, ""),

      column(10,
        fluidRow(
          HTML("<h3><span style = 'color: #0073e6; font-weight: bold;'><center>
            Plot a Single Distribution</center></span>"
          ),

          br(),

          h4("Plot general univariate distributions based on shape and
            univariate probability distributions from common families:"
          ),

          tags$ul(
            tags$li(HTML("<h4>a density curve of a variable based on shape")),
            tags$li(HTML("<h4>a histogram of the PMF of a discrete random
              variable"
            )),
            tags$li(HTML("<h4>a density curve for the PDF of a continuous random
              variable"
            )),
            tags$li(HTML("<h4>shade area(s) and customize plots when finding
              general probabilities or teaching inferential concepts such as
              p-values and confidence levels"
            ))
          )
        )
      ),

      column(1, "")
    ),

    column(4,
      HTML("<h3><span style = 'color: #0073e6; font-weight: bold;'><center>
        Plot Multiple Distributions</center></span>"
      ),

      br(),

      h4("Plot multiple superimposed density curves for comparing continuous
        univariate probability distributions from the same or different
        families."
      )
    ),

    column(4,
      column(1, ""),

      column(10,
        fluidRow(
          HTML("<h3><span style = 'color: #0073e6; font-weight: bold;'><center>
            Include Your Data</center></span>"
          ),

          br(),

          h4("Plot your own data, superimpose a density curve, and shade an area
            of interest under the curve."
          )
        )
      ),

      column(1, "")
    )
  ),




  ##############################################################################
  ##################### Plot a Single Distribution tab panel ###################
  ##############################################################################

  tabPanel("Plot a Single Distribution",
    icon = img(src = "Icon_Single_Dist.png", height = "9px"),

    # well panel -- inputs
    column(3,
      wellPanel(
        radioButtons("pop_dist", "Distribution",
          choices = c(
            "Select Shape" = "basic",
            "Select Distribution" = "advanced"
          )
        ),

        conditionalPanel(
          condition = "input.pop_dist == 'basic'",

          selectInput("pop_shape", "Shape",
            choices = c("Bell-shaped", "Skewed", "Bimodal", "Uniform"),
            selected = "Bell-shaped"
          ),


          conditionalPanel(
            condition = "input.pop_shape == 'Bell-shaped'",

            splitLayout(
              numericInput("norm_mean_basic", "Mean", value = 0),
              numericInput("norm_sd_basic", "Std. Dev.", value = 1)
            )
          ),


          conditionalPanel(
            condition = "
              input.pop_shape == 'Skewed' | input.pop_shape == 'Uniform'",

            splitLayout(
              numericInput("x_min_basic", "Min", value = 0),
              numericInput("x_max_basic", "Max", value = 1)
            ),

            conditionalPanel(
              condition = "input.pop_shape == 'Skewed'",

              splitLayout(
                sliderInput("skewed_shape1", "Shape 1", min = 1, max = 25,
                  step = 1, value = 5, ticks = FALSE
                ),
                sliderInput("skewed_shape2", "Shape 2", min = 1, max = 25,
                  step = 1, value = 2, ticks = FALSE
                )
              )
            )
          ),


          conditionalPanel(
            condition = "input.pop_shape == 'Bimodal'",

            splitLayout(
              numericInput("bi_mean1", "Mean of subpop. 1", value = 5),
              numericInput("bi_sd1", "Std. Dev. of subpop. 1", value = 1)
            ),

            splitLayout(
              numericInput("bi_mean2", "Mean of subpop. 2", value = 10),
              numericInput("bi_sd2", "Std. Dev. of subpop. 2", value = 1)
            )
          )
        ),


        conditionalPanel(
          condition = "input.pop_dist == 'advanced'",

          radioButtons("dist_type", "Distribution Type",
            choices = c("Continuous", "Discrete"),
            selected = "Continuous"
          ),


          conditionalPanel(
            condition = "input.dist_type == 'Continuous'",

            selectInput("cont_fam", "Family",
              choices = c("Beta", "Cauchy", "Chi-square", "Exponential", "F",
                "Gamma", "Laplace", "Log-normal", "Normal", "t",
                "Uniform (continuous)", "Weibull"
              ),
              selected = "Normal"
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Beta'",

              splitLayout(
                numericInput("beta_shape1", "Shape 1", value = 2),
                numericInput("beta_shape2", "Shape 2", value = 2)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Cauchy'",

              splitLayout(
                numericInput("cauchy_loc", "Location", value = 0),
                numericInput("cauchy_scale", "Scale", value = 1)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Chi-square'",

              numericInput("chisq_df", "DF", value = 5, width = "50%")
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Exponential'",

              numericInput("exp_rate", "Rate", value = 5, width = "50%")
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'F'",

              splitLayout(
                numericInput("f_df1", "DF 1", value = 5),
                numericInput("f_df2", "DF 2", value = 5)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Gamma'",

              p("Note: To input a scale instead of a rate, recall rate =
                1/scale."
              ),

              splitLayout(
                numericInput("gamma_shape", "Shape", value = 5),
                numericInput("gamma_rate", "Rate", value = 5)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Laplace'",

              splitLayout(
                numericInput("laplace_loc", "Location", value = 0),
                numericInput("laplace_scale", "Scale", value = 1)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Log-normal'",

              splitLayout(
                numericInput("lognorm_mean", "Mean", value = 0),
                numericInput("lognorm_sd", "Std. Dev.", value = 1)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Normal'",

              splitLayout(
                numericInput("norm_mean", "Mean", value = 0),
                numericInput("norm_sd", "Std. Dev.", value = 1)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 't'",

              numericInput("t_df", "DF", value = 10, width = "50%")
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Uniform (continuous)'",

              splitLayout(
                numericInput("uni_min", "Minimum", value = 0),
                numericInput("uni_max", "Maximum", value = 1)
              )
            ),


            conditionalPanel(
              condition = "input.cont_fam == 'Weibull'",

              splitLayout(
                numericInput("weib_shape", "Shape", value = 1),
                numericInput("weib_scale", "Scale", value = 1)
              )
            )
          ),


          conditionalPanel(
            condition = "input.dist_type == 'Discrete'",

            selectInput("disc_fam", "Family",
              choices = c(
                "Bernoulli", "Binomial", "Geometric", "Hypergeometric",
                "Negative Binomial", "Poisson", "Uniform (discrete)"
              ),
              selected = "Bernoulli"
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Bernoulli'",

              numericInput("bern_p", "Probability of success (p)", value = 0.5)
            ),


            conditionalPanel(
              condition ="input.disc_fam == 'Binomial'",

              numericInput("bin_n", "Number of trials (n)", value = 10),
              numericInput("bin_p", "Probability of success (p)", value = 0.5)
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Geometric'",

              numericInput("geom_p", "Probability of success (p)", value = 0.5)
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Hypergeometric'",

              numericInput("hyper_N", "Population size", value = 50),
              numericInput("hyper_K", "Number of successes in pop.",
                value = 10
              ),
              numericInput("hyper_n", "Number of draws", value = 30)
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Negative Binomial'",

              p("Note: X represents the number of failures before the rth
                success."
              ),

              numericInput("negbin_r", "Total number of successes desired (r)",
                value = 5
              ),

              numericInput("negbin_p", "Probability of success (p)",
                value = 0.5
              )
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Poisson'",

              numericInput("pois_rate", "Rate (lambda)", value = 5)
            ),


            conditionalPanel(
              condition = "input.disc_fam == 'Uniform (discrete)'",

              numericInput("uni_n", "Number of possible values", value = 6)
            )
          )
        ),


        column(12, align = "center",
          actionButton("make_plot_common", "Make Plot",
            class = "btn btn-primary", icon = icon("bolt")
          )
        ),

        br()
      )
    ),


    # directions and plots
    column(9,

      # download plot button
      column(12, align = "right",
        actionButton("download_plot_common", "Download Plot",
          class = "btn btn-primary", icon = icon("download")
        ),

        bsModal("download_popup_common", NULL, "download_plot_common",
          size = "small", uiOutput("download_popup_common")
        )
      ),


      column(3, ""),


      column(8,  plotOutput("plot_common"))
    ),

    column(12,

      br(),

      wellPanel(
        fluidRow(
          column(12,

            # change axis aspects
            column(4,
              checkboxInput("change_axis_aspects_common",
                strong("Change axis aspects")
              ),

              conditionalPanel(
                condition = "input.change_axis_aspects_common",

                column(6,
                  strong("Leave the input blank if you don't want to change the
                    respective aspect."
                  ),

                  br(),

                  fluidRow(
                    br(),

                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("X min:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("x_min_common", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("X max:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("x_max_common", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("X tick mark increment*:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(
                        style = "display: inline-block;",

                        textInput("x_tick_inc_common", NULL, width = "100px")
                      ),

                      p("*requires min and max inputs")
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("Y max:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("y_max_common", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("Text size:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(
                        style = "display: inline-block;",

                        numericInput("axis_text_size_common", NULL, value = 24,
                          width = "100px"
                        )
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("Label size:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(
                        style = "display: inline-block;",

                        numericInput("axis_lab_size_common", NULL, value = 28,
                          width = "100px"
                        )
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      textInput("x_lab_common", "X axis label", value = "")
                    )
                  ),

                  fluidRow(
                    column(12, align = "center",
                      actionButton("x_lab_update_common", "Update Label",
                        class = "btn btn-success", icon = icon("sync-alt")
                      )
                    ),

                    br(),
                    br()
                  ),


                  fluidRow(
                    column(12,
                      textInput("y_lab_common", "Y axis label", value = "")
                    )
                  ),

                  fluidRow(
                    column(12, align = "center",
                      actionButton("y_lab_update_common", "Update Label",
                        class = "btn btn-success", icon = icon("sync-alt")
                      )
                    ),

                    br(),
                    br()
                  )
                ),


                column(6,
                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis line:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      stri_dup(intToUtf8(160), 1)
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_line_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")  # add "Keep" to right-hand side of switch
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis label:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_lab_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis ticks:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_text_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis line:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      stri_dup(intToUtf8(160), 1)
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_line_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis label:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_lab_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis ticks:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_text_common", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  )
                )
              )
            ),


            column(2,

              # change density curve aspects
              conditionalPanel(
                condition = "
                  input.pop_dist == 'basic' |
                  (input.pop_dist == 'advanced' & input.dist_type == 'Continuous')",

                checkboxInput("density_aspects_common",
                  strong("Change curve aspects")
                ),

                conditionalPanel(
                  condition = "input.density_aspects_common",

                  column(12,
                    div(style = "display: inline-block;",
                      strong(paste0("Curve color:", stri_dup(intToUtf8(160), 3)))
                    ),

                    div(style = "display: inline-block;",
                      colourInput("curve_color_common", NULL, value = "black",
                        showColour = "background"
                      ),

                      tags$head(tags$style(
                        type = "text/css", "#curve_color_common{width: 100px;}"
                      ))
                    )
                  ),


                  column(12,
                    div(style = "display: inline-block; vertical-align: -1.75em;",
                      strong(paste0("Curve thickness:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      sliderInput("curve_lwd_common", NULL, min = 0, max = 3,
                        step = 0.1, value = 0.5, width = "150px"
                      )
                    )
                  )
                )
              ),


              # change main fill color
              conditionalPanel(
                condition = "input.pop_dist == 'advanced' &
                  input.dist_type == 'Discrete'",

                checkboxInput("fill_aspects_common",
                  strong("Change main fill color")
                ),

                conditionalPanel(
                  condition = "input.fill_aspects_common",

                  column(12,
                    div(style = "display: inline-block;",
                      strong(paste0("Main fill color:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      colourInput("fill_color_common", NULL, value = "grey",
                        showColour = "background"
                      ),

                      tags$head(tags$style(
                        type = "text/css", "#fill_color_common{width: 100px;}"
                      ))
                    )
                  )
                )
              )
            ),


            column(4,

              # shade area(s)
              conditionalPanel(
                condition = "input.pop_dist == 'advanced'",

                column(4,
                  checkboxInput("color_area_common", strong("Shade area(s)")),

                  conditionalPanel(
                    condition = "input.color_area_common",

                    conditionalPanel(
                      condition = "input.dist_type == 'Continuous'",

                      radioButtons("area_common_cont", "Area to shade",
                        choices = c("Left", "Right", "Between", "Outside/Tails")
                      )
                    ),


                    conditionalPanel(
                      condition = "input.dist_type == 'Discrete'",

                      radioButtons("area_common_disc", "Area to shade",
                        choices = c(
                          "One bar only" = "one",
                          "Left (inclusive)" = "left",
                          "Right (inclusive)" = "right",
                          "Between (inclusive)" = "between",
                          "Outside/Tails (inclusive)" = "outside"
                        )
                      )
                    )
                  )
                )
              ),


              # add line segments at statistic(s)
              conditionalPanel(
                condition = "input.pop_dist == 'basic' &
                  input.pop_shape != 'Bimodal'",

                checkboxInput("mark_stats", strong("Add lines at statistics")),

                conditionalPanel(
                  condition = "input.mark_stats",

                  checkboxInput("stats_mean_median", "Mean and/or median"),

                  conditionalPanel(
                    condition = "input.pop_shape == 'Bell-shaped'",

                    checkboxInput("stats_empirical", "Empirical rule")
                  ),

                  conditionalPanel(
                    condition = "input.stats_mean_median",

                    column(6,
                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("Add line at mean:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          stri_dup(intToUtf8(160), 2)
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("add_line_mean", "No",
                            status = "primary", right = FALSE, inline = TRUE
                          ),
                          span("Yes")  # add "Yes" to right-hand side of switch
                        )
                      ),

                      column(12,
                        div(style = "display: inline-block;",
                          strong(paste0("Line color:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          colourInput("line_color_mean", NULL, value = "black",
                            showColour = "background"
                          ),

                          tags$head(tags$style(
                            type = "text/css", "#line_color_mean{width: 100px;}"
                          ))
                        )
                      ),

                      column(12,
                        div(style = "display: inline-block; vertical-align: -1.75em;",
                          strong(paste0("Line thickness:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          sliderInput("line_lwd_mean", NULL, min = 0,
                            max = 3, step = 0.1, value = 0.5, width = "150px"
                          )
                        )
                      )
                    ),


                    column(6,
                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("Add line at median:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("add_line_median", "No",
                            status = "primary", right = FALSE, inline = TRUE
                          ),
                          span("Yes")  # add "Yes" to right-hand side of switch
                        )
                      ),

                      column(12,
                        div(style = "display: inline-block;",
                          strong(paste0(
                            "Line color:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          colourInput("line_color_median", NULL, value = "black",
                            showColour = "background"
                          ),

                          tags$head(tags$style(
                            type = "text/css", "#line_color_median{width: 100px;}"
                          ))
                        )
                      ),

                      column(12,
                        div(style = "display: inline-block; vertical-align: -1.75em;",
                          strong(paste0(
                            "Line thickness:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          sliderInput("line_lwd_median", NULL, min = 0,
                            max = 3, step = 0.1, value = 0.5, width = "150px"
                          )
                        )
                      )
                    )
                  )
                )
              ),


              column(8,
                conditionalPanel(
                  condition = "input.color_area_common",

                  conditionalPanel(
                    condition = "
                      input.pop_dist == 'basic' |
                      (input.pop_dist == 'advanced' &
                        input.dist_type == 'Continuous'
                      )
                    ",

                    br(),
                    br(),

                    conditionalPanel(
                      condition = "input.area_common_cont == 'Left'",

                      div(style = "display: inline-block;",
                        strong(paste0("Shade to the left of:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        numericInput("cutoff_left_common_cont", NULL,
                          value = NA, width = "100px"
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_cont == 'Right'",

                      div(style = "display: inline-block;",
                        strong(paste0("Shade to the right of:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        numericInput("cutoff_right_common_cont", NULL,
                          value = NULL, width = "100px"
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_cont == 'Between'",

                      fluidRow(
                        column(12,
                          # input names flipped since right first then left
                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the right of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_between_left_common_cont",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        ),

                        column(12,
                          # input names flipped since right first then left
                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the left of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_between_right_common_cont",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_cont == 'Outside/Tails'",

                      fluidRow(
                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the left of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_outside_left_common_cont",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        ),

                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the right of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_outside_right_common_cont",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        )
                      )
                    ),


                    div(
                      style = "display: inline-block;",

                      strong(paste0("Add vertical segment at cutoff(s):",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("add_cutoff_common", "No",
                        status = "primary", right = FALSE, inline = TRUE
                      ),
                      span("Yes")  # add "Yes" to right-hand side of switch
                    )
                  ),


                  conditionalPanel(
                    condition = "input.dist_type == 'Discrete'",

                    conditionalPanel(
                      condition = "input.area_common_disc == 'one'",

                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Shade at:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        numericInput("bar_value", NULL, value = NULL,
                          width = "100px"
                        )
                      )
                    ),


                    conditionalPanel(
                      condition = "input.area_common_disc == 'left'",

                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Shade to the left of:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        numericInput("cutoff_left_common_disc", NULL,
                          value = NULL, width = "100px"
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_disc == 'right'",

                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Shade to the right of:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        numericInput("cutoff_right_common_disc", NULL,
                          value = NULL, width = "100px"
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_disc == 'between'",

                      fluidRow(
                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0(
                              "Shade to the right of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_between_left_common_disc",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        ),

                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0(
                              "Shade to the left of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_between_right_common_disc",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        )
                      )
                    ),

                    conditionalPanel(
                      condition = "input.area_common_disc == 'outside'",

                      fluidRow(
                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0(
                              "Shade to the left of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_outside_left_common_disc",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        ),

                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0(
                              "Shade to the right of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_outside_right_common_disc",
                              NULL, value = NULL, width = "100px"
                            )
                          )
                        )
                      )
                    )
                  )
                ),


                conditionalPanel(
                  condition = "input.color_area_common",

                  fluidRow(
                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Shading color:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        colourInput("area_color_common", NULL, value = "black",
                          showColour = "background"
                        ),

                        tags$head(tags$style(
                          type = "text/css", "#area_color_common{width: 100px;}"
                        ))
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(style = "display: inline-block; vertical-align: -1.75em;",
                        strong(paste0(
                          "Opacity level:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        sliderInput("area_opac_common", NULL, min = 0, max = 1,
                          step = 0.05, value = 1, width = "200px"
                        )
                      )
                    )
                  )
                )
              )
            ),


            # add annotation(s)
            column(1,
              actionButton("annotate_common", "Add Annotations",
                class = "btn btn-success", icon = icon("edit")
              ),

              bsModal("annotate_popup_common", NULL, "annotate_common",
                size = "large", uiOutput("annotate_popup_common")
              ),


              # need below to reset inputs
              tags$script("
                Shiny.addCustomMessageHandler('reset_input', function(variableName) {
                  Shiny.onInputChange(variableName, null);
                });
              ")
            )
          )
        )
      )
    )
  ),




  ##############################################################################
  ##################### Plot Multiple Distributions tab panel ##################
  ##############################################################################

  tabPanel("Plot Multiple Distributions",
    icon = img(src = "Icon_Multiple_Dists.png", height = "9px"),

    # well panel
    column(5,
      uiOutput("multiple_ui")
    ),


    # multiple plot
    column(7,

      # download plot button
      column(12, align = "right",
        actionButton("download_plot_mult", "Download Plot",
          class = "btn btn-primary", icon = icon("download")
        ),

        bsModal("download_popup_mult", NULL, "download_plot_mult",
          size = "small", uiOutput("download_popup_mult")
        )
      ),

      fluidRow(
        column(12,

          column(1, ""),

          column(9, plotOutput("plot_mult")),
        )
      ),


      wellPanel(
        fluidRow(
          column(12,

            # change axis aspects
            column(8,
              checkboxInput("change_axis_aspects_mult",
                strong("Change axis aspects")
              ),

              conditionalPanel(
                condition = "input.change_axis_aspects_mult",

                column(6,
                  strong("Leave the input blank if you don't want to change the
                    respective aspect."
                  ),

                  br(),

                  fluidRow(
                    br(),

                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("X min:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("x_min_mult", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("X max:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("x_max_mult", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("X tick mark increment*:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(
                        style = "display: inline-block;",

                        textInput("x_tick_inc_mult", NULL, width = "100px")
                      ),

                      p("*requires min and max inputs")
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0("Y max:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(style = "display: inline-block;",
                        textInput("y_max_mult", NULL, width = "100px")
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("Text size:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(
                        style = "display: inline-block;",

                        numericInput("axis_text_size_mult", NULL, value = 24,
                          width = "100px"
                        )
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      div(
                        style = "display: inline-block;",

                        strong(paste0("Label size:", stri_dup(intToUtf8(160), 3)))
                      ),

                      div(
                        style = "display: inline-block;",

                        numericInput("axis_lab_size_mult", NULL, value = 28,
                          width = "100px"
                        )
                      )
                    )
                  ),


                  fluidRow(
                    column(12,
                      textInput("x_lab_mult", "X axis label", value = "")
                    )
                  ),

                  fluidRow(
                    column(12, align = "center",
                      actionButton("x_lab_update_mult", "Update Label",
                        class = "btn btn-success", icon = icon("sync-alt")
                      )
                    ),

                    br(),
                    br()
                  ),


                  fluidRow(
                    column(12,
                      textInput("y_lab_mult", "Y axis label", value = "")
                    )
                  ),

                  fluidRow(
                    column(12, align = "center",
                      actionButton("y_lab_update_mult", "Update Label",
                        class = "btn btn-success", icon = icon("sync-alt")
                      )
                    ),

                    br(),
                    br()
                  ),
                ),


                column(6,

                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis line:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      stri_dup(intToUtf8(160), 1)
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_line_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis label:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_lab_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("X axis ticks:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_x_axis_text_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis line:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(style = "display: inline-block;",
                      stri_dup(intToUtf8(160), 1)
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_line_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis label:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_lab_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  ),


                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Y axis ticks:",
                        stri_dup(intToUtf8(160), 3)
                      ))
                    ),

                    div(
                      style = "display: inline-block;",

                      materialSwitch("keep_y_axis_text_mult", label = "Remove",
                        value = TRUE, status = "primary", right = FALSE,
                        inline = TRUE
                      ),
                      span("Keep")
                    )
                  )
                )
              )
            ),


            # change curve thickness and/or legend aspects
            column(4,

              # change curve thickness
              checkboxInput("change_curve_mult",
                strong("Change curve thickness")
              ),

              conditionalPanel(
                condition = "input.change_curve_mult",

                column(12,
                  div(style = "display: inline-block; vertical-align: -1.75em;",
                    strong(paste0(
                      "Curve thickness:",
                      stri_dup(intToUtf8(160), 3)
                    ))
                  ),

                  div(style = "display: inline-block;",
                    sliderInput("curve_lwd_mult", NULL, min = 0, max = 2,
                      step = 0.1, value = 1, width = "150px"
                    )
                  )
                )
              ),


              # change legend aspects
              checkboxInput("change_legend_aspects_mult",
                strong("Change legend aspects")
              ),

              conditionalPanel(
                condition = "input.change_legend_aspects_mult",

                strong("Leave the input blank if you don't want to change
                  the respective aspect."
                ),

                br(),
                br(),

                fluidRow(
                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Text size:", stri_dup(intToUtf8(160), 3)))
                    ),

                    div(
                      style = "display: inline-block;",

                      numericInput("legend_text_size_mult", NULL, value = 24,
                        width = "100px"
                      )
                    )
                  )
                ),


                fluidRow(
                  column(12,
                    div(
                      style = "display: inline-block;",

                      strong(paste0("Label size:", stri_dup(intToUtf8(160), 3)))
                    ),

                    div(
                      style = "display: inline-block;",

                      numericInput("legend_lab_size_mult", NULL, value = 28,
                        width = "100px"
                      )
                    )
                  )
                ),


                textInput("legend_lab_mult", "Legend label", value = "")
              )
            )
          )
        )
      )
    )
  ),




  ##############################################################################
  ######################### Include Your Data tab panel ########################
  ##############################################################################

  tabPanel("Include Your Data", icon = icon("table"),

    tabsetPanel(type = "tabs",

      # Input Data and Users tab panel
      tabPanel("Input Data",

        br(),


        # well panel -- inputs
        column(3,
          wellPanel(
            radioButtons("input_type", "Data Input Method",
              choices = c("Upload File" = "upload", "Manually" = "manually"),
              selected = "upload"
            ),

            tags$hr(),

            conditionalPanel(
              condition = "input.input_type == 'manually'",

              textAreaInput("user_data", "Either copy and paste a column from a
                spreadsheet (such as Excel) or type numbers in the box below,
                separating each by a comma.",
                placeholder = "0, 2, 8"
              )
            ),

            conditionalPanel(
              condition = "input.input_type == 'upload'",

              checkboxInput("view_file_types", strong(style = "color: green;",
                "Check box to view a list of compatible file types.")
              ),

              conditionalPanel(
                condition = "input.view_file_types",

                p("The app accepts files with the following extensions when
                  uploading data: .csv (CSV), .xlsx (Excel), .xls (Excel), .json
                  (JSON), .mtp (Minitab), .mat (MATLAB), .R (R), .RData (R),
                  .rda (R), .txt (Plain Text), .sas7bdat (SAS), .sav (SPSS), and
                  .dta (Stata)."
                ),
              ),


              fileInput("data_file", "Click 'Browse...' and select the desired
                file.",
                multiple = FALSE
              ),


              conditionalPanel(
                condition = "output.na_input_status",    # from server

                checkboxInput("no_header", strong(style = "color: green;",
                  "Check box if the data set does NOT contain variable/column
                  names in the first row.")
                ),

                checkboxInput("missing_input", strong(style = "color: green;",
                  "Check box if the data set contains missing data."),
                  value = FALSE
                ),

                conditionalPanel(
                  condition = "input.missing_input",

                  p("If the missing values in your data set are represented by
                    empty cells, you may uncheck the box. If they are
                    represented by something else (e.g., a period), specify the
                    symbol, character, etc. that represents the missing values.
                    If there are multiple, separate each by a comma. Any entries
                    with missing data will appear as empty cells."
                  ),

                  textInput("denote_missing_input", NULL, value = ".",
                    width = "25%"
                  )
                )
              ),


              column(12, align = "center",
                actionButton("upload_dataset", "Upload Data Set",
                  class = "btn btn-primary", icon = icon("upload")
                )
              ),

              br()
            )
          )
        ),


        # display data set
        column(7,
          conditionalPanel(
            condition = "input.input_type == 'upload' & input.upload_dataset",

            column(12, align = "center",

              h4(style = "color: green; font-weight: bold;", "Only variables
                with numerical data are displayed below."
              ),

              br()
            ),


            DT::DTOutput("data_table_upload")
          ),

          conditionalPanel(
            condition = "input.input_type == 'manually'",

            tableOutput("table_manual")
          )
        )
      ),


      # Make Plot tabpanel
      tabPanel("Make Plot",

        br(),

        # well panel -- inputs
        column(3,
          wellPanel(
            conditionalPanel(
              condition = "input.input_type == 'upload'",

              selectInput("select_var", "Variable of Interest",
                choices = "", selected = ""
              )
            ),


            column(12, align = "center",
              actionButton("make_plot_user", "Make Plot",
                class = "btn btn-primary", icon = icon("bolt")
              )
            ),

            br(),
            br()
          ),


          wellPanel(
            radioButtons("bin_spec", "Bin Specification",
              choices = c("Bin Width", "Number of Bins"),
              selected = "Bin Width"
            ),

            conditionalPanel(
              condition = "input.bin_spec == 'Bin Width'",

              div(style = "display: inline-block; vertical-align: center;",
                strong(paste0("Bin Width:", stri_dup(intToUtf8(160), 3)))
              ),

              div(style = "display: inline-block;",
                textInput("hist_binwidth_user", NULL, placeholder = "Ex: 10",
                  width = "125px"
                )
              )
            ),

            conditionalPanel(
              condition = "input.bin_spec == 'Number of Bins'",

              div(style = "display: inline-block; vertical-align: center;",
                strong(paste0("Number of Bins:", stri_dup(intToUtf8(160), 3)))
              ),

              div(style = "display: inline-block;",
                textInput("hist_n_bins_user", NULL, placeholder = "Ex: 10",
                  width = "125px"
                )
              )
            )
          )
        ),


        column(9,

          # download plot button
          column(12, align = "right",
            actionButton("download_plot_user", "Download Plot",
              class = "btn btn-primary", icon = icon("download")
            ),

            bsModal("download_popup_user", NULL, "download_plot_user",
              size = "small", uiOutput("download_popup_user")
            )
          ),


          # plot
          column(12,

            column(1, ""),

            column(7, plotOutput("plot_user"))
          )
        ),


        column(12,

          br(),

          wellPanel(
            fluidRow(
              column(12,

                # change axis aspects
                column(4,
                  checkboxInput("change_axis_aspects_user",
                    strong("Change axis aspects")
                  ),

                  conditionalPanel(
                    condition = "input.change_axis_aspects_user",

                    column(6,
                      strong("Leave the input blank if you don't want to change
                        the respective aspect."
                      ),

                      br(),

                      fluidRow(
                        br(),

                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0("X min:", stri_dup(intToUtf8(160), 3)))
                          ),

                          div(style = "display: inline-block;",
                            textInput("x_min_user", NULL, width = "100px")
                          )
                        )
                      ),


                      fluidRow(
                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0("X max:", stri_dup(intToUtf8(160), 3)))
                          ),

                          div(style = "display: inline-block;",
                            textInput("x_max_user", NULL, width = "100px")
                          )
                        )
                      ),


                      fluidRow(
                        column(12,
                          div(
                            style = "display: inline-block;",

                            strong(paste0("X tick mark increment*:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(
                            style = "display: inline-block;",

                            textInput("x_tick_inc_user", NULL, width = "100px")
                          ),

                          p("*requires min and max inputs")
                        )
                      ),


                      fluidRow(
                        column(12,
                          div(style = "display: inline-block;",
                            strong(paste0("Y max:", stri_dup(intToUtf8(160), 3)))
                          ),

                          div(style = "display: inline-block;",
                            textInput("y_max_user", NULL, width = "100px")
                          )
                        )
                      ),


                      fluidRow(
                        column(12,
                          div(
                            style = "display: inline-block;",

                            strong(paste0(
                              "Text size:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(
                            style = "display: inline-block;",

                            numericInput("axis_text_size_user", NULL,
                              value = 24, width = "100px"
                            )
                          )
                        )
                      ),


                      fluidRow(
                        column(12,
                          div(
                            style = "display: inline-block;",

                            strong(paste0(
                              "Label size:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(
                            style = "display: inline-block;",

                            numericInput("axis_lab_size_user", NULL, value = 28,
                              width = "100px"
                            )
                          )
                        )
                      ),


                      fluidRow(
                        column(12,
                          textInput("x_lab_user", "X axis label", value = "")
                        )
                      ),

                      fluidRow(
                        column(12, align = "center",
                          actionButton("x_lab_update_user", "Update Label",
                            class = "btn btn-success", icon = icon("sync-alt")
                          )
                        ),

                        br(),
                        br()
                      ),


                      fluidRow(
                        column(12,
                          textInput("y_lab_user", "Y axis label", value = "")
                        )
                      ),

                      fluidRow(
                        column(12, align = "center",
                          actionButton("y_lab_update_user", "Update Label",
                            class = "btn btn-success", icon = icon("sync-alt")
                          )
                        ),

                        br(),
                        br()
                      )
                    ),


                    column(6,
                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("X axis line:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          stri_dup(intToUtf8(160), 1)
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_x_axis_line_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      ),


                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("X axis label:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_x_axis_lab_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      ),


                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("X axis ticks:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_x_axis_text_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      ),


                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("Y axis line:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(style = "display: inline-block;",
                          stri_dup(intToUtf8(160), 1)
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_y_axis_line_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      ),


                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("Y axis label:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_y_axis_lab_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      ),


                      column(12,
                        div(
                          style = "display: inline-block;",

                          strong(paste0("Y axis ticks:",
                            stri_dup(intToUtf8(160), 3)
                          ))
                        ),

                        div(
                          style = "display: inline-block;",

                          materialSwitch("keep_y_axis_text_user",
                            label = "Remove", value = TRUE, status = "primary",
                            right = FALSE, inline = TRUE
                          ),
                          span("Keep")  # add "Keep" to right-hand side of switch
                        )
                      )
                    )
                  )
                ),


                # change main fill color
                column(2,
                  checkboxInput("fill_aspects_user",
                    strong("Change main fill color")
                  ),

                  conditionalPanel(
                    condition = "input.fill_aspects_user",

                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Main fill color:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        colourInput("fill_color_user", NULL, value = "grey",
                          showColour = "background"
                        ),

                        tags$head(tags$style(
                          type = "text/css", "#fill_color_user{width: 100px;}"
                        ))
                      )
                    )
                  )
                ),


                column(2,
                  checkboxInput("add_density_user",
                    strong("Add normal density curve")
                  ),

                  conditionalPanel(
                    condition = "input.add_density_user",

                    column(12,
                      div(style = "display: inline-block;",
                        strong(paste0(
                          "Curve color:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        colourInput("curve_color_user", NULL, value = "black",
                          showColour = "background"
                        ),

                        tags$head(tags$style(
                          type = "text/css", "#curve_color_user{width: 100px;}"
                        ))
                      )
                    ),


                    column(12,
                      div(style = "display: inline-block; vertical-align: -1.75em;",
                        strong(paste0(
                          "Curve thickness:",
                          stri_dup(intToUtf8(160), 3)
                        ))
                      ),

                      div(style = "display: inline-block;",
                        sliderInput("curve_lwd_user", NULL, min = 0,
                          max = 4, step = 0.1, value = 1.5, width = "200px"
                        )
                      )
                    )
                  )
                ),


                column(4,
                  conditionalPanel(
                    condition = "input.add_density_user",

                    checkboxInput("color_area_user",
                      strong("Shade area under the curve")
                    ),

                    conditionalPanel(
                      condition = "input.color_area_user",

                      column(4,
                        radioButtons("area_user", "Area to shade",
                          choices = c("Left", "Right", "Between",
                            "Outside/Tails"
                          )
                        )
                      ),


                      column(8,
                        conditionalPanel(
                          condition = "input.area_user == 'Left'",

                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the left of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_left_user", NULL, value = NULL,
                              width = "100px"
                            )
                          )
                        ),

                        conditionalPanel(
                          condition = "input.area_user == 'Right'",

                          div(style = "display: inline-block;",
                            strong(paste0("Shade to the right of:",
                              stri_dup(intToUtf8(160), 3)
                            ))
                          ),

                          div(style = "display: inline-block;",
                            numericInput("cutoff_right_user", NULL,
                              value = NULL, width = "100px"
                            )
                          )
                        ),

                        conditionalPanel(
                          condition = "input.area_user == 'Between'",

                          fluidRow(
                            column(12,
                              # input names flipped since right first then left
                              div(style = "display: inline-block;",
                                strong(paste0("Shade to the right of:",
                                  stri_dup(intToUtf8(160), 3)
                                ))
                              ),

                              div(style = "display: inline-block;",
                                numericInput("cutoff_between_left_user", NULL,
                                  value = NULL, width = "100px"
                                )
                              )
                            ),

                            column(12,
                              # input names flipped since right first then left
                              div(style = "display: inline-block;",
                                strong(paste0("Shade to the left of:",
                                  stri_dup(intToUtf8(160), 3)
                                ))
                              ),

                              div(style = "display: inline-block;",
                                numericInput("cutoff_between_right_user", NULL,
                                  value = NULL, width = "100px"
                                )
                              )
                            )
                          )
                        ),

                        conditionalPanel(
                          condition = "input.area_user == 'Outside/Tails'",

                          fluidRow(
                            column(12,
                              div(style = "display: inline-block;",
                                strong(paste0("Shade to the left of:",
                                  stri_dup(intToUtf8(160), 3)
                                ))
                              ),

                              div(style = "display: inline-block;",
                                numericInput("cutoff_outside_left_user", NULL,
                                  value = NULL, width = "100px"
                                )
                              )
                            ),

                            column(12,
                              div(style = "display: inline-block;",
                                strong(paste0("Shade to the right of:",
                                  stri_dup(intToUtf8(160), 3)
                                ))
                              ),

                              div(style = "display: inline-block;",
                                numericInput("cutoff_outside_right_user", NULL,
                                  value = NULL, width = "100px"
                                )
                              )
                            )
                          )
                        ),


                        fluidRow(
                          column(12,
                            div(style = "display: inline-block;",
                              strong(paste0(
                                "Shading color:",
                                stri_dup(intToUtf8(160), 3)
                              ))
                            ),

                            div(style = "display: inline-block;",
                              colourInput("area_color_user", NULL,
                                value = "black", showColour = "background"
                              ),

                              tags$head(tags$style(
                                type = "text/css", "#area_color_user{width: 100px;}"
                              ))
                            )
                          )
                        ),


                        fluidRow(
                          column(12,
                            div(style = "display: inline-block; vertical-align: -1.75em;",
                              strong(paste0(
                                "Shading opacity:",
                                stri_dup(intToUtf8(160), 3)
                              ))
                            ),

                            div(style = "display: inline-block;",
                              sliderInput("area_opac_user", NULL, min = 0,
                                max = 1, step = 0.05, value = 1, width = "200px"
                              )
                            )
                          )
                        ),


                        fluidRow(
                          column(12,
                            div(
                              style = "display: inline-block;",

                              strong(paste0("Add vertical segment at cutoff(s)",
                                stri_dup(intToUtf8(160), 3)
                              ))
                            ),

                            div(
                              style = "display: inline-block;",

                              materialSwitch("add_cutoff_user", label = "No",
                                status = "primary", right = FALSE,
                                inline = TRUE
                              ),
                              span("Yes")  # add "Yes" to right-hand side of switch
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),




  ##############################################################################
  ############################## Gallery tab panel #############################
  ##############################################################################
  tabPanel("Gallery", icon = icon("images"),

    tabsetPanel(type = "tabs",

      # single dist tab panel
      tabPanel("Plotting a Single Distribution",

        br(),

        column(12,
          h4("Example 1: Illustrating the number of modes and symmetry vs.
            skewness"
          ),

          uiOutput("gallery_shape_ui")
        ),

        column(12, hr()),


        column(12,
          h4("Example 2: Comparing the mean and median for symmetric, skewed
            right, and skewed left distributions"
          ),

          uiOutput("gallery_centers_ui")
        ),

        column(12, hr()),


        column(12,
          h4("Example 3: Plotting a binomial probability (at or below a point)"),

          uiOutput("gallery_binom_ui")
        ),

        column(12, hr()),


        column(12,
          fluidRow(
            column(5,
              h4("Example 4: Explaining the empirical rule for normal
                distributions"
              ),

              uiOutput("gallery_empirical_rule_ui")
            ),

            column(5,
              br(),
              br(),
              br(),

              h4("Instructions for making this plot:"),

              tags$ul(

                tags$li(HTML('<h4>In the left-hand well panel, choose "Select
                  Shape" under "Probability Distribution," and then choose
                  "Bell-shaped" under "Shape." Next click the "Make Plot"
                  button.'
                )),
                tags$li(HTML('<h4>In the bottom well panel, check the "Add lines
                  at statistics" checkbox, and then check the "Empirical rule"
                  checkbox.'
                ))
              )
            )
          )
        ),

        column(12, hr()),


        column(12,
          h4("Example 5: Shading areas under normal curves"),

          uiOutput("gallery_normal_areas_ui")
        ),

        column(12, hr()),


        column(12,
          h4("Example 6: Explaining the confidence level"),

          uiOutput("gallery_conf_level_ui")
        ),

        column(12, hr()),


        column(12,
          h4("Example 7: Explaining p-values"),

          uiOutput("gallery_p_value_ui")
        ),

        column(12,
          br(),
          br()
        )
      ),


      # multiple dists tab panel
      tabPanel("Plotting Multiple Distributions",

        br(),

        column(12,
          column(6,
            h4("Example 1: Explaining the roles of the mean and standard
              deviation for normal distributions"
            ),

            uiOutput("gallery_normal_densities_ui")
          ),

          column(6,
            h4("Example 3: Comparing chi-squared distributions across different
              degrees of freedom values to explore how the skewness changes yet
              the distributions are always skewed right"
            ),

            uiOutput("gallery_chi_squareds_ui")
          )
        ),

        column(12, hr()),

        column(12,
          column(6,
            h4("Example 2: Comparing t distributions to each other as well as to
              the standard normal distribution and illustrating convergence"
            ),

            uiOutput("gallery_normal_and_t_ui")
          ),

          column(6,
            h4("Example 4: Demonstrating how different distributions from the
              Beta family can have substantially different shapes"
            ),

            uiOutput("gallery_betas_ui")
          )
        ),


        column(12,
          br(),
          br()
        )
      ),


      # include your data tab panel
      tabPanel("Including Your Data",

        br(),

        column(12,
          column(6,
            h4("Example 1: Using a normal distribution to approximate the
              distribution of data"
            ),

            uiOutput("gallery_normal_approx_density_ui")
          ),

          column(6,
            h4("Example 2: Using a normal distribution to approximate a
              probability/area associated with data"
            ),

            uiOutput("gallery_normal_approx_area_ui")
          )
        )
      )
    )
  ),




  ##############################################################################
  ############################### Help tab panel ###############################
  ##############################################################################
  tabPanel("Help", icon = icon("question-circle"),

    # main header
    column(12, align = "center",

      h3(style = "color: #0073e6; font-weight: bold;", "Below are the special
        text options available for use when annotating a plot or updating axis
        labels."
      ),


      h3("Note that the following characters are reserved for special text
        options only: [ ] { } @ * ''"
      ),

      br()
    ),


    column(1, ""),

    column(11,

      # column headers
      fluidRow(

        column(3, HTML("<h4><b>Type of addition</b>")),

        column(5, HTML("<h4><b>What to type</b>")),

        column(3, HTML("<h4><b>Example</b>"))
      ),


      # text options and details
      fluidRow(
        column(3, h5("Greek letter (lowercase)")),

        column(5, h5("Type the name of the greek letter, starting with a
          lowercase letter."
        )),

        column(3, h5("alpha"))
      ),

      fluidRow(
        column(3, h5("Greek letter (uppercase)")),

        column(5, h5("Type the name of the greek letter, starting with an
          uppercase letter."
        )),

        column(3, h5("Alpha"))
      ),

      fluidRow(
        column(3, h5("Subscript")),

        column(5, h5("Type the contents of the subscript inside of [ ]."
        )),

        column(3, h5("z[0.025]"))
      ),

      fluidRow(
        column(3, h5("Superscript")),

        column(5, h5("Type the contents of the superscript after ^ and inside of
          @ @."
        )),

        column(3, h5("sigma^@2@"))
      ),

      fluidRow(
        column(3, h5("Bar above letter")),

        column(5, h5("Type the letter inside of bar{ }.")),

        column(3, h5("bar{X}"))
      ),

      fluidRow(
        column(3, h5("Hat above letter")),

        column(5, h5("Type the letter inside of hat{ }.")),

        column(3, h5("hat{p}"))
      ),

      fluidRow(
        column(3, h5("Approximately equal symbol")),

        column(5, h5('Type "approx" without the quotes.')),

        column(3, h5("z approx 1.645"))
      ),

      fluidRow(
        column(3, h5("Greater than or equal to symbol")),

        column(5, h5('Type "gtoet" without the quotes.')),

        column(3, h5("P(Z gtoet 2.5)"))
      ),

      fluidRow(
        column(3, h5("Less than or equal to symbol")),

        column(5, h5('Type "ltoet" without the quotes.')),

        column(3, h5("P(Z ltoet 2.5)"))
      ),

      fluidRow(
        column(3, h5("Star symbol")),

        column(5, h5('Type "star" without the quotes.')),

        column(3, h5("xstar2"))
      ),

      fluidRow(
        column(3, h5("Degree symbol")),

        column(5, h5('Type "degreesym" without the quotes.')),

        column(3, h5("Temperature (degreesymF)"))
      ),

      fluidRow(
        column(3, h5("Bold font")),

        column(5, h5("Type the text to make bold inside of bold{ }.")),

        column(3, h5("bold{Confidence level}"))
      ),

      fluidRow(
        column(3, h5("Italicized font")),

        column(5, h5("Type the text to italicize inside of italic{ }.")),

        column(3, h5("italic{Confidence level}"))
      ),


      hr(),


      fluidRow(
        column(3,
          HTML("<h4><b>More examples</b>"),

          h5("alpha/2 = 0.025"),

          h5("z[0.025] = 1.96"),

          h5("z[0.05] approx 1.645")
        )
      ),

      br(),
      br()
    )
  ),




  ##############################################################################
  ############################## About tab panel ###############################
  ##############################################################################
  tabPanel("About", icon = icon("info-circle"),

    column(5,
      # information about the developer
      h4("The information below will be updated after the completion of the peer
        review process for the associated article."
      ),
      br(),
      br(),

      h4("Developer"),
      p("This tool was developed by Anonymous."),
      br(),

      # information about the app
      h4("About the App"),
      p("This tool enables users to plot univariate distributions and was
        designed with a particular focus on education. Users can plot
        distributions based on general shape (e.g., symmetric vs. skewed right)
        and distributions from common discrete and continuous families. They can
        also shade areas underneath the curve (e.g., areas corresponding to
        general probabilities, p-values, or confidence levels). Users can
        additionally plot their own quantitative data, as well as overlay a
        normal density curve and shade area(s) underneath."
      ),
      br(),

      # contact info
      h4("Contact"),
      p("Email: Anonymous"),
      br(),
      br(),

      # copyright statement
      p("Copyright \uA9 2021-2022 by Anonymous."),

      p("The license statement can be found",
        a("here.", href = "https://choosealicense.com/licenses/mit/",
          target = "_blank"
        )
      )
    ),


    column(1, ""),


    column(4,

      br(),
      br(),
      br(),
      br(),

      img(src = "DistPlotter_Sticker_Image.png", height = "300px")
    )
  )
)



