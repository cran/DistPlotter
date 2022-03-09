
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(ggplot2); theme_set(theme_grey(20))
library(scales)
library(colourpicker)
library(dplyr)
library(stringr)
library(extraDistr)
library(rio)


shinyServer(function(session, input, output){

  ### stop server when pop-up window closed
  session$onSessionEnded(function(x) stopApp())


  ### create and initialize reactive stuff to reset output
  bag <- reactiveValues(

    # making plots
    do_plot_common = FALSE,
    do_plot_mult = FALSE,
    do_plot_user = FALSE,

    # adding labels
    xlab_status_common = 0, ylab_status_common = 0,
    xlab_status_mult = 0, ylab_status_mult = 0,
    xlab_status_user = 0, ylab_status_user = 0,

    # annotating and adding lines to single (common) plot
    n_conds = 0,
    annotated_status = 0,

    # number of density curves when plotting multiple
    n_curves = 0,
    add_curve_status = 0
  )



  ### store plot width based on user's browser and adjust
  ###########################################################
  plot_width_common <- reactive({

    # proportion of screen width dedicated to plot
    plot_proportion <- 9/12 * 7/12


    # final plot width
    input$dimension[1] * plot_proportion
  })


  plot_width_mult <- reactive({

    # proportion of screen width dedicated to plot
    plot_proportion <- 7/12 * 9/12


    # final plot width
    input$dimension[1] * plot_proportion
  })


  plot_width_user <- reactive({

    # proportion of screen width dedicated to plot
    plot_proportion <- 7/12 * 9/12


    # final plot width
    input$dimension[1] * plot_proportion
  })



  ### fxn that cleans labels for correct parsing
  clean_label <- function(plot_lab) {

    lab <- plot_lab


    # remove extra spaces
    lab <- gsub("\\s+", " ", lab)


    ### convert symbols; add last * in certain ones in case user adds more
    ###  text after symbol

    # " needs to come first
    # lab <- gsub('\\"', '*\'\\"\'*', lab)
    # lab <- gsub(' \\*\'\\"\'\\* ', ' \'\\"\' ', lab)
    # lab <- gsub(' \\*\'\\"\'', ' \'\\"\'', lab)
    # lab <- gsub('\'\\"\'\\* ', '*\'\\"\' ', lab)

    # ' needs to come second
    lab <- gsub("\\'", "*\"\\'\"*", lab)
    lab <- gsub(" \\*\"\\'\"\\* ", " \"\\'\" ", lab)
    lab <- gsub(" \\*\"\\'\"", " \"\\'\"", lab)
    lab <- gsub("\"\\'\"\\* ", "*\"\\'\" ", lab)

    # convert ~ if included by user; needs to come before space to ~ conversion
    lab <- gsub("~", "*\'~\'*", lab)
    lab <- gsub("~\\*\'~\'\\*~", "~\'~\'~", lab)
    lab <- gsub("~\\*\'~\'", "~\'~\'", lab)
    lab <- gsub("\'~\'\\*~", "*\'~\'~", lab)

    # convert space to ~ for space
    lab <- gsub(" ", "~", lab)

    # convert equal sign
    lab <- gsub("=", "==", lab)
    lab <- gsub("~=~", "=", lab)
    lab <- gsub("~=", "=", lab)
    lab <- gsub("=~", "=", lab)

    # !
    lab <- gsub("!", "*\'!\'*", lab)
    lab <- gsub("~\\*\'!\'\\*~", "~\'!\'~", lab)
    lab <- gsub("~\\*\'!\'", "~\'!\'", lab)
    lab <- gsub("\'!\'\\*~", "*\'!\'~", lab)

    # #
    lab <- gsub("#", "*\'#\'*", lab)
    lab <- gsub("~\\*\'#\'\\*~", "~\'#\'~", lab)
    lab <- gsub("~\\*\'#\'", "~\'#\'", lab)
    lab <- gsub("\'#\'\\*~", "*\'#\'~", lab)

    # $
    lab <- gsub("\\$", "*\'$\'*", lab)
    lab <- gsub("~\\*\'\\$\'\\*~", "~\'$\'~", lab)
    lab <- gsub("~\\*\'\\$\'", "~\'$\'", lab)
    lab <- gsub("\'\\$\'\\*~", "*\'$\'~", lab)

    # %
    lab <- gsub("%", "*\'%\'*", lab)
    lab <- gsub("~\\*\'%\'\\*~", "~\'%\'~", lab)
    lab <- gsub("~\\*\'%\'", "~\'%\'", lab)
    lab <- gsub("\'%\'\\*~", "*\'%\'~", lab)

    # &
    lab <- gsub("&", "*\'&\'*", lab)
    lab <- gsub("~\\*\'&\'\\*~", "~\'&\'~", lab)
    lab <- gsub("~\\*\'&\'", "~\'&\'", lab)
    lab <- gsub("\'&\'\\*~", "*\'&\'~", lab)

    # (
    lab <- gsub("\\(", "*\'\\(\'*", lab)
    lab <- gsub("~\\*\'\\(\'\\*~", "~\'\\(\'~", lab)
    lab <- gsub("~\\*\'\\(\'", "~\'\\(\'", lab)
    lab <- gsub("\'\\(\'\\*~", "*\'\\(\'~", lab)

    # )
    lab <- gsub("\\)", "*\'\\)\'*", lab)
    lab <- gsub("~\\*\'\\)\'\\*~", "~\'\\)\'~", lab)
    lab <- gsub("~\\*\'\\)\'", "~\'\\)\'", lab)
    lab <- gsub("\'\\)\'\\*~", "*\'\\)\'~", lab)

    # _
    lab <- gsub("_", "*\'_\'*", lab)
    lab <- gsub("~\\*\'_\'\\*~", "~\'_\'~", lab)
    lab <- gsub("~\\*\'_\'", "~\'_\'", lab)
    lab <- gsub("\'_\'\\*~", "*\'_\'~", lab)

    # { and } to ( and ) for bar(), hat(), bold(), and italic()
    lab <- gsub("~\\{~|~\\{|\\{~|\\{", "\\(", lab)
    lab <- gsub("~\\}~|~\\}|\\}~|\\}", "\\)", lab)

    # 2 uses of @ for superscript
    lab <- sub("@", "\\{", lab)    # first @
    lab <- sub("@", "\\}", lab)    # second @

    # \
    lab <- gsub("\\\\", "\\*\'\\\\\\\\\'\\*", lab)
    lab <- gsub("~\\*\'\\\\\\\\\'\\*~", "~\'\\\\\\\\\'~", lab)
    lab <- gsub("~\\*\'\\\\\\\\\'", "~\'\\\\\\\\\'", lab)
    lab <- gsub("\'\\\\\\\\\'\\*~", "\\*\'\\\\\\\\\'~", lab)

    # |
    lab <- gsub("\\|", "*\'|\'*", lab)
    lab <- gsub("~\\*\'\\|\'\\*~", "~\'\\|\'~", lab)
    lab <- gsub("~\\*\'\\|\'", "~\'\\|\'", lab)
    lab <- gsub("\'\\|\'\\*~", "*\'\\|\'~", lab)

    # :
    lab <- gsub("\\:", "*\'\\:\'*", lab)
    lab <- gsub("~\\*\'\\:\'\\*~", "~\'\\:\'~", lab)
    lab <- gsub("~\\*\'\\:\'", "~\'\\:\'", lab)
    lab <- gsub("\'\\:\'\\*~", "*\'\\:\'~", lab)

    # ;
    lab <- gsub("\\;", "*\'\\;\'*", lab)
    lab <- gsub("~\\*\'\\;\'\\*~", "~\'\\;\'~", lab)
    lab <- gsub("~\\*\'\\;\'", "~\'\\;\'", lab)
    lab <- gsub("\'\\;\'\\*~", "*\'\\;\'~", lab)

    # ,
    lab <- gsub("\\,", "\\*\'\\,\'\\*", lab)
    lab <- gsub("~\\*\'\\,\'\\*~", "~\'\\,\'~", lab)
    lab <- gsub("~\\*\'\\,\'", "~\'\\,\'", lab)
    lab <- gsub("\'\\,\'\\*~", "\\*\'\\,\'~", lab)

    # <
    lab <- gsub("<", "\\*\'<\'\\*", lab)
    lab <- gsub("~\\*\'<\'\\*~", "~\'<\'~", lab)
    lab <- gsub("~\\*\'<\'", "~\'<\'", lab)
    lab <- gsub("\'<\'\\*~", "\\*\'<\'~", lab)

    # .
    lab <- gsub("\'\\[\'\\.", "\'.\'", lab)

    # >
    lab <- gsub(">", "\\*\'>\'\\*", lab)
    lab <- gsub("~\\*\'>\'\\*~", "~\'>\'~", lab)
    lab <- gsub("~\\*\'>\'", "~\'>\'", lab)
    lab <- gsub("\'>\'\\*~", "\\*\'>\'~", lab)

    # ?
    lab <- gsub("\\?", "\\*\'\\?\'\\*", lab)
    lab <- gsub("~\\*\'\\?\'\\*~", "~\'\\?\'~", lab)
    lab <- gsub("~\\*\'\\?\'", "~\'\\?\'", lab)
    lab <- gsub("\'\\?\'\\*~", "\\*\'\\?\'~", lab)

    # /
    lab <- gsub("\\/", "\\*\'\\/\'\\*", lab)
    lab <- gsub("~\\*\'\\/\'\\*~", "~\'\\/\'~", lab)
    lab <- gsub("~\\*\'\\/\'", "~\'\\/\'", lab)
    lab <- gsub("\'\\/\'\\*~", "\\*\'\\/\'~", lab)

    # >=
    lab <- gsub("~gtoet~", ">=", lab)
    lab <- gsub("~gtoet", ">=", lab)
    lab <- gsub("gtoet~", ">=", lab)
    lab <- gsub("gtoet", ">=", lab)

    # <=
    lab <- gsub("~ltoet~", "<=", lab)
    lab <- gsub("~ltoet", "<=", lab)
    lab <- gsub("ltoet~", "<=", lab)
    lab <- gsub("ltoet", "<=", lab)

    # 'approx'
    lab <- gsub("~approx~", "%~~%", lab)
    lab <- gsub("~approx", "%~~%", lab)
    lab <- gsub("approx~", "%~~%", lab)
    lab <- gsub("approx", "%~~%", lab)

    # 'degrees'
    lab <- gsub("degreesym", "*degree*", lab)
    lab <- gsub("~\\*degree\\*~", "~degree~", lab)
    lab <- gsub("~\\*degree", "~degree", lab)
    lab <- gsub("degree\\*~", "*degree~", lab)

    # 'star'
    lab <- gsub("star", "*\'*\'*", lab)
    lab <- gsub("~\\*\'\\*\'\\*~", "~\'*\'~", lab)
    lab <- gsub("~\\*\'\\*\'", "~\'*\'", lab)
    lab <- gsub("\'\\*\'\\*~", "*\'*\'~", lab)

    lab <- gsub("\\{\\*\'\\*\'\\*", "\\{\'*\'", lab)
    lab <- gsub("\'\\*\'\\*\\}", "\'*\'\\}", lab)


    # 'in'
    lab <- gsub("in", "\'in\'", lab)


    # convert multiple *'s in a row to a single *
    lab <- gsub("\\*+", "\\*", lab)


    # add * between adjoined number and letter
    lab <- gsub("(\\d)([a-zA-Z])", "\\1*\\2", lab)


    # remove any ~ or * at beginning and end
    for (i in 1:nchar(lab)) {
      if (substr(lab, 1, 1) %in% c("~", "*")) {
        lab <- substr(lab, 2, nchar(lab))
      }

      if (stri_sub(lab, -1) %in% c("~", "*")) {
        lab <- substr(lab, 1, nchar(lab) - 1)
      }
    }


    # count number of open and closed parentheses, brackets, and @
    n_paren_open <- str_count(lab, "\\(")
    n_paren_closed <- str_count(lab, "\\)")
    issue_paren <- n_paren_open != n_paren_closed

    n_brack_open <- str_count(lab, "\\[")
    n_brack_closed <- str_count(lab, "\\]")
    issue_brack <- n_brack_open != n_brack_closed

    n_curly_open <- str_count(lab, "\\{")
    n_curly_closed <- str_count(lab, "\\}")
    issue_curly <- n_curly_open != n_curly_closed

    n_at_open <- str_count(lab, "@")
    n_at_closed <- str_count(lab, "@")
    issue_at <- n_at_open != n_at_closed

    # determine if end with operator such as ^, *, -, or +
    issue_end_operator <- stri_sub(lab, -1) %in% c("^", "*", "-", "+")


    if (issue_paren | issue_brack | issue_curly) {
      shinyalert("Warning!", "The label must contain the same number of open
        and closed parentheses, brackets, and @'s. For
        more information, see the 'Help' tab",
        type = "error"
      )
    }

    if (issue_end_operator) {
      shinyalert("Warning!", "The label may not end in an operator such as ^, *,
        -, or +.",
        type = "error"
      )
    }

    validate(need(
      all(
        c(
          issue_paren, issue_brack, issue_curly, issue_at, issue_end_operator
        ) == FALSE
      ),
      ""
    ))


    lab
  }



  ##############################################################################
  ######################### Plot a Single Distribution #########################
  ##############################################################################

  # check for issues with inputs
  observeEvent(input$make_plot_common, {

    ### warning popup if invalid parameter inputs
    #############################################################

    if (input$pop_dist == "basic") {

      if (input$pop_shape == "Bell-shaped") {

        issue_norm1 <- input$norm_sd_basic < 0

        issue_norm2 <- anyNA(c(input$norm_mean_basic, input$norm_sd_basic))

        if (issue_norm1 | issue_norm2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_norm1, issue_norm2) == FALSE), ""))

      } else if (input$pop_shape == "Bimodal") {

        issue_bi1 <- any(c(input$bi_sd1, input$bi_sd2) < 0)

        issue_bi2 <- anyNA(
          c(input$bi_mean1, input$bi_mean2, input$bi_sd1, input$bi_sd2)
        )

        if (issue_bi1 | issue_bi2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_bi1, issue_bi2) == FALSE), ""))

      } else {

        issue_skewed1 <- input$x_min_basic >= input$x_max_basic

        issue_skewed2 <- anyNA(c(input$x_min_basic, input$x_max_basic))

        if (issue_skewed1 | issue_skewed2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_skewed1, issue_skewed2) == FALSE), ""))
      }

    } else if (input$pop_dist == "advanced") {

      if (input$dist_type == "Continuous") {

        if (input$cont_fam == "Beta") {

          issue_beta1 <- any(c(input$beta_shape1, input$beta_shape2) <= 0)

          issue_beta2 <- anyNA(c(input$beta_shape1, input$beta_shape2))

          if (issue_beta1 | issue_beta2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid. Please enter a positive number for each.",
              type = "error"
            )
          }

          validate(need(all(c(issue_beta1, issue_beta2) == FALSE), ""))

        } else if (input$cont_fam == "Cauchy") {

          issue_cauchy1 <- input$cauchy_scale <= 0

          issue_cauchy2 <- anyNA(c(input$cauchy_loc, input$cauchy_scale))

          if (issue_cauchy1 | issue_cauchy2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_cauchy1, issue_cauchy2) == FALSE), ""))

        } else if (input$cont_fam == "Chi-square") {

          issue_chisq1 <- input$chisq_df <= 0

          issue_chisq2 <- is.na(input$chisq_df)

          if (issue_chisq1 | issue_chisq2) {
            shinyalert("Warning!", "The degrees of freedom value provided is not
              valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_chisq1, issue_chisq2) == FALSE), ""))

        } else if (input$cont_fam == "Exponential") {

          issue_exp1 <- input$exp_rate <= 0

          issue_exp2 <- is.na(input$exp_rate)

          if (issue_exp1 | issue_exp2) {
            shinyalert("Warning!", "The rate provided is not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_exp1, issue_exp2) == FALSE), ""))

        } else if (input$cont_fam == "F") {

          issue_f1 <- any(c(input$f_df1, input$f_df2) <= 0)

          issue_f2 <- anyNA(c(input$f_df1, input$f_df2))

          if (issue_f1 | issue_f2) {
            shinyalert("Warning!", "At least one degrees of freedom value provided
              is not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_f1, issue_f2) == FALSE), ""))

        } else if (input$cont_fam == "Gamma") {

          issue_gamma1 <- any(c(input$gamma_shape, input$gamma_rate) <= 0)

          issue_gamma2 <- anyNA(c(input$gamma_shape, input$gamma_rate))

          if (issue_gamma1 | issue_gamma2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_gamma1, issue_gamma2) == FALSE), ""))

        } else if (input$cont_fam == "Laplace") {

          issue_laplace1 <- input$laplace_scale <= 0

          issue_laplace2 <- anyNA(c(input$laplace_loc, input$laplace_scale))

          if (issue_laplace1 | issue_laplace2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_laplace1, issue_laplace2) == FALSE), ""))

        } else if (input$cont_fam == "Log-normal") {

          issue_lognorm1 <- input$lognorm_sd <= 0

          issue_lognorm2 <- anyNA(c(input$lognorm_mean, input$lognorm_sd))

          if (issue_lognorm1 | issue_lognorm2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_lognorm1, issue_lognorm2) == FALSE), ""))

        } else if (input$cont_fam == "Normal") {

          issue_norm1 <- input$norm_sd < 0

          issue_norm2 <- anyNA(c(input$norm_mean, input$norm_sd))

          if (issue_norm1 | issue_norm2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_norm1, issue_norm2) == FALSE), ""))

        } else if (input$cont_fam == "t") {

          issue_t1 <- input$t_df <= 0

          issue_t2 <- is.na(input$t_df)

          if (issue_t1 | issue_t2) {
            shinyalert("Warning!", "The degrees of freedom value provided is not
              valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_t1, issue_t2) == FALSE), ""))

        } else if (input$cont_fam == "Uniform (continuous)") {

          issue_uni1 <- input$uni_min >= input$uni_max

          issue_uni2 <- anyNA(c(input$uni_min, input$uni_max))

          if (issue_uni1 | issue_uni2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_uni1, issue_uni2) == FALSE), ""))

        } else if (input$cont_fam == "Weibull") {

          issue_weibull1 <- any(c(input$weib_shape, input$weib_scale) <= 0)

          issue_weibull2 <- anyNA(c(input$weib_shape, input$weib_scale))

          if (issue_weibull1 | issue_weibull2) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_weibull1, issue_weibull2) == FALSE), ""))
        }

      } else {

        if (input$disc_fam == "Bernoulli") {

          issue_bern1 <- input$bern_p < 0 | input$bern_p > 1

          issue_bern2 <- is.na(input$bern_p)

          if (issue_bern1 | issue_bern2) {
            shinyalert("Warning!", "The probability provided is not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_bern1, issue_bern2) == FALSE), ""))

        } else if (input$disc_fam == "Binomial") {

          # decimal value for n
          issue_bin1 <- mean(c(
            as.numeric(input$bin_n), as.integer(input$bin_n)
          )) != as.numeric(input$bin_n)

          # non-positive value for n
          issue_bin2 <- input$bin_n <= 0

          # input left blank
          issue_bin3 <- anyNA(c(input$bin_n, input$bin_p))

          issue_bin4 <- input$bin_p < 0 | input$bin_p > 1

          if (issue_bin1 | issue_bin2 | issue_bin3 | issue_bin4) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(
            all(c(issue_bin1, issue_bin2, issue_bin3, issue_bin4) == FALSE),
            ""
          ))

        } else if (input$disc_fam == "Geometric") {

          issue_geom1 <- input$geom_p < 0 | input$geom_p > 1

          issue_geom2 <- is.na(input$geom_p)

          if (issue_geom1 | issue_geom2) {
            shinyalert("Warning!", "The probability provided is not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_geom1, issue_geom2) == FALSE), ""))

        } else if (input$disc_fam == "Hypergeometric") {

          # decimal value for N
          issue_hyper1 <- mean(c(
            as.numeric(input$hyper_N), as.integer(input$hyper_N)
          )) != as.numeric(input$hyper_N)

          # invalid value for N
          issue_hyper2 <- input$hyper_N <= 0

          # decimal value for K
          issue_hyper3 <- mean(c(
            as.numeric(input$hyper_K), as.integer(input$hyper_K)
          )) != as.numeric(input$hyper_K)

          # invalid value for K
          issue_hyper4 <- input$hyper_K < 0 | input$hyper_K > input$hyper_N

          # decimal value for n
          issue_hyper5 <- mean(c(
            as.numeric(input$hyper_n), as.integer(input$hyper_n)
          )) != as.numeric(input$hyper_n)

          # invalid value for n
          issue_hyper6 <- input$hyper_n <= 0 | input$hyper_n > input$hyper_N

          # input left blank
          issue_hyper7 <- anyNA(c(input$hyper_N, input$hyper_K, input$hyper_n))

          if (issue_hyper1 | issue_hyper2 | issue_hyper3 | issue_hyper4 |
              issue_hyper5 | issue_hyper6 | issue_hyper7
          ) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(
            all(c(issue_hyper1, issue_hyper2, issue_hyper3, issue_hyper4,
              issue_hyper5, issue_hyper6, issue_hyper7) == FALSE
            ),
            ""
          ))

        } else if (input$disc_fam == "Negative Binomial") {

          # decimal value for r
          issue_negbin1 <- mean(c(
            as.numeric(input$negbin_r), as.integer(input$negbin_r)
          )) != as.numeric(input$negbin_r)

          # non-positive value for r
          issue_negbin2 <- input$negbin_r <= 0

          # input left blank
          issue_negbin3 <- anyNA(c(input$negbin_r, input$negbin_p))

          issue_negbin4 <- input$negbin_p < 0 | input$negbin_p > 1

          if (issue_negbin1 | issue_negbin2 | issue_negbin3 | issue_negbin4) {
            shinyalert("Warning!", "At least one parameter value provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(
            all(c(issue_negbin1, issue_negbin2, issue_negbin3,
              issue_negbin4) == FALSE
            ),
            ""
          ))

        } else if (input$disc_fam == "Poisson") {

          issue_pois1 <- input$pois_rate < 0

          issue_pois2 <- is.na(input$pois_rate)

          if (issue_pois1 | issue_pois2) {
            shinyalert("Warning!", "The rate provided is not valid.",
              type = "error"
            )
          }

          validate(need(all(c(issue_pois1, issue_pois2) == FALSE), ""))

        } else if (input$disc_fam == "Uniform (discrete)") {

          # decimal value for size of set
          issue_uni1 <- mean(c(
            as.numeric(input$uni_n), as.integer(input$uni_n)
          )) != as.numeric(input$uni_n)

          # non-positive value for size of set
          issue_uni2 <- input$uni_n <= 0

          # input left blank
          issue_uni3 <- is.na(input$uni_n)

          if (issue_uni1 | issue_uni2 | issue_uni3) {
            shinyalert("Warning!", "The number of possible values provided is
              not valid.",
              type = "error"
            )
          }

          validate(need(
            all(c(issue_uni1, issue_uni2, issue_uni3) == FALSE),
            ""
          ))

        }
      }
    }


    # need this to reset plot if certain inputs changed
    bag$do_plot_common <- input$make_plot_common
  })



  ### reset plot if certain inputs changed
  ################################################
  reset_plot_common <- reactive({

    list(input$pop_dist, input$pop_shape, input$dist_type, input$cont_fam,
      input$disc_fam
    )
  })


  observeEvent(reset_plot_common(), {

    bag$do_plot_common <- FALSE
    bag$xlab_status_common <- 0
    bag$ylab_status_common <- 0
    bag$annotated_status <- 0
  })




  ### make plot
  ##############################################
  make_init_plot_common <- reactive({

    if (bag$do_plot_common != FALSE) {

      if (input$pop_dist == "basic") {

        n <- 1e4 + 1

        if (input$pop_shape == "Bell-shaped") {

          dist_fxn <- dnorm

          params <- c(input$norm_mean_basic, input$norm_sd_basic)

          x_min <- qnorm(0.0001, input$norm_mean_basic, input$norm_sd_basic)
          x_max <- qnorm(0.9999, input$norm_mean_basic, input$norm_sd_basic)


          plot_density <- stat_function(
            fun = dnorm,
            n = n,
            args = list(mean = params[1], sd = params[2]),
            color = input$curve_color_common,
            lwd = input$curve_lwd_common
          )

        } else if (input$pop_shape == "Skewed") {

          dist_fxn <- dnsbeta

          params <- c(input$skewed_shape1, input$skewed_shape2)

          x_min <- input$x_min_basic
          x_max <- input$x_max_basic


          plot_density <- stat_function(
            fun = dnsbeta,
            n = n,
            args = list(
              min = input$x_min_basic,
              max = input$x_max_basic,
              shape1 = params[1],
              shape2 = params[2]
            ),
            color = input$curve_color_common,
            lwd = input$curve_lwd_common
          )

        } else if (input$pop_shape == "Bimodal") {

          dist_fxn <- dmixnorm

          params <- c(
            input$bi_mean1, input$bi_mean2,
            input$bi_sd1, input$bi_sd2,
            0.5, 0.5
          )

          x_min <- qnorm(
            0.0001,
            min(c(input$bi_mean1, input$bi_mean2)),
            max(c(input$bi_sd1, input$bi_sd2))
          )

          x_max <- qnorm(
            0.9999,
            max(c(input$bi_mean1, input$bi_mean2)),
            max(c(input$bi_sd1, input$bi_sd2))
          )


          plot_density <- stat_function(
            fun = dmixnorm,
            n = n,
            args = list(
              mean = c(params[1], params[2]),
              sd = c(params[3], params[4]),
              alpha = c(params[5], params[6])
            ),
            color = input$curve_color_common,
            lwd = input$curve_lwd_common
          )

        } else if (input$pop_shape == "Uniform") {

          dist_fxn <- dunif

          params <- c(input$x_min_basic, input$x_max_basic)

          x_min <- input$x_min_basic
          x_max <- input$x_max_basic


          plot_density <- stat_function(
            fun = dunif,
            n = n,
            args = list(min = params[1], max = params[2]),
            color = input$curve_color_common,
            lwd = input$curve_lwd_common
          )
        }


        df_x <- data.frame(x = c(x_min, x_max))

        plot_common <- ggplot(df_x, aes(x = x))


        if (input$add_line_mean) {

          plot_common <- plot_common +
            geom_segment(
              aes(
                x = if (input$pop_shape == "Bell-shaped") {
                      input$norm_mean_basic
                    } else if (input$pop_shape == "Skewed") {
                      params[1] / (params[1] + params[2]) *
                        (input$x_max_basic - input$x_min_basic)
                    } else if (input$pop_shape == "Uniform") {
                      0.5*(input$x_max_basic - input$x_min_basic)
                    },
                y = 0,
                xend = if (input$pop_shape == "Bell-shaped") {
                         input$norm_mean_basic
                       } else if (input$pop_shape == "Skewed") {
                         params[1] / (params[1] + params[2]) *
                           (input$x_max_basic - input$x_min_basic)
                       } else if (input$pop_shape == "Uniform") {
                         0.5*(input$x_max_basic - input$x_min_basic)
                       },
                yend = if (input$pop_shape == "Bell-shaped") {
                         dnorm(
                           input$norm_mean_basic,
                           input$norm_mean_basic,
                           input$norm_sd_basic
                         )
                       } else if (input$pop_shape == "Skewed") {
                         dnsbeta(
                           params[1] / (params[1] + params[2]) *
                             (input$x_max_basic - input$x_min_basic),
                           min = input$x_min_basic,
                           max = input$x_max_basic,
                           shape1 = params[1],
                           shape2 = params[2]
                         )
                       } else if (input$pop_shape == "Uniform") {
                         1/(input$x_max_basic - input$x_min_basic)
                       },
              ),
              color = input$line_color_mean,
              lwd = input$line_lwd_mean
            )
        }


        if (input$add_line_median) {

          plot_common <- plot_common +
            geom_segment(
              aes(
                x = if (input$pop_shape =="Bell-shaped") {
                      input$norm_mean_basic
                    } else if (input$pop_shape == "Skewed") {
                      (params[1] - 1/3) /
                        (params[1] + params[2] - 2/3) *
                        (input$x_max_basic - input$x_min_basic)
                    } else if (input$pop_shape == "Uniform") {
                      0.5*(input$x_max_basic - input$x_min_basic)
                    },
                y = 0,
                xend = if (input$pop_shape == "Bell-shaped") {
                         input$norm_mean_basic
                       } else if (input$pop_shape == "Skewed") {
                         (params[1] - 1/3) /
                           (params[1] + params[2] - 2/3) *
                           (input$x_max_basic - input$x_min_basic)
                       } else if (input$pop_shape == "Uniform") {
                         0.5*(input$x_max_basic - input$x_min_basic)
                       },
                yend = if (input$pop_shape == "Bell-shaped") {
                         dnorm(
                           input$norm_mean_basic,
                           input$norm_mean_basic,
                           input$norm_sd_basic
                         )
                       } else if (input$pop_shape == "Skewed") {
                         dnsbeta(
                           (params[1] - 1/3) /
                             (params[1] + params[2] - 2/3) *
                             (input$x_max_basic - input$x_min_basic),
                           min = input$x_min_basic,
                           max = input$x_max_basic,
                           shape1 = params[1],
                           shape2 = params[2]
                         )
                       } else if (input$pop_shape == "Uniform") {
                         1/(input$x_max_basic - input$x_min_basic)
                       },
              ),
              color = input$line_color_median,
              lwd = input$line_lwd_median
            )
        }



        if (input$x_max_common != "" & input$x_min_common != "") {

          x_min_common <- ifelse(
            input$x_min_common != "",
            as.numeric(input$x_min_common),
            NA
          )

          x_max_common <- ifelse(
            input$x_max_common != "",
            as.numeric(input$x_max_common),
            NA
          )


          if (input$x_tick_inc_common != "") {

            if (!is.na(as.numeric(input$x_tick_inc_common))) {
              if (as.numeric(input$x_tick_inc_common) > 0 &
                  as.numeric(input$x_max_common) > as.numeric(input$x_min_common)
              ) {

                plot_common <- plot_common +
                  scale_x_continuous(
                    oob = oob_keep(),
                    breaks = c(
                      x_min_common,    # force x min to appear w/ repeat
                      seq(
                        x_min_common,
                        x_max_common,
                        by = as.numeric(input$x_tick_inc_common)
                      ),
                      x_max_common     # force x max to appear w/ repeat
                    ),
                    limits = c(x_min_common, x_max_common),
                    expand = c(0, 0)
                    #expand = expansion(mult = c(0, 0.04))
                  )
              }
            }

          } else if (input$x_tick_inc_common == "") {

            if (as.numeric(input$x_max_common) > as.numeric(input$x_min_common)) {

              plot_common <- plot_common +
                scale_x_continuous(
                  oob = oob_keep(),
                  limits = c(x_min_common, x_max_common),
                  expand = c(0, 0)
                  #expand = expansion(mult = c(0, 0.04))
                )

            } else {

              plot_common <- plot_common +
                scale_x_continuous(
                  oob = oob_keep(),
                  limits = c(x_min_common, NA),
                  expand = c(0, 0)
                  #expand = expansion(mult = c(0, 0.04))
                )
            }
          }

        } else if (input$x_max_common == "" & input$x_min_common != "") {

          plot_common <- plot_common +
            scale_x_continuous(
              oob = oob_keep(),
              limits = c(as.numeric(input$x_min_common), NA),
              expand = c(0, 0)
              #expand = expansion(mult = c(0, 0.04))
            )

        } else if (input$x_max_common != "" & input$x_min_common == "") {

          plot_common <- plot_common +
            scale_x_continuous(
              oob = oob_keep(),
              limits = c(NA, as.numeric(input$x_max_common)),
              expand = c(0, 0)
              #expand = expansion(mult = c(0, 0.04))
            )

        } else {

          plot_common <- plot_common +
            scale_x_continuous(oob = oob_keep(), expand = c(0, 0))
        }



        if (input$pop_shape == "Bell-shaped") {
          if (input$stats_empirical) {

            plot_common <- plot_common +
              geom_segment(
                aes(
                  x = input$norm_mean_basic + input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic + input$norm_sd_basic,
                  yend = 1.2*dnorm(
                    input$norm_mean_basic + input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "blue"
              ) +
              geom_segment(
                aes(
                  x = input$norm_mean_basic - input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic - input$norm_sd_basic,
                  yend = 1.2*dnorm(
                    input$norm_mean_basic - input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "blue"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 1.2*dnorm(
                    input$norm_mean_basic + input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic + input$norm_sd_basic,
                  yend = 1.2*dnorm(
                    input$norm_mean_basic + input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "blue"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 1.2*dnorm(
                    input$norm_mean_basic - input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic - input$norm_sd_basic,
                  yend = 1.2*dnorm(
                    input$norm_mean_basic - input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "blue"
              ) +
              annotate(
                geom = "text",
                label = "68%",
                x = 0,
                y = 0.015 + 1.2*dnorm(
                  input$norm_mean_basic + input$norm_sd_basic,
                  input$norm_mean_basic,
                  input$norm_sd_basic
                ),
                color = "blue"
              ) +

              geom_segment(
                aes(
                  x = input$norm_mean_basic + 2*input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic + 2*input$norm_sd_basic,
                  yend = 2*dnorm(
                    input$norm_mean_basic + 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "darkgreen"
              ) +
              geom_segment(
                aes(
                  x = input$norm_mean_basic - 2*input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic - 2*input$norm_sd_basic,
                  yend = 2*dnorm(
                    input$norm_mean_basic - 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "darkgreen"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 2*dnorm(
                    input$norm_mean_basic + 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic + 2*input$norm_sd_basic,
                  yend = 2*dnorm(
                    input$norm_mean_basic + 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "darkgreen"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 2*dnorm(
                    input$norm_mean_basic - 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic - 2*input$norm_sd_basic,
                  yend = 2*dnorm(
                    input$norm_mean_basic - 2*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "darkgreen"
              ) +
              annotate(
                geom = "text",
                label = "95%",
                x = 0,
                y = 0.015 + 2*dnorm(
                  input$norm_mean_basic + 2*input$norm_sd_basic,
                  input$norm_mean_basic,
                  input$norm_sd_basic
                ),
                color = "darkgreen"
              ) +

              geom_segment(
                aes(
                  x = input$norm_mean_basic + 3*input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic + 3*input$norm_sd_basic,
                  yend = 6*dnorm(
                    input$norm_mean_basic + 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "red"
              ) +
              geom_segment(
                aes(
                  x = input$norm_mean_basic - 3*input$norm_sd_basic,
                  y = 0,
                  xend = input$norm_mean_basic - 3*input$norm_sd_basic,
                  yend = 6*dnorm(
                    input$norm_mean_basic - 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                linetype = "dotted",
                color = "red"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 6*dnorm(
                    input$norm_mean_basic + 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic + 3*input$norm_sd_basic,
                  yend = 6*dnorm(
                    input$norm_mean_basic + 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "red"
              ) +
              geom_segment(
                aes(
                  x = 0,
                  y = 6*dnorm(
                    input$norm_mean_basic - 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  ),
                  xend = input$norm_mean_basic - 3*input$norm_sd_basic,
                  yend = 6*dnorm(
                    input$norm_mean_basic - 3*input$norm_sd_basic,
                    input$norm_mean_basic,
                    input$norm_sd_basic
                  )
                ),
                arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                color = "red"
              ) +
              annotate(
                geom = "text",
                label = "99.7%",
                x = 0,
                y = 0.015 + 6*dnorm(
                  input$norm_mean_basic + 3*input$norm_sd_basic,
                  input$norm_mean_basic,
                  input$norm_sd_basic
                ),
                color = "red"
              ) +

              scale_x_continuous(
                name = NULL,
                breaks = c(-3:3),
                labels = c(
                  expression(paste(mu, "\u2212", "3", sigma)),
                  expression(paste(mu, "\u2212", "2", sigma)),
                  expression(paste(mu, "\u2212", "1", sigma)),
                  expression(mu),
                  expression(paste(mu, "\u002B", "1", sigma)),
                  expression(paste(mu, "\u002B", "2", sigma)),
                  expression(paste(mu, "\u002B", "3", sigma))
                )
              )
          }
        }


        plot_common <- plot_common + plot_density + ylab("Density")


        if (input$pop_shape == "Uniform") {

          plot_common <- plot_common +
            geom_segment(
              aes(
                x = input$x_min_basic,
                y = 0,
                xend = input$x_min_basic,
                yend = 1/(input$x_max_basic - input$x_min_basic)
              ),
              color = input$curve_color_common,
              lwd = input$curve_lwd_common,
              linetype = "dotted"
            ) +
            geom_segment(
              aes(
                x = input$x_max_basic,
                y = 0,
                xend = input$x_max_basic,
                yend = 1/(input$x_max_basic - input$x_min_basic)
              ),
              color = input$curve_color_common,
              lwd = input$curve_lwd_common,
              linetype = "dotted"
            )
        }

      } else if (input$pop_dist == "advanced") {

        if (input$dist_type == "Continuous") {

          # density function
          dist_fxn <- switch(input$cont_fam,
            "Beta" = dbeta,
            "Cauchy" = dcauchy,
            "Chi-square" = dchisq,
            "Exponential" = dexp,
            "F" = df,
            "Gamma" = dgamma,
            "Laplace" = dlaplace,
            "Log-normal" = dlnorm,
            "Normal" = dnorm,
            "t" = dt,
            "Uniform (continuous)" = dunif,
            "Weibull" = dweibull
          )

          # parameters of pop. distribution for density curve
          params <- switch(input$cont_fam,
            "Beta" = c(input$beta_shape1, input$beta_shape2),
            "Cauchy" = c(input$cauchy_loc, input$cauchy_scale),
            "Chi-square" = input$chisq_df,
            "Exponential" = input$exp_rate,
            "F" = c(input$f_df1, input$f_df2),
            "Gamma" = c(input$gamma_shape, input$gamma_rate),
            "Laplace" = c(input$laplace_loc, input$laplace_scale),
            "Log-normal" = c(input$lognorm_mean, input$lognorm_sd),
            "Normal" = c(input$norm_mean, input$norm_sd),
            "t" = input$t_df,
            "Uniform (continuous)" = c(input$uni_min, input$uni_max),
            "Weibull" = c(input$weib_shape, input$weib_scale)
          )


          # x min for density curve
          x_min <- switch(input$cont_fam,
            "Beta" = 0,
            "Cauchy" = qcauchy(0.01, input$cauchy_loc, input$cauchy_scale),
            "Chi-square" = 0,
            "Exponential" = 0,
            "F" = 0,
            "Gamma" = 0,
            "Laplace" = qlaplace(0.001, input$laplace_loc, input$laplace_scale),
            "Log-normal" = qlnorm(0.001, input$lognorm_mean, input$lognorm_sd),
            "Normal" = qnorm(0.0001, input$norm_mean, input$norm_sd),
            "t" = qt(0.0001, input$t_df),
            "Uniform (continuous)" = input$uni_min,
            "Weibull" = 0
          )


          # x max for density curve
          x_max <- switch(input$cont_fam,
            "Beta" = 1,
            "Cauchy" = qcauchy(0.99, input$cauchy_loc, input$cauchy_scale),
            "Chi-square" = qchisq(0.999, input$chisq_df),
            "Exponential" = qexp(0.999, input$exp_rate),
            "F" = qf(0.98, input$f_df1, input$f_df2),
            "Gamma" = qgamma(0.999, input$gamma_shape, input$gamma_rate),
            "Laplace" = qlaplace(0.999, input$laplace_loc, input$laplace_scale),
            "Log-normal" = qlnorm(0.999, input$lognorm_mean, input$lognorm_sd),
            "Normal" = qnorm(0.9999, input$norm_mean, input$norm_sd),
            "t" = qt(0.9999, input$t_df),
            "Uniform (continuous)" = input$uni_max,
            "Weibull" = qweibull(0.999, input$weib_shape, input$weib_scale)
          )


          df_x <- data.frame(x = c(x_min, x_max))

          n <- 1e4 + 1


          plot_density <- stat_function(
            fun = dist_fxn,
            n = n,
            args = if (length(params) == 1) {
              list(params)
            } else if (length(params) == 2) {
              list(params[1], params[2])
            },
            color = input$curve_color_common,
            lwd = input$curve_lwd_common
          )


          if (!input$color_area_common) {

            plot_common <- ggplot(df_x, aes(x = x)) +
              plot_density +
              ylab("Density")

          } else if (input$color_area_common) {

            if (input$area_common_cont == "Left") {

              if (!is.na(input$cutoff_left_common_cont)) {

                plot_area <- stat_function(
                  fun = dist_fxn,
                  n = n,
                  args =
                    if (length(params) == 1) {
                      list(params)
                    } else if (length(params) == 2) {
                      list(params[1], params[2])
                    },
                  xlim = c(x_min, input$cutoff_left_common_cont),
                  geom = "area",
                  fill = input$area_color_common,
                  alpha = input$area_opac_common
                )


                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_area +
                  plot_density +
                  ylab("Density")

              } else {

                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_density +
                  ylab("Density")
              }

            } else if (input$area_common_cont == "Right") {

              if (!is.na(input$cutoff_right_common_cont)) {

                plot_area <- stat_function(
                  fun = dist_fxn,
                  n = n,
                  args =
                    if (length(params) == 1) {
                      list(params)
                    } else if (length(params) == 2) {
                      list(params[1], params[2])
                    },
                  xlim = c(input$cutoff_right_common_cont, x_max),
                  geom = "area",
                  fill = input$area_color_common,
                  alpha = input$area_opac_common
                )


                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_area +
                  plot_density +
                  ylab("Density")

              } else {

                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_density +
                  ylab("Density")
              }

            } else if (input$area_common_cont == "Between") {

              if (!is.na(input$cutoff_between_left_common_cont) &
                  !is.na(input$cutoff_between_right_common_cont)
              ) {

                plot_area <- stat_function(
                  fun = dist_fxn,
                  n = n,
                  args =
                    if (length(params) == 1) {
                      list(params)
                    } else if (length(params) == 2) {
                      list(params[1], params[2])
                    },
                  xlim = c(
                    input$cutoff_between_left_common_cont,
                    input$cutoff_between_right_common_cont
                  ),
                  geom = "area",
                  fill = input$area_color_common,
                  alpha = input$area_opac_common
                )


                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_area +
                  plot_density +
                  ylab("Density")

              } else {

                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_density +
                  ylab("Density")
              }

            } else if (input$area_common_cont == "Outside/Tails") {

              if (!is.na(input$cutoff_outside_left_common_cont) &
                  !is.na(input$cutoff_outside_right_common_cont)
              ) {

                plot_area_left <- stat_function(
                  fun = dist_fxn,
                  n = n,
                  args =
                    if (length(params) == 1) {
                      list(params)
                    } else if (length(params) == 2) {
                      list(params[1], params[2])
                    },
                  xlim = c(x_min, input$cutoff_outside_left_common_cont),
                  geom = "area",
                  fill = input$area_color_common,
                  alpha = input$area_opac_common
                )

                plot_area_right <- stat_function(
                  fun = dist_fxn,
                  n = n,
                  args =
                    if (length(params) == 1) {
                      list(params)
                    } else if (length(params) == 2) {
                      list(params[1], params[2])
                    },
                  xlim = c(input$cutoff_outside_right_common_cont, x_max),
                  geom = "area",
                  fill = input$area_color_common,
                  alpha = input$area_opac_common
                )

                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_area_left +
                  plot_area_right +
                  plot_density +
                  ylab("Density")

              } else {

                plot_common <- ggplot(df_x, aes(x = x)) +
                  plot_density +
                  ylab("Density")
              }
            }
          }


          if (input$cont_fam == "Uniform (continuous)") {

            plot_common <- plot_common +
              geom_segment(
                aes(
                  x = input$uni_min,
                  y = 0,
                  xend = input$uni_min,
                  yend = 1/(input$uni_max - input$uni_min)
                ),
                color = input$curve_color_common,
                lwd = input$curve_lwd_common,
                linetype = "dotted"
              ) +
              geom_segment(
                aes(
                  x = input$uni_max,
                  y = 0,
                  xend = input$uni_max,
                  yend = 1/(input$uni_max - input$uni_min)
                ),
                color = input$curve_color_common,
                lwd = input$curve_lwd_common,
                linetype = "dotted"
              )
          }



          if (input$x_max_common != "" & input$x_min_common != "") {

            x_min_common <- ifelse(
              input$x_min_common != "",
              as.numeric(input$x_min_common),
              NA
            )

            x_max_common <- ifelse(
              input$x_max_common != "",
              as.numeric(input$x_max_common),
              NA
            )


            if (input$x_tick_inc_common != "") {

              if (!is.na(as.numeric(input$x_tick_inc_common))) {
                if (as.numeric(input$x_tick_inc_common) > 0 &
                    as.numeric(input$x_max_common) > as.numeric(input$x_min_common)
                ) {

                  plot_common <- plot_common +
                    scale_x_continuous(
                      oob = oob_keep(),
                      breaks = c(
                        x_min_common,    # force x min to appear w/ repeat
                        seq(
                          x_min_common,
                          x_max_common,
                          by = as.numeric(input$x_tick_inc_common)
                        ),
                        x_max_common     # force x max to appear w/ repeat
                      ),
                      limits = c(x_min_common, x_max_common),
                      expand = c(0, 0)
                      #expand = expansion(mult = c(0, 0.04))
                    )
                }
              }

            } else if (input$x_tick_inc_common == "") {

              if (as.numeric(input$x_max_common) > as.numeric(input$x_min_common)) {

                plot_common <- plot_common +
                  scale_x_continuous(
                    oob = oob_keep(),
                    limits = c(x_min_common, x_max_common),
                    expand = c(0, 0)
                    #expand = expansion(mult = c(0, 0.04))
                  )

              } else {

                plot_common <- plot_common +
                  scale_x_continuous(
                    oob = oob_keep(),
                    limits = c(x_min_common, NA),
                    expand = c(0, 0)
                    #expand = expansion(mult = c(0, 0.04))
                  )
              }
            }

          } else if (input$x_max_common == "" & input$x_min_common != "") {

            plot_common <- plot_common +
              scale_x_continuous(
                oob = oob_keep(),
                limits = c(as.numeric(input$x_min_common), NA),
                expand = c(0, 0)
                #expand = expansion(mult = c(0, 0.04))
              )

          } else if (input$x_max_common != "" & input$x_min_common == "") {

            plot_common <- plot_common +
              scale_x_continuous(
                oob = oob_keep(),
                limits = c(NA, as.numeric(input$x_max_common)),
                expand = c(0, 0)
                #expand = expansion(mult = c(0, 0.04))
              )

          } else {

            plot_common <- plot_common +
              scale_x_continuous(oob = oob_keep(), expand = c(0, 0))
          }



          if (input$add_cutoff_common) {

            if (input$area_common_cont == "Left") {

              if (is.numeric(input$cutoff_left_common_cont)) {

                cutoff <- input$cutoff_left_common_cont

                plot_common <- plot_common +
                  geom_segment(aes(
                    x = cutoff,
                    y = 0,
                    xend = cutoff,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff, params[1], params[2])
                      }
                  ))
              }

            } else if (input$area_common_cont == "Right") {

              if (is.numeric(input$cutoff_right_common_cont)) {

                cutoff <- input$cutoff_right_common_cont

                plot_common <- plot_common +
                  geom_segment(aes(
                    x = cutoff,
                    y = 0,
                    xend = cutoff,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff, params[1], params[2])
                      }
                  ))
              }

            } else if (input$area_common_cont == "Between") {

              if (is.numeric(input$cutoff_between_left_common_cont) &
                  is.numeric(input$cutoff_between_right_common_cont)
              ) {

                cutoff_left <- input$cutoff_between_left_common_cont
                cutoff_right <- input$cutoff_between_right_common_cont

                plot_common <- plot_common +
                  geom_segment(aes(
                    x = cutoff_left,
                    y = 0,
                    xend = cutoff_left,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff_left, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff_left, params[1], params[2])
                      }
                  )) +
                  geom_segment(aes(
                    x = cutoff_right,
                    y = 0,
                    xend = cutoff_right,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff_right, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff_right, params[1], params[2])
                      }
                  ))
              }

            } else if (input$area_common_cont == "Outside/Tails") {

              if (is.numeric(input$cutoff_outside_left_common_cont) &
                  is.numeric(input$cutoff_outside_right_common_cont)
              ) {

                cutoff_left <- input$cutoff_outside_left_common_cont
                cutoff_right <- input$cutoff_outside_right_common_cont

                plot_common <- plot_common +
                  geom_segment(aes(
                    x = cutoff_left,
                    y = 0,
                    xend = cutoff_left,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff_left, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff_left, params[1], params[2])
                      }
                  )) +
                  geom_segment(aes(
                    x = cutoff_right,
                    y = 0,
                    xend = cutoff_right,
                    yend =
                      if (length(params) == 1) {
                        dist_fxn(cutoff_right, params)
                      } else if (length(params) == 2) {
                        dist_fxn(cutoff_right, params[1], params[2])
                      }
                  ))
              }
            }
          }

        } else if (input$dist_type == "Discrete") {

          if (input$disc_fam != "Uniform (discrete)") {

            # pop. distribution to randomly sample from
            dist_fxn <- switch(input$disc_fam,
              "Bernoulli" = dbinom,
              "Binomial" = dbinom,
              "Geometric" = dgeom,
              "Hypergeometric" = dhyper,
              "Negative Binomial" = dnbinom,
              "Poisson" = dpois
            )

            # parameters of pop. distribution
            params <- switch(input$disc_fam,
              "Bernoulli" = c(1, input$bern_p),
              "Binomial" = c(input$bin_n, input$bin_p),
              "Geometric" = input$geom_p,
              "Hypergeometric" = c(input$hyper_K, input$hyper_N - input$hyper_K,
                input$hyper_n
              ),
              "Negative Binomial" = c(input$negbin_r, input$negbin_p),
              "Poisson" = input$pois_rate
            )


            # min x
            x_min <- 0

            # max x
            x_max <- switch(input$disc_fam,
              "Bernoulli" = 1,
              "Binomial" = input$bin_n,
              "Geometric" = qgeom(0.999, input$geom_p),
              "Hypergeometric" = qhyper(0.999, input$hyper_K,
                input$hyper_N - input$hyper_K, input$hyper_n
              ),
              "Negative Binomial" = qnbinom(0.999, input$negbin_r,
                input$negbin_p
              ),
              "Poisson" = qpois(0.999, input$pois_rate)
            )


            df_xy <- data.frame(x = seq(0, x_max, by = 1))

            df_xy$y <- if (length(params) == 1) {
              dist_fxn(df_xy$x, params)
            } else if (length(params) == 2) {
              dist_fxn(df_xy$x, params[1], params[2])
            } else if (length(params) == 3) {
              dist_fxn(df_xy$x, params[1], params[2], params[3])
            }

          } else if (input$disc_fam == "Uniform (discrete)") {

            df_xy <- data.frame(
              x = seq(1, input$uni_n, by = 1),
              y = 1/input$uni_n
            )
          }


          plot_common <- ggplot(df_xy, aes(x = factor(x), y = y)) +
            geom_bar(
              stat = "identity",
              fill = input$fill_color_common,
              color = "black",
              width = if (input$disc_fam == "Bernoulli") 0.5
            ) +
            xlab("x") +
            ylab("P(x)")


          if (input$disc_fam == "Bernoulli") {
            plot_common <- plot_common +
              scale_x_discrete(breaks = as.character(0:1))
          }



          if (input$x_max_common != "" & input$x_min_common != "") {

            x_min_common <- ifelse(
              input$x_min_common != "",
              as.numeric(input$x_min_common),
              NA
            )

            x_max_common <- ifelse(
              input$x_max_common != "",
              as.numeric(input$x_max_common),
              NA
            )


            if (input$x_tick_inc_common != "") {

              if (!is.na(as.numeric(input$x_tick_inc_common))) {
                if (as.numeric(input$x_tick_inc_common) > 0 &
                    as.numeric(input$x_max_common) > as.numeric(input$x_min_common)
                ) {

                  plot_common <- plot_common +
                    scale_x_discrete(
                      breaks = as.character(
                        seq(
                          x_min_common,
                          x_max_common,
                          as.numeric(input$x_tick_inc_common)
                        )
                      ),
                      limits = as.character(seq(x_min_common, x_max_common, 1))
                    )
                }
              }

            } else if (input$x_tick_inc_common == "") {

              if (as.numeric(input$x_max_common) > as.numeric(input$x_min_common)) {

                plot_common <- plot_common +
                  scale_x_discrete(
                    limits = as.character(seq(x_min_common, x_max_common, 1))
                  )

              } else {

                plot_common <- plot_common +
                  scale_x_discrete(
                    limits = c(x_min_common, NA)
                  )
              }
            }

          } else if (input$x_max_common == "" & input$x_min_common != "") {

            plot_common <- plot_common +
              scale_x_discrete(
                limits = as.character(seq(
                  as.numeric(input$x_min_common),
                  x_max,
                  1
                ))
              )

          } else if (input$x_max_common != "" & input$x_min_common == "") {

            plot_common <- plot_common +
              scale_x_discrete(
                limits = as.character(seq(
                  x_min,
                  as.numeric(input$x_max_common),
                  1
                ))
              )
          }



          if (input$color_area_common) {

            plot_common <- plot_common +
              geom_bar(
                data = switch(input$area_common_disc,
                  "one" = filter(df_xy, x == input$bar_value),
                  "left" = filter(df_xy, x <= input$cutoff_left_common_disc),
                  "right" = filter(df_xy, x >= input$cutoff_right_common_disc),
                  "between" = filter(df_xy,
                    x >= input$cutoff_between_left_common_disc &
                    x <= input$cutoff_between_right_common_disc
                  ),
                  "outside" = filter(df_xy,
                    x <= input$cutoff_outside_left_common_disc |
                    x >= input$cutoff_outside_right_common_disc
                  )
                ),
                stat = "identity",
                fill = input$area_color_common,
                color = "black",
                alpha = input$area_opac_common
              )
          }
        }
      }


      ### change max y, but only if doesn't cut off top of plot
      max_y_plot_current <- ggplot_build(plot_common)$layout$panel_params[[1]]$y.range[2]

      if (input$y_max_common != "") {
        if (!is.na(as.numeric(input$y_max_common))) {
          if (as.numeric(input$y_max_common) > max_y_plot_current) {
            max_y <- as.numeric(input$y_max_common)
          } else {
            max_y <- NA
          }
        }
      } else {
        max_y <- NA
      }


      plot_common <- plot_common +
        scale_y_continuous(
          limits = c(0, max_y),
          expand = expansion(mult = c(0, 0.015))
        ) +
        theme_classic() +
        theme(
          axis.title = element_text(size = ifelse(
            input$axis_lab_size_common == "",
            20,
            input$axis_lab_size_common
          )),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 5),
          axis.text = element_text(
            size = ifelse(
              input$axis_text_size_common == "",
              16,
              input$axis_text_size_common
            ),
            color = "black"
          ),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(3, "mm"),
          plot.margin = unit(c(10, 10, 10, 10), "mm")
        )


      if (!input$keep_x_axis_line_common) {
        plot_common <- plot_common + theme(axis.line.x = element_blank())
      }


      if (!input$keep_x_axis_lab_common) {
        plot_common <- plot_common + theme(axis.title.x = element_blank())
      }


      if (!input$keep_x_axis_text_common) {
        plot_common <- plot_common +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }


      if (!input$keep_y_axis_line_common) {
        plot_common <- plot_common + theme(axis.line.y = element_blank())
      }


      if (!input$keep_y_axis_lab_common) {
        plot_common <- plot_common + theme(axis.title.y = element_blank())
      }


      if (!input$keep_y_axis_text_common) {
        plot_common <- plot_common +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }


      plot_common
    }
  })



  # update X axis label
  observeEvent(input$x_lab_update_common, {

    x_lab <- clean_label(input$x_lab_common)

    bag$plot_common_xlab <- xlab(parse(text = x_lab))


    bag$xlab_status_common <- 1
  })



  # update Y axis label
  observeEvent(input$y_lab_update_common, {

    y_lab <- clean_label(input$y_lab_common)

    bag$plot_common_ylab <- ylab(parse(text = y_lab))


    bag$ylab_status_common <- 1
  })




  ### make popup UI for adding annotations, arrow, and/or line segments
  ########################################################################
  output$annotate_popup_common <- renderUI({

    if (bag$do_plot_common == FALSE) {

      wellPanel(
        tagList(
          strong("Warning! You must first make a plot to annotate."),

          br(),
          br()
        )
      )

    } else {

      wellPanel(
        tagList(

          if (bag$n_conds == 0) {

            list(
              strong("Click 'Add Another' below to begin."),

              br(),
              br()
            )

          } else if (bag$n_conds >= 1) {

            list(
              strong("If adding text, go to the 'Help' tab to view options for
                including special text such as Greek letters, subscripts,
                superscripts, etc."
              ),

              br(),
              br(),


              lapply(1:bag$n_conds, function(i) {

                list(
                  fluidRow(
                    column(12,
                      fluidRow(
                        column(3,
                          fluidRow(
                            column(6,
                              radioButtons(paste0("add_type_", i), "Type",
                                choices = c(
                                  "Text" = "text",
                                  "Arrow" = "arrow",
                                  "Segment" = "segment"
                                ),
                                selected = input[[paste0("add_type_", i)]]
                              )
                            )
                          )
                        ),


                        conditionalPanel(
                          paste0("condition =", paste0("input.add_type_", i),
                            "== 'text'"
                          ),

                          column(6,
                            if (is.null(input[[paste0("add_text_", i)]])) {
                              textAreaInput(paste0("add_text_", i),
                                "Text",
                                height = "34px",
                                resize = "none",
                                placeholder = "Type text here."
                              )
                            } else if (input[[paste0("add_text_", i)]] == "") {
                              textAreaInput(paste0("add_text_", i),
                                "Text",
                                height = "34px",
                                resize = "none",
                                placeholder = "Type text here."
                              )
                            } else {
                              textAreaInput(paste0("add_text_", i),
                                "Text",
                                height = "34px",
                                resize = "none",
                                value = input[[paste0("add_text_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_text_size_", i)]])) {
                              numericInput(paste0("add_text_size_", i),
                                "Size",
                                value = 6
                              )
                            } else {
                              numericInput(paste0("add_text_size_", i),
                                "Size",
                                value = input[[paste0("add_text_size_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_text_loc_x_", i)]])) {
                              numericInput(paste0("add_text_loc_x_", i),
                                "X coord.",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_text_loc_x_", i),
                                "X coord.",
                                value = input[[paste0("add_text_loc_x_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_text_loc_y_", i)]])) {
                              numericInput(paste0("add_text_loc_y_", i),
                                "Y coord.",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_text_loc_y_", i),
                                "Y coord.",
                                value = input[[paste0("add_text_loc_y_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (is.null(input[[paste0("add_text_color_", i)]])) {
                              colourInput(paste0("add_text_color_", i),
                                "Color",
                                value = "black",
                                showColour = "background"
                              )
                            } else {
                              colourInput(paste0("add_text_color_", i),
                                "Color",
                                value = input[[paste0("add_text_color_", i)]],
                                showColour = "background"
                              )
                            }
                          )
                        ),


                        conditionalPanel(
                          paste0("condition =", paste0("input.add_type_", i),
                            "== 'arrow'"
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_arrow_start_loc_x_", i)]])) {
                              numericInput(paste0("add_arrow_start_loc_x_", i),
                                "Start X",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_arrow_start_loc_x_", i),
                                "Start X",
                                value = input[[paste0("add_arrow_start_loc_x_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_arrow_start_loc_y_", i)]])) {
                              numericInput(paste0("add_arrow_start_loc_y_", i),
                                "Start Y",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_arrow_start_loc_y_", i),
                                "Start Y",
                                value = input[[paste0("add_arrow_start_loc_y_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_arrow_lwd_", i)]])) {
                              sliderInput(paste0("add_arrow_lwd_", i),
                                "Width", min = 0, max = 3, step = 0.1,
                                value = 1, ticks = FALSE
                              )
                            } else {
                              sliderInput(paste0("add_arrow_lwd_", i),
                                "Width", min = 0, max = 3, step = 0.1,
                                value = input[[paste0("add_arrow_lwd_", i)]],
                                ticks = FALSE
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_arrow_end_loc_x_", i)]])) {
                              numericInput(paste0("add_arrow_end_loc_x_", i),
                                "End X",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_arrow_end_loc_x_", i),
                                "End X",
                                value = input[[paste0("add_arrow_end_loc_x_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_arrow_end_loc_y_", i)]])) {
                              numericInput(paste0("add_arrow_end_loc_y_", i),
                                "End Y",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_arrow_end_loc_y_", i),
                                "End Y",
                                value = input[[paste0("add_arrow_end_loc_y_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (is.null(input[[paste0("add_arrow_color_", i)]])) {
                              colourInput(paste0("add_arrow_color_", i),
                                "Color",
                                value = "black",
                                showColour = "background"
                              )
                            } else {
                              colourInput(paste0("add_arrow_color_", i),
                                "Color",
                                value = input[[paste0("add_arrow_color_", i)]],
                                showColour = "background"
                              )
                            }
                          )
                        ),

                        conditionalPanel(
                          paste0("condition =", paste0("input.add_type_", i),
                            "== 'segment'"
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_segment_start_loc_x_", i)]])) {
                              numericInput(paste0("add_segment_start_loc_x_", i),
                                "Start X",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_segment_start_loc_x_", i),
                                "Start X",
                                value = input[[paste0("add_segment_start_loc_x_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_segment_start_loc_y_", i)]])) {
                              numericInput(paste0("add_segment_start_loc_y_", i),
                                "Start Y",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_segment_start_loc_y_", i),
                                "Start Y",
                                value = input[[paste0("add_segment_start_loc_y_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_segment_lwd_", i)]])) {
                              sliderInput(paste0("add_segment_lwd_", i),
                                "Width", min = 0, max = 3, step = 0.1,
                                value = 1, ticks = FALSE
                              )
                            } else {
                              sliderInput(paste0("add_segment_lwd_", i),
                                "Width", min = 0, max = 3, step = 0.1,
                                value = input[[paste0("add_segment_lwd_", i)]],
                                ticks = FALSE
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_segment_end_loc_x_", i)]])) {
                              numericInput(paste0("add_segment_end_loc_x_", i),
                                "End X",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_segment_end_loc_x_", i),
                                "End X",
                                value = input[[paste0("add_segment_end_loc_x_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (!is.numeric(input[[paste0("add_segment_end_loc_y_", i)]])) {
                              numericInput(paste0("add_segment_end_loc_y_", i),
                                "End Y",
                                value = 0
                              )
                            } else {
                              numericInput(paste0("add_segment_end_loc_y_", i),
                                "End Y",
                                value = input[[paste0("add_segment_end_loc_y_", i)]]
                              )
                            }
                          ),

                          column(3,
                            if (is.null(input[[paste0("add_segment_color_", i)]])) {
                              colourInput(paste0("add_segment_color_", i),
                                "Color",
                                value = "black",
                                showColour = "background"
                              )
                            } else {
                              colourInput(paste0("add_segment_color_", i),
                                "Color",
                                value = input[[paste0("add_segment_color_", i)]],
                                showColour = "background"
                              )
                            }
                          )
                        )
                      )
                    )
                  ),

                  hr()
                )
              })
            )
          },

          br(),

          column(12,
            fluidRow(
              div(style = "display: inline-block",

                actionButton("add_annotation", "Add Another",
                  class = "btn btn-success", icon = icon("plus")
                ),

                actionButton("annotate_plot", "Annotate Plot",
                  class = "btn btn-primary", icon = icon("edit")
                ),

                actionButton("reset_conditions", "Reset",
                  class = "btn btn-danger", icon = icon("redo-alt")
                )
              )
            )
          ),

          br()
        )
      )
    }
  })



  ### add condition for annotating plot
  ######################################
  observeEvent(input$add_annotation, {

    bag$n_conds <- bag$n_conds + 1
  })



  ### annotate plot
  ######################################
  observeEvent(input$annotate_plot, {

    req(bag$n_conds > 0)


    bag$plot_common_annot <- list()
    bag$plot_common_annot_max_y <- vector()


    for (i in 1:bag$n_conds) {

      if (input[[paste0("add_type_", i)]] == "text") {

        if (nchar(input[[paste0("add_text_", i)]]) == 0) {
          shinyalert("Warning!", "The text must contain at least one character.",
            type = "error"
          )
        }

        validate(need(nchar(input[[paste0("add_text_", i)]]) > 0, ""))


        text_lab <- clean_label(input[[paste0("add_text_", i)]])


        bag$plot_common_annot[[i]] <- annotate(
          geom = "text",
          label = text_lab,
          x = input[[paste0("add_text_loc_x_", i)]],
          y = input[[paste0("add_text_loc_y_", i)]],
          size = input[[paste0("add_text_size_", i)]],
          color = input[[paste0("add_text_color_", i)]],
          parse = TRUE
        )

        bag$plot_common_annot_max_y[i] <- input[[paste0("add_text_loc_y_", i)]]

      } else if (input[[paste0("add_type_", i)]] == "arrow") {

        bag$plot_common_annot[[i]] <- annotate(
          geom = "segment",
          x = input[[paste0("add_arrow_start_loc_x_", i)]],
          y = input[[paste0("add_arrow_start_loc_y_", i)]],
          xend = input[[paste0("add_arrow_end_loc_x_", i)]],
          yend = input[[paste0("add_arrow_end_loc_y_", i)]],
          arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
          lwd = input[[paste0("add_arrow_lwd_", i)]],
          col = input[[paste0("add_arrow_color_", i)]]
        )

        bag$plot_common_annot_max_y[i] <- max(
          input[[paste0("add_arrow_start_loc_y_", i)]],
          input[[paste0("add_arrow_end_loc_y_", i)]]
        )

      } else if (input[[paste0("add_type_", i)]] == "segment") {

        bag$plot_common_annot[[i]] <- annotate(
          geom = "segment",
          x = input[[paste0("add_segment_start_loc_x_", i)]],
          y = input[[paste0("add_segment_start_loc_y_", i)]],
          xend = input[[paste0("add_segment_end_loc_x_", i)]],
          yend = input[[paste0("add_segment_end_loc_y_", i)]],
          lwd = input[[paste0("add_segment_lwd_", i)]],
          col = input[[paste0("add_segment_color_", i)]]
        )

        bag$plot_common_annot_max_y[i] <- max(
          input[[paste0("add_segment_start_loc_y_", i)]],
          input[[paste0("add_segment_end_loc_y_", i)]]
        )
      }
    }


    bag$annotated_status <- 1

  })




  ### reset conditions for annotating plot
  ############################################
  observeEvent(input$reset_conditions, {

    req(bag$n_conds > 0)


    # reset previous conditions
    for (i in 1:bag$n_conds) {

      updateRadioButtons(session, paste0("add_type_", i),
        selected = "text"
      )

      if (input[[paste0("add_type_", i)]] == "text") {

        updateTextAreaInput(session, paste0("add_text_", i),
          value = NULL
        )

        updateNumericInput(session, paste0("add_text_loc_x_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_text_loc_y_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_text_size_", i),
          value = 6
        )

        updateNumericInput(session, paste0("add_text_color_", i),
          value = "white"
        )

      } else if (input[[paste0("add_type_", i)]] == "arrow") {

        updateNumericInput(session, paste0("add_arrow_start_loc_x_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_arrow_start_loc_y_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_arrow_end_loc_x_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_arrow_end_loc_y_", i),
          value = 0
        )

        updateSliderInput(session, paste0("add_arrow_lwd_", i),
          value = 1
        )

        updateColourInput(session, paste0("add_arrow_color_", i),
          value = "white"
        )

      } else if (input[[paste0("add_type_", i)]] == "segment") {

        updateNumericInput(session, paste0("add_segment_start_loc_x_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_segment_start_loc_y_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_segment_end_loc_x_", i),
          value = 0
        )

        updateNumericInput(session, paste0("add_segment_end_loc_y_", i),
          value = 0
        )

        updateSliderInput(session, paste0("add_segment_lwd_", i),
          value = 1
        )

        updateColourInput(session, paste0("add_segment_color_", i),
          value = "white"
        )
      }


      # reset annotation addition input
      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_type_", i)
      )


      # reset text inputs
      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_text_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_text_loc_x_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_text_loc_y_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_text_size_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_text_size_", i)
      )


      # reset arrow inputs
      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_start_loc_x_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_start_loc_y_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_end_loc_x_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_end_loc_y", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_lwd_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_arrow_color_", i)
      )


      # reset segment inputs
      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_start_loc_x_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_start_loc_y_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_end_loc_x_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_end_loc_y", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_lwd_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("add_segment_color_", i)
      )
    }


    # reset number of conditions to 0
    bag$n_conds <- 0
    bag$annotated_status <- 0

  })



  # display and store plot
  observe({
    output$plot_common <- renderPlot({

      plot <- make_init_plot_common()

      if (bag$xlab_status_common == 1) plot <- plot + bag$plot_common_xlab

      if (bag$ylab_status_common == 1) plot <- plot + bag$plot_common_ylab

      if (bag$annotated_status == 1) {

        plot_max_y <- ggplot_build(plot)$layout$panel_params[[1]]$y.range[2]

        # keep annotations above plot from getting cut off
        if (max(bag$plot_common_annot_max_y) > plot_max_y) {

          plot <- plot +
            coord_cartesian(clip = "off") +
            bag$plot_common_annot

        } else {

          plot <- plot + bag$plot_common_annot
        }
      }


      (bag$plot_common <- plot)


    }, width = plot_width_common(), height = 400)
  })




  ### make popup UI for downloading plot
  ###########################################################
  output$download_popup_common <- renderUI({

    req(bag$plot_common)

    list(
      column(12, align = "center",
        wellPanel(
          h3("Use a transparent background:"),
          br(),

          downloadButton("dl_plot_common_trans", "Yes",
            class = "btn btn-success", icon = icon("download")
          ),
          downloadButton("dl_plot_common_notrans", "No",
            class = "btn btn-danger", icon = icon("download")
          )
        )
      )
    )
  })




  ##### download plot w/ transparent background -- common
  ###########################################################
  output$dl_plot_common_trans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  dl_plot <- bag$plot_common +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA)
        )

		  ggsave(file,
		    dl_plot,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_common()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)



  ##### download plot w/o transparent background -- common
  ###########################################################
  output$dl_plot_common_notrans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  ggsave(file,
		    bag$plot_common,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_common()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)




  ##############################################################################
  ######################### Plot Multiple Distributions ########################
  ##############################################################################

  ### make well panel UI for selecting distributions
  ############################################################
  output$multiple_ui <- renderUI({

    wellPanel(
      tagList(
        if (bag$n_curves == 0) {

          list(
            strong("Click 'Add Curve to List' below to get started."),

            br(),
            br()
          )

        } else if (bag$n_curves >= 1) {

          list(
            lapply(1:bag$n_curves, function(i) {

              list(
                fluidRow(
                  column(12,
                    fluidRow(
                      column(3,
                        strong(paste0("Distribution ", i, ":"))
                      ),

                      column(4,
                        if (is.null(input[[paste0("multiple_fam_", i)]])) {
                          selectInput(paste0("multiple_fam_", i), "Family",
                            choices = c("Beta", "Cauchy", "Chi-square",
                              "Exponential", "F", "Gamma", "Laplace",
                              "Log-normal", "Normal", "t",
                              "Uniform (continuous)", "Weibull"
                            ),
                            selected = "Normal"
                          )
                        } else {
                          selectInput(paste0("multiple_fam_", i), "Family",
                            choices = c("Beta", "Cauchy", "Chi-square",
                              "Exponential", "F", "Gamma", "Laplace",
                              "Log-normal", "Normal", "t",
                              "Uniform (continuous)", "Weibull"
                            ),
                            selected = input[[paste0("multiple_fam_", i)]]
                          )
                        }
                      ),

                      column(3,
                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Beta'"
                          ),

                          if (!is.numeric(
                            input[[paste0("beta_shape1_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("beta_shape1_mult_", i),
                              "Shape 1",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("beta_shape1_mult_", i),
                              "Shape 1",
                              value = input[[paste0("beta_shape1_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("beta_shape2_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("beta_shape2_mult_", i),
                              "Shape 2",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("beta_shape2_mult_", i),
                              "Shape 2",
                              value = input[[paste0("beta_shape2_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Cauchy'"
                          ),

                          if (!is.numeric(
                            input[[paste0("cauchy_loc_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("cauchy_loc_mult_", i),
                              "Location",
                              value = 0
                            )
                          } else {
                            numericInput(
                              paste0("cauchy_loc_mult_", i),
                              "Location",
                              value = input[[paste0("cauchy_loc_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("cauchy_scale_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("cauchy_scale_mult_", i),
                              "Scale",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("cauchy_scale_mult_", i),
                              "Scale",
                              value = input[[paste0("cauchy_scale_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Chi-square'"
                          ),

                          if (!is.numeric(
                            input[[paste0("chisq_df_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("chisq_df_mult_", i),
                              "DF",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("chisq_df_mult_", i),
                              "DF",
                              value = input[[paste0("chisq_df_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Exponential'"
                          ),

                          if (!is.numeric(
                            input[[paste0("exp_rate_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("exp_rate_mult_", i),
                              "Rate",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("exp_rate_mult_", i),
                              "Rate",
                              value = input[[paste0("exp_rate_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'F'"
                          ),

                          if (!is.numeric(
                            input[[paste0("f_df1_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("f_df1_mult_", i),
                              "DF 1",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("f_df1_mult_", i),
                              "DF 1",
                              value = input[[paste0("f_df1_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("f_df2_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("f_df2_mult_", i),
                              "DF 2",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("f_df2_mult_", i),
                              "DF 2",
                              value = input[[paste0("f_df2_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Gamma'"
                          ),

                          if (!is.numeric(
                            input[[paste0("gamma_shape_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("gamma_shape_mult_", i),
                              "Shape",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("gamma_shape_mult_", i),
                              "Shape",
                              value = input[[paste0("gamma_shape_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("gamma_rate_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("gamma_rate_mult_", i),
                              "Rate",
                              value = 5
                            )
                          } else {
                            numericInput(
                              paste0("gamma_rate_mult_", i),
                              "Rate",
                              value = input[[paste0("gamma_rate_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Laplace'"
                          ),

                          if (!is.numeric(
                            input[[paste0("laplace_loc_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("laplace_loc_mult_", i),
                              "Location",
                              value = 0
                            )
                          } else {
                            numericInput(
                              paste0("laplace_loc_mult_", i),
                              "Location",
                              value = input[[paste0("laplace_loc_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("laplace_scale_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("laplace_scale_mult_", i),
                              "Scale",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("laplace_scale_mult_", i),
                              "Scale",
                              value = input[[paste0("laplace_scale_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Log-normal'"
                          ),

                          if (!is.numeric(
                            input[[paste0("lognorm_mean_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("lognorm_mean_mult_", i),
                              "Mean",
                              value = 0
                            )
                          } else {
                            numericInput(
                              paste0("lognorm_mean_mult_", i),
                              "Mean",
                              value = input[[paste0("lognorm_mean_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("lognorm_sd_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("lognorm_sd_mult_", i),
                              "Std. Dev.",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("lognorm_sd_mult_", i),
                              "Std. Dev.",
                              value = input[[paste0("lognorm_sd_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Normal'"
                          ),

                          if (!is.numeric(
                            input[[paste0("norm_mean_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("norm_mean_mult_", i),
                              "Mean",
                              value = 0
                            )
                          } else {
                            numericInput(
                              paste0("norm_mean_mult_", i),
                              "Mean",
                              value = input[[paste0("norm_mean_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("norm_sd_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("norm_sd_mult_", i),
                              "Std. Dev.",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("norm_sd_mult_", i),
                              "Std. Dev.",
                              value = input[[paste0("norm_sd_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 't'"
                          ),

                          if (!is.numeric(
                            input[[paste0("t_df_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("t_df_mult_", i),
                              "DF",
                              value = 10
                            )
                          } else {
                            numericInput(
                              paste0("t_df_mult_", i),
                              "DF",
                              value = input[[paste0("t_df_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Uniform (continuous)'"
                          ),

                          if (!is.numeric(
                            input[[paste0("uni_min_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("uni_min_mult_", i),
                              "Minimum",
                              value = 0
                            )
                          } else {
                            numericInput(
                              paste0("uni_min_mult_", i),
                              "Minimum",
                              value = input[[paste0("uni_min_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("uni_max_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("uni_max_mult_", i),
                              "Maximum",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("uni_max_mult_", i),
                              "Maximum",
                              value = input[[paste0("uni_max_mult_", i)]]
                            )
                          }
                        ),


                        conditionalPanel(
                          paste0(
                            "condition =",
                            paste0("input.multiple_fam_", i),
                            "== 'Weibull'"
                          ),

                          if (!is.numeric(
                            input[[paste0("weib_shape_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("weib_shape_mult_", i),
                              "Shape",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("weib_shape_mult_", i),
                              "Shape",
                              value = input[[paste0("weib_shape_mult_", i)]]
                            )
                          },

                          if (!is.numeric(
                            input[[paste0("weib_scale_mult_", i)]]
                          )) {
                            numericInput(
                              paste0("weib_scale_mult_", i),
                              "Scale",
                              value = 1
                            )
                          } else {
                            numericInput(
                              paste0("weib_scale_mult_", i),
                              "Scale",
                              value = input[[paste0("weib_scale_mult_", i)]]
                            )
                          }
                        )
                      ),

                      column(2,
                        if (is.null(input[[paste0("line_color_mult_", i)]])) {
                          colourInput(paste0("line_color_mult_", i),
                            "Color",
                            value = "black",
                            showColour = "background"
                          )
                        } else {
                          colourInput(paste0("line_color_mult_", i),
                            "Color",
                            value = input[[paste0("line_color_mult_", i)]],
                            showColour = "background"
                          )
                        }
                      )
                    )
                  )
                ),

                hr()
              )
            })
          )
        },

        br(),

        column(12, align = "center",
          actionButton("add_curve_to_list", "Add Curve to List",
            class = "btn btn-success", icon = icon("plus")
          ),

          stri_dup(intToUtf8(160), 10),

          actionButton("update_plot", "Update Plot", class = "btn btn-primary",
            icon = icon("bolt")
          ),

          stri_dup(intToUtf8(160), 10),

          actionButton("reset_multiple", "Reset Plot",
            class = "btn btn-danger", icon = icon("redo-alt")
          )
        ),

        br()
      )
    )
  })




  ### add curve to list
  ###############################################
  observeEvent(input$add_curve_to_list, {

    bag$n_curves <- bag$n_curves + 1

    bag$add_curve_status <- 0
  })




  ### update plot
  ######################################
  observeEvent(input$update_plot, {

    req(bag$n_curves > 0)

    bag$add_curve_status <- 1
  })




  ### make plot
  ######################################
  make_init_plot_mult <- reactive({

    req(bag$add_curve_status == 1 & bag$n_curves > 0)


    df_mult <- data.frame(x = c(-4, 4))

    plot_mult <- ggplot(df_mult, aes(x = x))

    n <- 1e4 + 1


    line_color <- vector()
    legend_lab <- vector()
    x_min <- vector()
    x_max <- vector()


    for (i in 1:bag$n_curves) {

      # density function
      dist_fxn <- switch(input[[paste0("multiple_fam_", i)]],
        "Beta" = dbeta,
        "Cauchy" = dcauchy,
        "Chi-square" = dchisq,
        "Exponential" = dexp,
        "F" = df,
        "Gamma" = dgamma,
        "Laplace" = dlaplace,
        "Log-normal" = dlnorm,
        "Normal" = dnorm,
        "t" = dt,
        "Uniform (continuous)" = dunif,
        "Weibull" = dweibull
      )


      # parameters of pop. distribution for density curve
      params <- switch(input[[paste0("multiple_fam_", i)]],

        "Beta" = c(
          input[[paste0("beta_shape1_mult_", i)]],
          input[[paste0("beta_shape2_mult_", i)]]
        ),

        "Cauchy" = c(
          input[[paste0("cauchy_loc_mult_", i)]],
          input[[paste0("cauchy_scale_mult_", i)]]
        ),

        "Chi-square" = input[[paste0("chisq_df_mult_", i)]],

        "Exponential" = input[[paste0("exp_rate_mult_", i)]],

        "F" = c(
          input[[paste0("f_df1_mult_", i)]],
          input[[paste0("f_df2_mult_", i)]]
        ),

        "Gamma" = c(
          input[[paste0("gamma_shape_mult_", i)]],
          input[[paste0("gamma_rate_mult_", i)]]
        ),

        "Laplace" = c(
          input[[paste0("laplace_loc_mult_", i)]],
          input[[paste0("laplace_scale_mult_", i)]]
        ),

        "Log-normal" = c(
          input[[paste0("lognorm_mean_mult_", i)]],
          input[[paste0("lognorm_sd_mult_", i)]]
        ),

        "Normal" = c(
          input[[paste0("norm_mean_mult_", i)]],
          input[[paste0("norm_sd_mult_", i)]]
        ),

        "t" = input[[paste0("t_df_mult_", i)]],

        "Uniform (continuous)" = c(
          input[[paste0("uni_min_mult_", i)]],
          input[[paste0("uni_max_mult_", i)]]
        ),

        "Weibull" = c(
          input[[paste0("weib_shape_mult_", i)]],
          input[[paste0("weib_scale_mult_", i)]]
        )
      )


      if (input[[paste0("multiple_fam_", i)]] == "Beta") {

        issue_beta1 <- any(
          c(
            input[[paste0("beta_shape1_mult_", i)]],
            input[[paste0("beta_shape2_mult_", i)]]
          ) <= 0
        )

        issue_beta2 <- anyNA(c(
          input[[paste0("beta_shape1_mult_", i)]],
          input[[paste0("beta_shape2_mult_", i)]]
        ))

        if (issue_beta1 | issue_beta2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_beta1, issue_beta2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Cauchy") {

        issue_cauchy1 <- input[[paste0("cauchy_scale_mult_", i)]] <= 0

        issue_cauchy2 <- anyNA(c(
          input[[paste0("cauchy_loc_mult_", i)]],
          input[[paste0("cauchy_scale_mult_", i)]]
        ))

        if (issue_cauchy1 | issue_cauchy2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_cauchy1, issue_cauchy2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Chi-square") {

        issue_chisq1 <- input[[paste0("chisq_df_mult_", i)]] <= 0

        issue_chisq2 <- is.na(input[[paste0("chisq_df_mult_", i)]])

        if (issue_chisq1 | issue_chisq2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_chisq1, issue_chisq2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Exponential") {

        issue_exp1 <- input[[paste0("exp_rate_mult_", i)]] <= 0

        issue_exp2 <- is.na(input[[paste0("exp_rate_mult_", i)]])

        if (issue_exp1 | issue_exp2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_exp1, issue_exp2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "F") {

        issue_f1 <- any(
          c(
            input[[paste0("f_df1_mult_", i)]],
            input[[paste0("f_df2_mult_", i)]]
          ) <= 0
        )

        issue_f2 <- anyNA(c(
          input[[paste0("f_df1_mult_", i)]],
          input[[paste0("f_df2_mult_", i)]]
        ))

        if (issue_f1 | issue_f2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_f1, issue_f2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Gamma") {

        issue_gamma1 <- any(
          c(
            input[[paste0("gamma_shape_mult_", i)]],
            input[[paste0("gamma_rate_mult_", i)]]
          ) <= 0
        )

        issue_gamma2 <- anyNA(c(
          input[[paste0("gamma_shape_mult_", i)]],
          input[[paste0("gamma_rate_mult_", i)]]
        ))

        if (issue_gamma1 | issue_gamma2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_gamma1, issue_gamma2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Laplace") {

        issue_laplace1 <- input[[paste0("laplace_scale_mult_", i)]] <= 0

        issue_laplace2 <- anyNA(c(
          input[[paste0("laplace_loc_mult_", i)]],
          input[[paste0("laplace_scale_mult_", i)]]
        ))

        if (issue_laplace1 | issue_laplace2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_laplace1, issue_laplace2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Log-normal") {

        issue_lognorm1 <- input[[paste0("lognorm_sd_mult_", i)]] <= 0

        issue_lognorm2 <- anyNA(c(
          input[[paste0("lognorm_mean_mult_", i)]],
          input[[paste0("lognorm_sd_mult_", i)]]
        ))

        if (issue_lognorm1 | issue_lognorm2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_lognorm1, issue_lognorm2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Normal") {

        issue_norm1 <- input[[paste0("norm_sd_mult_", i)]] < 0

        issue_norm2 <- anyNA(c(
          input[[paste0("norm_mean_mult_", i)]],
          input[[paste0("norm_sd_mult_", i)]]
        ))

        if (issue_norm1 | issue_norm2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_norm1, issue_norm2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "t") {

        issue_t1 <- input[[paste0("t_df_mult_", i)]] <= 0

        issue_t2 <- is.na(input[[paste0("t_df_mult_", i)]])

        if (issue_t1 | issue_t2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_t1, issue_t2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Uniform (continuous)") {

        issue_uni1 <- input[[paste0("uni_min_mult_", i)]] >=
          input[[paste0("uni_max_mult_", i)]]

        issue_uni2 <- anyNA(c(
          input[[paste0("uni_min_mult_", i)]],
          input[[paste0("uni_max_mult_", i)]]
        ))

        if (issue_uni1 | issue_uni2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_uni1, issue_uni2) == FALSE), ""))

      } else if (input[[paste0("multiple_fam_", i)]] == "Weibull") {

        issue_weibull1 <- any(
          c(
            input[[paste0("weib_shape_mult_", i)]],
            input[[paste0("weib_scale_mult_", i)]]
          ) <= 0
        )

        issue_weibull2 <- anyNA(c(
          input[[paste0("weib_shape_mult_", i)]],
          input[[paste0("weib_scale_mult_", i)]]
        ))

        if (issue_weibull1 | issue_weibull2) {
          shinyalert("Warning!", "At least one parameter value provided is
            not valid.",
            type = "error"
          )
        }

        validate(need(all(c(issue_weibull1, issue_weibull2) == FALSE), ""))
      }


      # x min for density curve
      x_min[i] <- switch(input[[paste0("multiple_fam_", i)]],

        "Beta" = 0,
        "Cauchy" = qcauchy(0.001, input[[paste0("cauchy_loc_mult_", i)]],
          input[[paste0("cauchy_scale_mult_", i)]]
        ),
        "Chi-square" = 0,
        "Exponential" = 0,
        "F" = 0,
        "Gamma" = 0,
        "Laplace" = qlaplace(0.001, input[[paste0("laplace_loc_mult_", i)]],
          input[[paste0("laplace_scale_mult_", i)]]
        ),
        "Log-normal" = qlnorm(0.001, input[[paste0("lognorm_mean_mult_", i)]],
          input[[paste0("lognorm_sd_mult_", i)]]
        ),
        "Normal" = qnorm(0.0001, input[[paste0("norm_mean_mult_", i)]],
          input[[paste0("norm_sd_mult_", i)]]
        ),
        "t" = qt(0.0001, input[[paste0("t_df_mult_", i)]]),
        "Uniform (continuous)" = input[[paste0("uni_min_mult_", i)]],
        "Weibull" = 0
      )


      # x max for density curve
      x_max[i] <- switch(input[[paste0("multiple_fam_", i)]],

        "Beta" = 1,
        "Cauchy" = qcauchy(0.999, input[[paste0("cauchy_loc_mult_", i)]],
          input[[paste0("cauchy_scale_mult_", i)]]
        ),
        "Chi-square" = qchisq(0.999, input[[paste0("chisq_df_mult_", i)]]),
        "Exponential" = qexp(0.999, input[[paste0("exp_rate_mult_", i)]]),
        "F" = qf(0.98, input[[paste0("f_df1_mult_", i)]],
          input[[paste0("f_df2_mult_", i)]]
        ),
        "Gamma" = qgamma(0.999, input[[paste0("gamma_shape_mult_", i)]],
          input[[paste0("gamma_rate_mult_", i)]]
        ),
        "Laplace" = qlaplace(0.999, input[[paste0("laplace_loc_mult_", i)]],
          input[[paste0("laplace_scale_mult_", i)]]
        ),
        "Log-normal" = qlnorm(0.999, input[[paste0("lognorm_mean_mult_", i)]],
          input[[paste0("lognorm_sd_mult_", i)]]
        ),
        "Normal" = qnorm(0.9999, input[[paste0("norm_mean_mult_", i)]],
          input[[paste0("norm_sd_mult_", i)]]
        ),
        "t" = qt(0.9999, input[[paste0("t_df_mult_", i)]]),
        "Uniform (continuous)" = input[[paste0("uni_max_mult_", i)]],
        "Weibull" = qweibull(0.999, input[[paste0("weib_shape_mult_", i)]],
          input[[paste0("weib_scale_mult_", i)]]
        )
      )


      line_color[i] <- input[[paste0("line_color_mult_", i)]]


      legend_lab[i] <- switch(input[[paste0("multiple_fam_", i)]],

        "Beta" = paste0(
          "Beta(",
          input[[paste0("beta_shape1_mult_", i)]],
          ",",
          input[[paste0("beta_shape2_mult_", i)]],
          ")"
        ),

        "Cauchy" = paste0(
          "Cauchy(",
          input[[paste0("cauchy_loc_mult_", i)]],
          ",",
          input[[paste0("cauchy_scale_mult_", i)]],
          ")"
        ),

        "Chi-square" = parse(text = paste0(
          expression("chi["),
          input[[paste0("chisq_df_mult_", i)]],
          expression("]^2")
        )),

        "Exponential" = paste0(
          "Exp(",
          input[[paste0("exp_rate_mult_", i)]],
          ")"
        ),

        "F" = parse(text = paste0(
          expression(italic(F)),
          "(",
          input[[paste0("f_df1_mult_", i)]],
          ",",
          input[[paste0("f_df2_mult_", i)]],
          ")"
        )),

        "Gamma" = paste0(
          "Gamma(",
          input[[paste0("gamma_shape_mult_", i)]],
          ",",
          input[[paste0("gamma_rate_mult_", i)]],
          ")"
        ),

        "Laplace" = paste0(
          "Laplace(",
          input[[paste0("laplace_loc_mult_", i)]],
          ",",
          input[[paste0("laplace_scale_mult_", i)]],
          ")"
        ),

        "Log-normal" = paste0(
          "Lognormal(",
          input[[paste0("lognorm_mean_mult_", i)]],
          ",",
          input[[paste0("lognorm_sd_mult_", i)]],
          ")"
        ),

        "Normal" = paste0(
          "N(",
          input[[paste0("norm_mean_mult_", i)]],
          ",",
          input[[paste0("norm_sd_mult_", i)]],
          ")"
        ),

        "t" = parse(text = paste0(
          expression("italic(t)["),
          input[[paste0("t_df_mult_", i)]],
          expression("]")
        )),

        "Uniform (continuous)" = paste0(
          "U(",
          input[[paste0("uni_min_mult_", i)]],
          ",",
          input[[paste0("uni_max_mult_", i)]],
          ")"
        ),

        "Weibull" = paste0(
          "Weib(",
          input[[paste0("weib_shape_mult_", i)]],
          ",",
          input[[paste0("weib_scale_mult_", i)]],
          ")"
        )
      )


      plot_mult <- plot_mult +
        stat_function(
          fun = dist_fxn,
          n = n,
          args = if (length(params) == 1) {
            list(params)
          } else if (length(params) == 2) {
            list(params[1], params[2])
          },
          lwd = input$curve_lwd_mult,
          aes(colour = !!paste0("dist_", i))    # need !! here
        )
    }



    if (input$x_max_mult != "" & input$x_min_mult != "") {

      x_min_mult <- ifelse(
        input$x_min_mult != "",
        as.numeric(input$x_min_mult),
        NA
      )

      x_max_mult <- ifelse(
        input$x_max_mult != "",
        as.numeric(input$x_max_mult),
        NA
      )


      if (input$x_tick_inc_mult != "") {

        if (!is.na(as.numeric(input$x_tick_inc_mult))) {
          if (as.numeric(input$x_tick_inc_mult) > 0 &
              as.numeric(input$x_max_mult) > as.numeric(input$x_min_mult)
          ) {

            plot_mult <- plot_mult +
              scale_x_continuous(
                oob = oob_keep(),
                breaks = c(
                  x_min_mult,    # force x min to appear w/ repeat
                  seq(
                    x_min_mult,
                    x_max_mult,
                    by = as.numeric(input$x_tick_inc_mult)
                  ),
                  x_max_mult     # force x max to appear w/ repeat
                ),
                limits = c(x_min_mult, x_max_mult),
                expand = c(0, 0)
                #expand = expansion(mult = c(0, 0.04))
              )
          }
        }

      } else if (input$x_tick_inc_mult == "") {

        if (as.numeric(input$x_max_mult) > as.numeric(input$x_min_mult)) {

          plot_mult <- plot_mult +
            scale_x_continuous(
              oob = oob_keep(),
              limits = c(x_min_mult, x_max_mult),
              expand = c(0, 0)
              #expand = expansion(mult = c(0, 0.04))
            )

        } else {

          plot_mult <- plot_mult +
            scale_x_continuous(
              oob = oob_keep(),
              limits = c(x_min_mult, NA),
              expand = c(0, 0)
              #expand = expansion(mult = c(0, 0.04))
            )
        }
      }

    } else if (input$x_max_mult == "" & input$x_min_mult != "") {

      plot_mult <- plot_mult +
        scale_x_continuous(
          oob = oob_keep(),
          limits = c(as.numeric(input$x_min_mult), max(x_max)),
          expand = c(0, 0)
          #expand = expansion(mult = c(0, 0.04))
        )

    } else if (input$x_max_mult != "" & input$x_min_mult == "") {

      plot_mult <- plot_mult +
        scale_x_continuous(
          oob = oob_keep(),
          limits = c(min(x_min), as.numeric(input$x_max_mult)),
          expand = c(0, 0)
          #expand = expansion(mult = c(0, 0.04))
        )

    } else {

      plot_mult <- plot_mult +
        scale_x_continuous(
          oob = oob_keep(),
          limits = c(min(x_min), max(x_max)),
          expand = c(0, 0)
        )
    }



    ### change max y, but only if doesn't cut off top of plot
    max_y_plot_current <- ggplot_build(plot_mult)$layout$panel_params[[1]]$y.range[2]

    if (input$y_max_mult != "") {
      if (!is.na(as.numeric(input$y_max_mult))) {
        if (as.numeric(input$y_max_mult) > max_y_plot_current) {
          max_y <- as.numeric(input$y_max_mult)
        } else {
          max_y <- NA
        }
      }
    } else {
      max_y <- NA
    }


    #max_y <- ifelse(input$y_max_mult != "", as.numeric(input$y_max_mult), NA)


    plot_mult <- plot_mult +
      scale_y_continuous(
        limits = c(0, max_y),
        expand = expansion(mult = c(0, 0.015))
      ) +
      ylab("Density") +
      scale_color_manual(
        name = ifelse(input$legend_lab_mult != "", input$legend_lab_mult,
          "Distribution"
        ),
        values = line_color,
        labels = legend_lab
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = ifelse(
          input$axis_lab_size_mult == "",
          20,
          input$axis_lab_size_mult
        )),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 5),
        axis.text = element_text(
          size = ifelse(
            input$axis_text_size_mult == "",
            16,
            input$axis_text_size_mult
          ),
          color = "black"
        ),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(3, "mm"),
        legend.title = element_text(size = ifelse(
          input$legend_lab_size_mult == "",
          16,
          input$legend_lab_size_mult
        )),
        legend.text = element_text(size = ifelse(
          input$legend_text_size_mult == "",
          13,
          input$legend_text_size_mult
        )),
        legend.text.align = 0,    # left-justify legend text
        plot.margin = unit(c(10, 10, 10, 10), "mm")
      )


    if (!input$keep_x_axis_line_mult) {
      plot_mult <- plot_mult + theme(axis.line.x = element_blank())
    }


    if (!input$keep_x_axis_lab_mult) {
      plot_mult <- plot_mult + theme(axis.title.x = element_blank())
    }


    if (!input$keep_x_axis_text_mult) {
      plot_mult <- plot_mult +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }


    if (!input$keep_y_axis_line_mult) {
      plot_mult <- plot_mult + theme(axis.line.y = element_blank())
    }


    if (!input$keep_y_axis_lab_mult) {
      plot_mult <- plot_mult + theme(axis.title.y = element_blank())
    }


    if (!input$keep_y_axis_text_mult) {
      plot_mult <- plot_mult +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }


    plot_mult

  })




  ### reset distributions UI for multiple curves
  #####################################################
  observeEvent(input$reset_multiple, {

    req(bag$n_curves > 0)

    bag$plot_mult <- NULL
    bag$xlab_status_mult <- 0
    bag$ylab_status_mult <- 0


    # reset previous distributions
    for (i in 1:bag$n_curves) {

      # parameters of pop. distribution for density curve
      if (input[[paste0("multiple_fam_", i)]] == "Beta") {

        updateNumericInput(session, paste0("beta_shape1_mult_", i), value = 5)
        updateNumericInput(session, paste0("beta_shape2_mult_", i), value = 5)

      } else if (input[[paste0("multiple_fam_", i)]] == "Cauchy") {

        updateNumericInput(session, paste0("cauchy_loc_mult_", i), value = 0)
        updateNumericInput(session, paste0("cauchy_scale_mult_", i), value = 1)

      } else if (input[[paste0("multiple_fam_", i)]] == "Chi-square") {

        updateNumericInput(session, paste0("chisq_df_mult_", i), value = 5)

      } else if (input[[paste0("multiple_fam_", i)]] == "Exponential") {

        updateNumericInput(session, paste0("exp_rate_mult_", i), value = 5)

      } else if (input[[paste0("multiple_fam_", i)]] == "F") {

        updateNumericInput(session, paste0("f_df1_mult_", i), value = 5)
        updateNumericInput(session, paste0("f_df2_mult_", i), value = 5)

      } else if (input[[paste0("multiple_fam_", i)]] == "Gamma") {

        updateNumericInput(session, paste0("gamma_shape_mult_", i), value = 5)
        updateNumericInput(session, paste0("gamma_rate_mult_", i), value = 5)

      } else if (input[[paste0("multiple_fam_", i)]] == "Laplace") {

        updateNumericInput(session, paste0("laplace_loc_mult_", i), value = 0)
        updateNumericInput(session, paste0("laplace_scale_mult_", i), value = 1)

      } else if (input[[paste0("multiple_fam_", i)]] == "Log-normal") {

        updateNumericInput(session, paste0("lognorm_mean_mult_", i), value = 0)
        updateNumericInput(session, paste0("lognorm_sd_mult_", i), value = 1)

      } else if (input[[paste0("multiple_fam_", i)]] == "Normal") {

        updateNumericInput(session, paste0("norm_mean_mult_", i), value = 0)
        updateNumericInput(session, paste0("norm_sd_mult_", i), value = 1)

      } else if (input[[paste0("multiple_fam_", i)]] == "t") {

        updateNumericInput(session, paste0("t_df_mult_", i), value = 10)

      } else if (input[[paste0("multiple_fam_", i)]] == "Uniform (continuous)") {

        updateNumericInput(session, paste0("uni_min_mult_", i), value = 0)
        updateNumericInput(session, paste0("uni_max_mult_", i), value = 1)

      } else if (input[[paste0("multiple_fam_", i)]] == "Weibull") {

        updateNumericInput(session, paste0("weib_shape_mult_", i), value = 1)
        updateNumericInput(session, paste0("weib_scale_mult_", i), value = 1)

      }


      updateColourInput(session, paste0("line_color_mult_", i), value = "black")


      updateSelectInput(session, paste0("multiple_fam_", i),
        selected = "Normal"
      )



      # reset all inputs -- need this session code for each
      session$sendCustomMessage(type = "reset_input",
        message = paste0("multiple_fam_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("line_color_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("beta_shape1_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("beta_shape2_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("cauchy_loc_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("cauchy_scale_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("chisq_df_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("exp_rate_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("f_df1_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("f_df2_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("gamma_shape_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("gamma_rate_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("lognorm_mean_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("lognorm_sd_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("norm_mean_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("norm_sd_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("t_df_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("uni_min_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("uni_max_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("weib_shape_mult_", i)
      )

      session$sendCustomMessage(type = "reset_input",
        message = paste0("weib_scale_mult_", i)
      )
    }


    bag$n_curves <- 0
    bag$add_curve_status <- 0
  })



  # update X axis label
  observeEvent(input$x_lab_update_mult, {

    x_lab <- clean_label(input$x_lab_mult)

    bag$plot_mult_xlab <- xlab(parse(text = x_lab))


    bag$xlab_status_mult <- 1
  })



  # update Y axis label
  observeEvent(input$y_lab_update_mult, {

    y_lab <- clean_label(input$y_lab_mult)

    bag$plot_mult_ylab <- ylab(parse(text = y_lab))


    bag$ylab_status_mult <- 1
  })



  # display and store plot
  observe({
    output$plot_mult <- renderPlot({

      plot <- make_init_plot_mult()

      if (bag$xlab_status_mult == 1) plot <- plot + bag$plot_mult_xlab

      if (bag$ylab_status_mult == 1) plot <- plot + bag$plot_mult_ylab


      (bag$plot_mult <- plot)

    }, width = plot_width_mult(), height = 400)
  })




  ### make popup UI for downloading plot -- multiple
  ###########################################################
  output$download_popup_mult <- renderUI({

    req(bag$plot_mult)

    list(
      column(12, align = "center",
        wellPanel(
          h3("Use a transparent background:"),
          br(),

          downloadButton("dl_plot_mult_trans", "Yes",
            class = "btn btn-success", icon = icon("download")
          ),
          downloadButton("dl_plot_mult_notrans", "No",
            class = "btn btn-danger", icon = icon("download")
          )
        )
      )
    )
  })




  ##### download plot w/ transparent background -- multiple
  ###########################################################
  output$dl_plot_mult_trans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  dl_plot <- bag$plot_mult +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA)
        )

		  ggsave(file,
		    dl_plot,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_mult()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)



  ##### download plot w/o transparent background -- multiple
  ###########################################################
  output$dl_plot_mult_notrans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  ggsave(file,
		    bag$plot_mult,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_mult()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)




  ##############################################################################
  ############################## Include Your Data #############################
  ##############################################################################

  ### upload data
  ##################################################
  observeEvent(input$data_file, {

    # determine file extension of uploaded file
    bag$file_extension <- tools::file_ext(input$data_file$datapath)
  })


  output$na_input_status <- reactive({
    req(bag$file_extension)

    bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  })

  # don't suspend value of na_input_status output
  outputOptions(output, "na_input_status", suspendWhenHidden = FALSE)


  output$na_output_status <- reactive({
    req(bag$file_extension)

    bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  })

  # don't suspend value of na_input_status output
  outputOptions(output, "na_output_status", suspendWhenHidden = FALSE)



  # store uploaded dataset and update variable names in selectInput box
  observeEvent(input$upload_dataset, {

    req(input$data_file)


    # read file
    if (bag$file_extension == "csv") {
      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)


      # use read.csv() instead of fread() from data.table package that rio
      #  package uses
      bag$user_data_df <- read.csv(input$data_file$datapath, na.strings = nas,
        header = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else if (bag$file_extension %in% c("xls", "xlsx")) {
      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)

      bag$user_data_df <- import(input$data_file$datapath, na = nas,
        col_names = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else if (bag$file_extension == "txt") {

      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)

      bag$user_data_df <- import(input$data_file$datapath, na.strings = nas,
        header = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else {
      bag$user_data_df <- import(input$data_file$datapath)
    }


    # remove non-numeric variables from uploaded dataset
    bag$user_data_df <- select_if(bag$user_data_df, is.numeric)


    updateSelectInput(session, "select_var", "Variable of Interest",
      choices = colnames(bag$user_data_df)
    )
  })



  ### print data in tabular form
  #################################################

  output$data_table_upload <- renderDT(bag$user_data_df,

    options = list(pageLength = 20, scrollX = TRUE)
  )


  output$table_manual <- renderTable({

    # convert user-input data to numeric vector
    user_data <- input$user_data %>%
      strsplit(split = "\n|,") %>%    # split if see comma or new line
      unlist() %>%
      gsub("[^0-9\\.]", "", .) %>%    # only keep integers and decimals
      as.numeric() %>%
      na.omit()    # remove any NAs

    (bag$user_data_df <- data.frame(Data = user_data))
  })



  ##### user plot
  #######################################################

  observeEvent(input$make_plot_user, {

    ### warning popup if no data set input
    #############################################################
    if (is.null(bag$user_data_df)) {
      shinyalert("Warning!", "You must first input data in the 'Input Data'
        tab.", type = "error"
      )
    }

    validate(need(!is.null(bag$user_data_df), ""))


    # need this to reset plot output if any input changed
    bag$do_plot_user <- input$make_plot_user
  })



  # display user plot
  make_init_plot_user <- reactive({

    req(bag$user_data_df)


    # clear output until make plot button clicked again
    if (bag$do_plot_user != FALSE) {


      plot_user <- ggplot(bag$user_data_df, aes(x = !!sym(input$select_var)))


      # set binwidth; can't use ifelse() here
      if (input$bin_spec == "Bin Width") {

        if (!input$add_density_user) {

          if (input$hist_binwidth_user == "") {

            plot_user <- plot_user +
              geom_histogram(
                bins = 15,
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")

          } else {

            plot_user <- plot_user +
              geom_histogram(
                binwidth = as.numeric(input$hist_binwidth_user),
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")
          }

        } else {

          if (input$hist_binwidth_user == "") {

            plot_user <- plot_user +
              geom_histogram(
                aes(y = stat(density)),
                bins = 15,
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")

          } else {

            plot_user <- plot_user +
              geom_histogram(
                aes(y = stat(density)),
                binwidth = as.numeric(input$hist_binwidth_user),
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")
          }
        }

      } else if (input$bin_spec == "Number of Bins") {

        if (!input$add_density_user) {

          if (input$hist_n_bins_user == "") {

            plot_user <- plot_user +
              geom_histogram(
                bins = 15,
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")

          } else {

            plot_user <- plot_user +
              geom_histogram(
                bins = as.numeric(input$hist_n_bins_user),
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")
          }

        } else {

          if (input$hist_n_bins_user == "") {

            plot_user <- plot_user +
              geom_histogram(
                aes(y = stat(density)),
                bins = 15,
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")

          } else {

            plot_user <- plot_user +
              geom_histogram(
                aes(y = stat(density)),
                bins = as.numeric(input$hist_n_bins_user),
                boundary = 0,
                fill = input$fill_color_user,
                color = "black"
              ) +
              ylab("Frequency")
          }
        }
      }



      x_min <- qnorm(0.001, mean(bag$user_data_df[, input$select_var]),
        sd = sd(bag$user_data_df[, input$select_var])
      )

      x_max <- qnorm(0.999, mean(bag$user_data_df[, input$select_var]),
        sd = sd(bag$user_data_df[, input$select_var])
      )

      n <- 1e4 + 1


      if (input$color_area_user) {

        if (input$area_user == "Left") {

          if (!is.na(input$cutoff_left_user)) {

            plot_area <- stat_function(
              fun = dnorm,
              n = n,
              args = list(
                mean = mean(bag$user_data_df[, input$select_var]),
                sd = sd(bag$user_data_df[, input$select_var])
              ),
              xlim = c(x_min, input$cutoff_left_user),
              geom = "area",
              fill = input$area_color_user,
              alpha = input$area_opac_user
            )


            plot_user <- plot_user + plot_area
          }

        } else if (input$area_user == "Right") {

          if (!is.na(input$cutoff_right_user)) {

            plot_area <- stat_function(
              fun = dnorm,
              n = n,
              args = list(
                mean = mean(bag$user_data_df[, input$select_var]),
                sd = sd(bag$user_data_df[, input$select_var])
              ),
              xlim = c(input$cutoff_right_user, x_max),
              geom = "area",
              fill = input$area_color_user,
              alpha = input$area_opac_user
            )


            plot_user <- plot_user + plot_area
          }

        } else if (input$area_user == "Between") {

          if (!is.na(input$cutoff_between_left_user) &
              !is.na(input$cutoff_between_right_user)
          ) {

            plot_area <- stat_function(
              fun = dnorm,
              n = n,
              args = list(
                mean = mean(bag$user_data_df[, input$select_var]),
                sd = sd(bag$user_data_df[, input$select_var])
              ),
              xlim = c(
                input$cutoff_between_left_user,
                input$cutoff_between_right_user
              ),
              geom = "area",
              fill = input$area_color_user,
              alpha = input$area_opac_user
            )


            plot_user <- plot_user + plot_area

          }

        } else if (input$area_user == "Outside/Tails") {

          if (!is.na(input$cutoff_outside_left_user) &
              !is.na(input$cutoff_outside_right_user)
          ) {

            if (input$cutoff_outside_left_user < input$cutoff_outside_right_user) {

              plot_area_left <- stat_function(
                fun = dnorm,
                n = n,
                args = list(
                  mean = mean(bag$user_data_df[, input$select_var]),
                  sd = sd(bag$user_data_df[, input$select_var])
                ),
                xlim = c(x_min, input$cutoff_outside_left_user),
                geom = "area",
                fill = input$area_color_user,
                alpha = input$area_opac_user
              )

              plot_area_right <- stat_function(
                fun = dnorm,
                n = n,
                args = list(
                  mean = mean(bag$user_data_df[, input$select_var]),
                  sd = sd(bag$user_data_df[, input$select_var])
                ),
                xlim = c(input$cutoff_outside_right_user, x_max),
                geom = "area",
                fill = input$area_color_user,
                alpha = input$area_opac_user
              )

              plot_user <- plot_user +
                plot_area_left +
                plot_area_right
            }
          }
        }
      }



      if (input$add_density_user) {

        plot_user <- plot_user +
          stat_function(
            fun = dnorm,
            n = n,
            args = list(
              mean = mean(bag$user_data_df[, input$select_var]),
              sd = sd(bag$user_data_df[, input$select_var])
            ),
            xlim = c(x_min, x_max),
            lwd = input$curve_lwd_user,
            col = input$curve_color_user
          ) +
          ylab("Density")
      }


      if (input$add_cutoff_user) {

        if (input$area_user == "Left") {

          if (is.numeric(input$cutoff_left_user)) {

            plot_user <- plot_user +
              geom_segment(
                aes(
                  x = input$cutoff_left_user,
                  y = 0,
                  xend = input$cutoff_left_user,
                  yend = dnorm(
                    input$cutoff_left_user,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              )
          }

        } else if (input$area_user == "Right") {

          if (is.numeric(input$cutoff_right_user)) {

            plot_user <- plot_user +
              geom_segment(
                aes(
                  x = input$cutoff_right_user,
                  y = 0,
                  xend = input$cutoff_right_user,
                  yend = dnorm(
                    input$cutoff_right_user,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              )
          }

        } else if (input$area_user == "Between") {

          if (is.numeric(input$cutoff_between_left_user) &
              is.numeric(input$cutoff_between_right_user)
          ) {

            cutoff_left <- input$cutoff_between_left_user
            cutoff_right <- input$cutoff_between_right_user


            plot_user <- plot_user +
              geom_segment(
                aes(
                  x = cutoff_left,
                  y = 0,
                  xend = cutoff_left,
                  yend = dnorm(
                    cutoff_left,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              ) +
              geom_segment(
                aes(
                  x = cutoff_right,
                  y = 0,
                  xend = cutoff_right,
                  yend = dnorm(
                    cutoff_right,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              )
          }

        } else if (input$area_user == "Outside/Tails") {

          if (is.numeric(input$cutoff_outside_left_user) &
              is.numeric(input$cutoff_outside_right_user)
          ) {

            cutoff_left <- input$cutoff_outside_left_user
            cutoff_right <- input$cutoff_outside_right_user


            plot_user <- plot_user +
              geom_segment(
                aes(
                  x = cutoff_left,
                  y = 0,
                  xend = cutoff_left,
                  yend = dnorm(
                    cutoff_left,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              ) +
              geom_segment(
                aes(
                  x = cutoff_right,
                  y = 0,
                  xend = cutoff_right,
                  yend = dnorm(
                    cutoff_right,
                    mean(bag$user_data_df[, input$select_var]),
                    sd(bag$user_data_df[, input$select_var])
                  )
                ),
                color = input$curve_color_user,
                lwd = input$curve_lwd_user
              )
          }
        }
      }



      if (input$x_max_user != "" & input$x_min_user != "") {

        x_min_user <- ifelse(
          input$x_min_user != "",
          as.numeric(input$x_min_user),
          NA
        )

        x_max_user <- ifelse(
          input$x_max_user != "",
          as.numeric(input$x_max_user),
          NA
        )


        if (input$x_tick_inc_user != "") {

          if (!is.na(as.numeric(input$x_tick_inc_user))) {
            if (as.numeric(input$x_tick_inc_user) > 0 &
                as.numeric(input$x_max_user) > as.numeric(input$x_min_user)
            ) {

              plot_user <- plot_user +
                scale_x_continuous(
                  oob = oob_keep(),
                  breaks = c(
                    x_min_user,    # force x min to appear w/ repeat
                    seq(
                      x_min_user,
                      x_max_user,
                      by = as.numeric(input$x_tick_inc_user)
                    ),
                    x_max_user     # force x max to appear w/ repeat
                  ),
                  limits = c(x_min_user, x_max_user),
                  expand = c(0, 0)
                  #expand = expansion(mult = c(0, 0.04))
                )
            }
          }

        } else if (input$x_tick_inc_user == "") {

          if (as.numeric(input$x_max_user) > as.numeric(input$x_min_user)) {

            plot_user <- plot_user +
              scale_x_continuous(
                oob = oob_keep(),
                limits = c(x_min_user, x_max_user),
                expand = c(0, 0)
                #expand = expansion(mult = c(0, 0.04))
              )

          } else {

            plot_user <- plot_user +
              scale_x_continuous(
                oob = oob_keep(),
                limits = c(x_min_user, NA),
                expand = c(0, 0)
                #expand = expansion(mult = c(0, 0.04))
              )
          }
        }

      } else if (input$x_max_user == "" & input$x_min_user != "") {

        plot_user <- plot_user +
          scale_x_continuous(
            oob = oob_keep(),
            limits = c(as.numeric(input$x_min_user), NA),
            expand = c(0, 0)
            #expand = expansion(mult = c(0, 0.04))
          )

      } else if (input$x_max_user != "" & input$x_min_user == "") {

        plot_user <- plot_user +
          scale_x_continuous(
            oob = oob_keep(),
            limits = c(NA, as.numeric(input$x_max_user)),
            expand = c(0, 0)
            #expand = expansion(mult = c(0, 0.04))
          )

      } else {

        plot_user <- plot_user +
          scale_x_continuous(oob = oob_keep(), expand = c(0, 0))
      }



      ### change max y, but only if doesn't cut off top of plot
      max_y_plot_current <- ggplot_build(plot_user)$layout$panel_params[[1]]$y.range[2]

      if (input$y_max_user != "") {
        if (!is.na(as.numeric(input$y_max_user))) {
          if (as.numeric(input$y_max_user) > max_y_plot_current) {
            max_y <- as.numeric(input$y_max_user)
          } else {
            max_y <- NA
          }
        }
      } else {
        max_y <- NA
      }



      plot_user <- plot_user +
        scale_y_continuous(
          limits = c(0, max_y),
          expand = expansion(mult = c(0, 0.005))
        ) +
        theme_classic() +
        theme(
          axis.title = element_text(size = ifelse(
            input$axis_lab_size_user == "",
            20,
            input$axis_lab_size_user
          )),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 5),
          axis.text = element_text(
            size = ifelse(
              input$axis_text_size_user == "",
              16,
              input$axis_text_size_user
            ),
            color = "black"
          ),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(3, "mm"),
          plot.margin = unit(c(10, 10, 10, 10), "mm")
        )


      if (!input$keep_x_axis_line_user) {
        plot_user <- plot_user + theme(axis.line.x = element_blank())
      }


      if (!input$keep_x_axis_lab_user) {
        plot_user <- plot_user + theme(axis.title.x = element_blank())
      }


      if (!input$keep_x_axis_text_user) {
        plot_user <- plot_user +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }


      if (!input$keep_y_axis_line_user) {
        plot_user <- plot_user + theme(axis.line.y = element_blank())
      }


      if (!input$keep_y_axis_lab_user) {
        plot_user <- plot_user + theme(axis.title.y = element_blank())
      }


      if (!input$keep_y_axis_text_user) {
        plot_user <- plot_user +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }


      plot_user
    }
  })




  # update X axis label
  observeEvent(input$x_lab_update_user, {

    x_lab <- clean_label(input$x_lab_user)

    bag$plot_user_xlab <- xlab(parse(text = x_lab))


    bag$xlab_status_user <- 1
  })



  # update Y axis label
  observeEvent(input$y_lab_update_user, {

    y_lab <- clean_label(input$y_lab_user)

    bag$plot_user_ylab <- ylab(parse(text = y_lab))


    bag$ylab_status_user <- 1
  })



  # display and store plot
  observe({
    output$plot_user <- renderPlot({

      plot <- make_init_plot_user()

      if (bag$xlab_status_user == 1) plot <- plot + bag$plot_user_xlab

      if (bag$ylab_status_user == 1) plot <- plot + bag$plot_user_ylab


      (bag$plot_user <- plot)

    }, width = plot_width_user(), height = 400)
  })




  ### reset plot if certain inputs changed
  ################################################
  reset_plot_user <- reactive({

    list(input$upload_dataset, input$user_data, input$select_var)
  })


  observeEvent(reset_plot_user(), {

    bag$do_plot_user <- FALSE
    bag$xlab_status_user <- 0
    bag$ylab_status_user <- 0
  })




  ### make popup UI for downloading plot -- user
  ###########################################################
  output$download_popup_user <- renderUI({

    req(bag$plot_user)

    list(
      column(12, align = "center",
        wellPanel(
          h3("Use a transparent background:"),
          br(),

          downloadButton("dl_plot_user_trans", "Yes",
            class = "btn btn-success", icon = icon("download")
          ),
          downloadButton("dl_plot_user_notrans", "No",
            class = "btn btn-danger", icon = icon("download")
          )
        )
      )
    )
  })




  ##### download plot w/ transparent background -- user
  ###########################################################
  output$dl_plot_user_trans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  dl_plot <- bag$plot_user +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA)
        )

		  ggsave(file,
		    dl_plot,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_user()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)



  ##### download plot w/o transparent background -- user
  ###########################################################
  output$dl_plot_user_notrans <- downloadHandler(

    filename = function() "plot.png",

		content = function(file) {

		  ggsave(file,
		    bag$plot_user,
		    device = "png",
		    height = 400*300/72,    # increase resolution
		    width = plot_width_user()*300/72,    # increase resolution
		    units = "px"
		  )
		}
	)




  ##############################################################################
  ################################### Gallery ##################################
  ##############################################################################

  output$gallery_shape_ui <- renderUI({
    list(
      img(src = "Unimodal_Normal.png", height = "225px"),
      img(src = "Bimodal.png", height = "225px"),
      img(src = "Skewed_left.png", height = "225px")
    )
  })


  output$gallery_centers_ui <- renderUI({
    list(
      img(src = "Mean_Median_Symmetric.png", height = "225px"),
      img(src = "Mean_Median_Skewed_Right.png", height = "225px"),
      img(src = "Mean_Median_Skewed_Left.png", height = "225px")
    )
  })


  output$gallery_empirical_rule_ui <- renderUI({
    img(src = "Empirical_Rule.png", height = "225px")
  })


  output$gallery_binom_ui <- renderUI({
    list(
      img(src = "Binomial_PMF.png", height = "225px"),
      img(src = "Binomial_CDF.png", height = "225px")
    )
  })


  output$gallery_normal_areas_ui <- renderUI({
    list(
      img(src = "Normal_Left.png", height = "225px"),
      img(src = "Normal_Right.png", height = "225px"),
      img(src = "Normal_Between.png", height = "225px")
    )
  })


  output$gallery_conf_level_ui <- renderUI({
    img(src = "Confidence_Level.png", height = "225px")
  })


  output$gallery_p_value_ui <- renderUI({
    img(src = "Pvalue.png", height = "225px")
  })


  output$gallery_normal_densities_ui <- renderUI({
    img(src = "Normals.png", height = "225px")
  })


  output$gallery_normal_and_t_ui <- renderUI({
    img(src = "Normal_and_t.png", height = "225px")
  })


  output$gallery_chi_squareds_ui <- renderUI({
    img(src = "Chi_Squareds.png", height = "225px")
  })


  output$gallery_betas_ui <- renderUI({
    img(src = "Betas.png", height = "225px")
  })


  output$gallery_normal_approx_density_ui <- renderUI({
    img(src = "Normal_Approx_Density.png", height = "225px")
  })


  output$gallery_normal_approx_area_ui <- renderUI({
    img(src = "Normal_Approx_Area.png", height = "225px")
  })

})



