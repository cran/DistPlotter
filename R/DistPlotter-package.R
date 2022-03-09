#' DistPlotter: A Graphical User Interface for Plotting Common Univariate
#'   Distributions
#'
#' @description The DistPlotter package enables users to plot common univariate
#'   distributions and was designed with a particular focus on education. Users
#'   can plot distributions based on general shape (e.g., symmetric vs. skewed
#'   right) and distributions from common discrete and continuous families. They
#'   can also shade areas underneath the curve (e.g., areas corresponding to
#'   general probabilities, p-values, or confidence levels). Users can
#'   additionally plot their own quantitative data, as well as overlay a normal
#'   density curve and shade area(s) underneath.
#'
#' @aliases DistPlotter-package DistPlotter
#'
#' @import ggplot2
#' @import shiny
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom dplyr filter select_if sym
#' @importFrom DT DTOutput renderDT
#' @importFrom extraDistr dnsbeta qlaplace
#' @importFrom rio import
#' @importFrom scales oob_keep
#' @importFrom shinyalert shinyalert useShinyalert
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets materialSwitch
#' @importFrom stats dnorm na.omit qcauchy qchisq qexp qf qgamma qgeom qhyper
#'   qlnorm qnbinom qnorm qpois qt qweibull sd
#' @importFrom stringi stri_dup stri_sub
#' @importFrom stringr str_count str_trim
#' @importFrom utils read.csv
#'
#' @section Function: \itemize{
#' \item \code{\link{runDistPlotterApp}}
#' }
#'
#' @details Package: DistPlotter \cr
#' Type: Package \cr
#' Version: 0.0.1 \cr
#' Date: 2022-02-21 \cr
#' Depends: R (>= 3.5.0) \cr
#' Imports: colourpicker, dplyr, DT, extraDistr, ggplot2, rio, scales, shiny,
#' shinyalert, shinyalert, shinyBS, shinyjs, shinyWidgets, stringi, stringr
#' License: MIT \cr
#' BugReports: https://github.com/ccasement/DistPlotter/issues \cr
#' Encoding: UTF-8 \cr
#'
#' @author
#' Christopher Casement \cr
#' Department of Mathematics \cr
#' Fairfield University \cr
#' \email{casementc@@gmail.com}
#'
#' Grant Innerst \cr
#' Department of Mathematics \cr
#' Shippensburg University
#'
#' Melissa Innerst \cr
#' Department of Mathematics \cr
#' Juniata College
#'
#' @docType package
"_PACKAGE"
