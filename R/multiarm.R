#' multiarm: Design and analysis of multi-arm clinical trials
#'
#' \strong{multiarm} provides functions to assist with the design and analysis
#' of multi-arm clinical trials. Available functions allow for sample size
#' determination when utilising a variety of multiple comparison corrections
#' (including for \emph{A}-, \emph{D}-, and \emph{E}-optimal designs), trial
#' simulation, analytical operating characteristic calculation (including the
#' conjunctive power, disjunctive power, family-wise error-rate, and false
#' discovery rate), and the production of several plots. An R Shiny graphical
#' user interface is also provided to aid design determination.
#'
#' @section Getting started:
#'
#' You can install the latest development version of multiarm from
#' \href{https://github.com/}{Github} with:
#'
#' \code{devtools::install_github("mjg211/multiarm")}
#'
#' An introductory example of how to make use of the package's core
#' functionality can be found \href{https://github.com/mjg211/multiarm}{here}.
#' More detailed support is available in the package vignette, which can be
#' accessed with \code{vignette("multiarm")}. For further help, please contact
#' Michael Grayling at \email{michael.grayling@@newcastle.ac.uk}.
#'
#' @section Details:
#' Currently, functions are provided to support trials in which the primary
#' outcome variable is assumed to be either normally or Bernoulli distributed.
#' In total, 14 functions are available. Their naming conventions are such that
#' several character strings are joined together separated by underscores. The
#' first character string indicates the type of calculation the function
#' performs (e.g., design determination, operating characteristic calculations),
#' and the remainder which type of design it is for (normally or Bernoulli
#' distributed outcomes):
#'
#' \itemize{
#' \item \code{\link{an_ma}} and \code{\link{an_ma_bern}}: Analyse summary
#' statistics from fixed-sample multi-arm clinical trial designs, in order to
#' determine which null hypotheses to reject.
#' \item \code{\link{build_ma}} and \code{\link{build_ma_bern}}: Build multi-arm
#' clinical trial design objects, like those returned by \code{\link{des_ma}}
#' and \code{\link{des_ma_bern}}. For use when a specific design is of interest.
#' \item \code{\link{des_int_ma}}: Determines the \emph{A}-, \emph{D}-, and
#' \emph{E}-optimal allocation of a set of patients in a multi-arm clinical
#' trial with normally distributed outcomes.
#' \item \code{\link{des_ma}} and \code{\link{des_ma_bern}}: Determine the
#' sample size required by a multi-arm clinical trial when one of several
#' multiple comparison corrections is used.
#' \item \code{\link{gui}}: Provides a graphical user interface to design
#' determination.
#' \item \code{\link{opchar_ma}} and \code{\link{opchar_ma_bern}}: Determine the
#' operating characteristics (power, family-wise error rates, etc.) of supplied
#' fixed-sample multi-arm clinical trial designs using multivariate normal
#' integration.
#' \item \code{\link{plot.multiarm_des_ma}} and
#' \code{\link{plot.multiarm_des_ma_bern}}: Produce informative plots (power,
#' false discovery rate curves, etc.) relating to supplied fixed-sample
#' multi-arm clinical trial designs.
#' \item \code{\link{sim_ma}} and \code{\link{sim_ma_bern}}: Empirically
#' estimate the operating characteristics (power, family-wise error rates, etc.)
#' of supplied fixed-sample multi-arm clinical trial designs, via simulation.
#' }
#'
#' @docType package
#' @name multiarm
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}
