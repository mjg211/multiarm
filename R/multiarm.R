#' multiarm: Design and analysis of fixed-sample multi-arm clinical trials
#'
#' \strong{multiarm} provides functionality to assist with the design and
#' analysis of fixed-sample multi-arm clinical trials utilising one of several
#' supported multiple comparison corrections. Available functions allow for
#' sample size determination (including for \emph{A}-, \emph{D}-, and \emph{E}-
#' optimal designs), trial simulation, analytical operating characteristic
#' calculation (including the conjunctive power, disjunctive power, family-wise
#' error rate, and false discovery rate), and the production of several
#' informative plots. An R Shiny graphical user interface is also provided to
#' ease design determination.
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
#' Eight key functions are provided: \code{\link{an_ma}},
#' \code{\link{build_ma}}, \code{\link{des_int_ma}}, \code{\link{des_ma}},
#' \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, and \code{\link{sim_ma}}. Outputs from
#' all but \code{\link{gui_ma}} and \code{\link{plot.multiarm_des_ma}} are also
#' supported by S3 \code{\link{print}} and \code{\link{summary}} generics.
#'
#' \itemize{
#' \item \code{\link{an_ma}}: Analyses summary statistics from a supplied
#' fixed-sample multi-arm clinical trial design, in order to determine which
#' null hypotheses to reject.
#' \item \code{\link{build_ma}}: Builds a fixed-sample multi-arm clinical trial
#' design object, like those returned by \code{\link{des_ma}} and
#' \code{\link{des_int_ma}}. For use when a specific design is of interest.
#' \item \code{\link{des_int_ma}}: Determines the \emph{A}-, \emph{D}-, and
#' \emph{E}-optimal allocation of a set of patients in a fixed-sample multi-arm
#' clinical trial.
#' \item \code{\link{des_ma}}: Determines the sample size required a
#' fixed-sample multi-arm clinical trial when one of a variety of multiple
#' comparison procedures is used.
#' \item \code{\link{gui_ma}}: Provides a graphical user interface to design
#' determination.
#' \item \code{\link{opchar_ma}}: Analytically determines the operating
#' characteristics (power, family-wise error rates, etc.) of a supplied
#' fixed-sample multi-arm clinical trial design.
#' \item \code{\link{plot.multiarm_des_ma}}: Produces informative plots (power,
#' false discovery rate curves, etc.) relating to a supplied fixed-sample
#' multi-arm clinical trial design.
#' \item \code{\link{sim_ma}}: Empirically estimates the operating
#' characteristics (power, family-wise error rates, etc.) of a supplied
#' fixed-sample multi-arm clinical trial design, via simulation.
#' }
#'
#' @docType package
#' @name multiarm
NULL
