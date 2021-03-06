#' multiarm: Design of single- and multi-stage multi-arm clinical trials
#'
#' \strong{multiarm} provides functions to assist with the design of
#' single- and multi-stage multi-arm clinical trials. In both cases the
#' available functions allow for sample size determination, trial simulation,
#' analytical operating characteristic calculation, and the production of
#' several informative plots. An R Shiny graphical user interface is also
#' provided to aid design determination. Further details on single-stage design
#' can be found in Grayling and Wason (2020).
#'
#' @section Getting started:
#'
#' You can install the latest development version of \strong{multiarm} from
#' \href{https://github.com/}{Github} with:
#'
#' \code{devtools::install_github("mjg211/multiarm")}
#'
#' An introductory example of how to make use of the package's core
#' functionality can be found \href{https://github.com/mjg211/multiarm}{here}.
#' More detailed support is available in the package vignette, which can be
#' accessed with \code{vignette("multiarm")}. For further help, please email
#' \email{michael.grayling@@newcastle.ac.uk}.
#'
#' @section Details:
#'
#' In total, 46 functions are currently available. Their naming conventions are
#' such that several character strings are joined together, separated by
#' underscores. The first string indicates the purpose of the function (i.e.,
#' what type of calculation it performs):
#'
#' \itemize{
#' \item \code{build_###_###()}: Build multi-arm clinical trial design objects,
#' like those returned by the \code{des_###_###()} functions. For use when a
#' specific design is of interest.
#' \item \code{des_###_###()}: Determine the sample size required by a
#' particular type of multi-arm clinical trial design.
#' \item \code{gui()}: Provides a graphical user interface to design
#' determination.
#' \item \code{opchar_###_###()}: Determine the operating characteristics
#' (power, family-wise error-rates, etc.) of a supplied multi-arm clinical trial
#' design, via multivariate normal integration.
#' \item \code{plot.multiarm_des_###_###()}: Produce informative plots (power,
#' false discovery rate curves, etc.) relating to a supplied multi-arm clinical
#' trial design.
#' \item \code{sim_###_###()}: Empirically estimate the operating
#' characteristics (power, family-wise error-rates, etc.) of a supplied
#' multi-arm clinical trial design, via simulation.
#' }
#'
#' The second indicates the design:
#'
#' \itemize{
#' \item \code{###_dtl_###()}: Relate to multi-stage drop-the-losers designs.
#' See, e.g.,
#' \href{https://doi.org/10.1177/0962280214550759}{Wason \emph{et al} (2017)}.
#' \item \code{###_gs_###()}: Relate to group-sequential multi-arm multi-stage
#' designs. See, e.g.,
#' \href{https://doi.org/10.1093/biomet/ass002}{Magirr \emph{et al} (2012)}.
#' \item \code{###_ss_###()}: Relate to single-stage designs. See, e.g.,
#' \href{https://doi.org/10.1186/s12885-020-6525-0}{Grayling and Wason (2020)}.
#' }
#'
#' The third indicates what type of outcome the function is for:
#'
#' \itemize{
#' \item \code{###_###_bern()}: Assume a Bernoulli distributed primary outcome.
#' \item \code{###_###_norm()}: Assume a normally distributed primary outcome.
#' \item \code{###_###_pois()}: Assume a Poisson distributed primary outcome.
#' }
#'
#' @section References:
#' Grayling MJ, Wason JMS (2020) A web application for the design of multi-arm
#' clinical trials. \emph{BMC Cancer} \strong{20:}80. DOI:
#' \href{https://doi.org/10.1186/s12885-020-6525-0}{10.1186/s12885-020-6525-0}.
#' PMID: \href{https://www.ncbi.nlm.nih.gov/pubmed/32005187}{32005187}.
#'
#' Magirr D, Jaki T, Whitehead J (2012) A generalized Dunnett test for multi-arm
#' multi-stage clinical studies with treatment selection. \emph{Biometrika}
#' \strong{99}(2)\strong{:}494--501.
#' DOI: \href{https://doi.org/10.1093/biomet/ass002}{10.1093/biomet/ass002}.
#'
#' Wason J, Stallard N, Bowden J, Jennison C (2017) A multi-stage
#' drop-the-losers design for multi-arm clinical trials.
#' \emph{Stat Meth Med Res} \strong{26}(1)\strong{:}508--524. DOI:
#' \href{https://doi.org/10.1177/0962280214550759}{10.1177/0962280214550759}.
#' PMID: \href{https://www.ncbi.nlm.nih.gov/pubmed/25228636}{25228636}.
#'
#' @docType package
#' @name multiarm
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}
