#' Analytically determine the operating characteristics of a single-stage
#' multi-arm clinical trial for a Bernoulli distributed primary outcome
#'
#' \code{opchar_ss_bern()} determines the operating characteristics of a
#' specified single-stage multi-arm clinical trial design assuming the primary
#' outcome variable is Bernoulli distributed, for given values of the true
#' treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ss_bern"}, as
#' returned by \code{\link{des_ss_bern}} (i.e., a single-stage multi-arm
#' clinical trial design for a Poisson distributed outcome). Defaults to
#' \code{des_ss_bern()}.
#' @param pi A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations,
#' for the specified design \code{des}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} giving the determined
#' operating characteristics.
#' \item Each of the input variables.
#' }
#' @examples
#' # The operating characteristics for the default parameters
#' opchar        <- opchar_ss_bern()
#' # An A-optimal design
#' des_A         <- des_ss_bern(ratio = "A")
#' opchar_A      <- opchar_ss_bern(des_A)
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and specifying pi explicitly
#' des_root_K    <- des_ss_bern(ratio      = rep(1/sqrt(2), 2),
#'                              correction = "holm_bonferroni",
#'                              power      = "disjunctive")
#' opchar_root_K <- opchar_ss_bern(des_root_K, rbind(c(0.3, 0.3, 0.3),
#'                                                   c(0.3, 0.5, 0.5),
#'                                                   c(0.3, 0.5, 0.3),
#'                                                   c(0.3, 0.3, 0.5)))
#' @seealso \code{\link{build_ss_bern}}, \code{\link{des_ss_bern}},
#' \code{\link{plot.multiarm_des_ss_bern}}, \code{\link{sim_ss_bern}}.
#' @export
opchar_ss_bern <- function(des = des_ss_bern(), pi, summary = FALSE) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_bern(des)
  pi <- check_pi(pi, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_opchar_ss(des, pi, "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp          <- components_ss_bern(des$alpha, des$beta, des$correction,
                                      des$delta0, des$delta1, des$integer,
                                      des$K, des$pi0, des$power, des$ratio,
                                      des$ratio_scenario, des$n)
  comp$pi       <- pi
  comp$tau      <- pi[, -1] - des$pi0
  comp$nrow_tau <- comp$nrow_pi <- nrow(pi)
  comp          <- components_ss_update(comp)
  comp          <- opchar_ss_internal(comp)
  if (summary) {
    message("..completed the required calculations.")
    message("  Preparing for outputting..")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  list(des     = des,
       opchar  = comp$opchar,
       pi      = pi,
       summary = summary)

}
