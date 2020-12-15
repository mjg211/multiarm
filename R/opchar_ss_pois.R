#' Analytically determine the operating characteristics of a single-stage
#' multi-arm clinical trial for a Poisson distributed primary outcome
#'
#' \code{opchar_ss_pois()} determines the operating characteristics of a
#' specified single-stage multi-arm clinical trial design assuming the primary
#' outcome variable is Poisson distributed, for given values of the true
#' treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ss_pois"}, as
#' returned by \code{\link{des_ss_pois}} (i.e., a single-stage multi-arm
#' clinical trial design for a Poisson distributed outcome). Defaults to
#' \code{des_ss_pois()}.
#' @param lambda A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&lambda;</i></b>}}{\eqn{\bold{\lambda}}} at which to
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
#' opchar        <- opchar_ss_pois()
#' # An A-optimal design
#' des_A         <- des_ss_pois(ratio = "A")
#' opchar_A      <- opchar_ss_pois(des_A)
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and specifying lambda explicitly
#' des_root_K    <- des_ss_pois(ratio      = rep(1/sqrt(2), 2),
#'                              correction = "holm_bonferroni",
#'                              power      = "disjunctive")
#' opchar_root_K <- opchar_ss_pois(des_root_K, rbind(c(0.3, 0.3, 0.3),
#'                                                   c(0.3, 0.5, 0.5),
#'                                                   c(0.3, 0.5, 0.3),
#'                                                   c(0.3, 0.3, 0.5)))
#' @seealso \code{\link{build_ss_pois}}, \code{\link{des_ss_pois}},
#' \code{\link{plot.multiarm_des_ss_pois}}, \code{\link{sim_ss_pois}}.
#' @export
opchar_ss_pois <- function(des = des_ss_pois(), lambda, summary = FALSE) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_pois(des)
  lambda <- check_lambda(lambda, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_opchar_ss(des, lambda, "pois")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp          <- components_ss_pois(des$alpha, des$beta, des$correction,
                                      des$delta0, des$delta1, des$integer,
                                      des$K, des$lambda0, des$power, des$ratio,
                                      des$ratio_scenario, des$n)
  comp$lambda   <- lambda
  comp$tau      <- lambda[, -1] - des$lambda0
  comp$nrow_tau <- comp$nrow_lambda <- nrow(lambda)
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
       lambda  = lambda,
       opchar  = comp$opchar,
       summary = summary)

}
