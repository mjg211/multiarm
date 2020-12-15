#' Analytically determine the operating characteristics of a multi-stage
#' drop-the-losers multi-arm clinical trial for a Poisson distributed primary
#' outcome
#'
#' \code{opchar_dtl_pois()} determines the operating characteristics of a
#' specified multi-stage drop-the-losers multi-arm clinical trial design
#' assuming the primary outcome variable is Poisson distributed, for given
#' values of the true treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_dtl_pois"}, as
#' returned by \code{\link{build_dtl_pois}} or \code{\link{des_dtl_pois}} (i.e., a
#' multi-stage drop-the-losers multi-arm clinical trial design for a Poisson
#' distributed outcome). Defaults to \code{des_dtl_pois()}.
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
#' \item A \code{\link{tibble}} in the slot \code{$pmf_N} giving the determined
#' probability mass functions of the random required sample size.
#' \item Each of the input variables.
#' }
#' @examples
#' # The operating characteristics for the default parameters
#' opchar        <- opchar_dtl_pois()
#' @seealso \code{\link{build_dtl_pois}}, \code{\link{des_dtl_pois}},
#' \code{\link{plot.multiarm_des_dtl_pois}}, \code{\link{sim_dtl_pois}}.
#' @export
opchar_dtl_pois <- function(des = des_dtl_pois(), lambda, summary = FALSE) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_pois(des)
  lambda <- check_lambda(lambda, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_dtl(des, lambda, "pois")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_dtl_init(des$alpha, des$beta, des$delta0, des$delta1,
                              des$integer, des$Kv, des$power, des$ratio,
                              des$summary, des$type, lambda0 = des$lambda0,
                              n_factor = des$n_factor, e = des$e)
  comp <- components_dtl_update(comp, lambda = lambda)
  comp <- opchar_dtl_internal(comp)
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
