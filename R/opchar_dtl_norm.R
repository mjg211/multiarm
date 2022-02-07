#' Analytically determine the operating characteristics of a multi-stage
#' drop-the-losers multi-arm clinical trial for a normally distributed primary
#' outcome
#'
#' \code{opchar_dtl_norm()} determines the operating characteristics of a
#' specified multi-stage drop-the-losers multi-arm clinical trial design
#' assuming the primary outcome variable is normally distributed, for given
#' values of the true treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_dtl_norm"}, as
#' returned by \code{\link{build_dtl_norm}} or \code{\link{des_dtl_norm}} (i.e., a
#' multi-stage drop-the-losers multi-arm clinical trial design for a normally
#' distributed outcome). Defaults to \code{des_dtl_norm()}.
#' @param tau A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}} at which to
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
#' opchar        <- opchar_dtl_norm()
#' @seealso \code{\link{build_dtl_norm}}, \code{\link{des_dtl_norm}},
#' \code{\link{plot.multiarm_des_dtl_norm}}, \code{\link{sim_dtl_norm}}.
#' @export
opchar_dtl_norm <- function(des = des_dtl_norm(), tau, summary = FALSE) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_norm(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_dtl(des, tau, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_dtl_init(des$alpha, des$beta, des$delta0, des$delta1,
                              des$integer, des$Kv, des$power, des$ratio,
                              des$spacing, des$summary, des$type, des$sigma,
                              n_factor = des$n_factor, e = des$e)
  comp <- components_dtl_update(comp, tau)
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
       opchar  = comp$opchar,
       summary = summary,
       tau     = tau)

}
