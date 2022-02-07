#' Analytically determine the operating characteristics of a multi-stage
#' group-sequential multi-arm clinical trial for a normally distributed primary
#' outcome
#'
#' \code{opchar_gs_norm()} determines the operating characteristics of a
#' specified multi-stage group-sequential multi-arm clinical trial design
#' assuming the primary outcome variable is normally distributed, for given
#' values of the true treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_gs_norm"}, as
#' returned by \code{\link{build_gs_norm}} or \code{\link{des_gs_norm}} (i.e., a
#' multi-stage group-sequential multi-arm clinical trial design for a normally
#' distributed outcome). Defaults to \code{des_gs_norm()}.
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
#' opchar        <- opchar_gs_norm()
#' @seealso \code{\link{build_gs_norm}}, \code{\link{des_gs_norm}},
#' \code{\link{plot.multiarm_des_gs_norm}}, \code{\link{sim_gs_norm}}.
#' @export
opchar_gs_norm <- function(des = des_gs_norm(), tau, summary = FALSE) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_norm(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_gs_norm(des, tau, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_gs_init(des$alpha, des$beta, des$delta0, des$delta1,
                             des$efix, des$eshape, des$ffix, des$fshape,
                             des$integer, des$J, des$K, des$power, des$ratio,
                             "identity", des$spacing, des$stopping, summary,
                             des$type, des$sigma, n_factor = des$n_factor,
                             f = des$f, e = des$e)
                             des$spacing, des$stopping, summary, des$type,
                             des$sigma, n_factor = des$n_factor, f = des$f,
                             e = des$e)
  comp <- components_gs_update(comp, tau)
  comp <- opchar_gs_internal(comp)
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
       pmf_N   = comp$pmf_N,
       summary = summary,
       tau     = tau)

}
