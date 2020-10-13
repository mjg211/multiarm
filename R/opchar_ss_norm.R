#' Analytically determine the operating characteristics of a single-stage
#' multi-arm clinical trial for a normally distributed primary outcome
#'
#' \code{opchar_ss_norm()} determines the operating characteristics of a
#' specified single-stage multi-arm clinical trial design assuming the primary
#' outcome variable is normally distributed, for given values of the true
#' treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ss_norm"}, as
#' returned by \code{\link{build_ss_norm}} or \code{\link{des_ss_norm}} (i.e., a
#' single-stage multi-arm clinical trial design for a normally distributed
#' outcome). Defaults to \code{des_ss_norm()}.
#' @param tau A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations,
#' for the specified design \code{des}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} giving the determined
#' operating characteristics.
#' \item Each of the input variables.
#' }
#' @examples
#' # The operating characteristics for the default parameters
#' opchar        <- opchar_ss_norm()
#' # An A-optimal design
#' des_A         <- des_ss_norm(ratio = "A")
#' opchar_A      <- opchar_ss_norm(des_A)
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and specifying tau explicitly
#' des_root_K    <- des_ss_norm(ratio      = rep(1/sqrt(2), 2),
#'                              correction = "holm_bonferroni",
#'                              power      = "disjunctive")
#' opchar_root_K <- opchar_ss_norm(des_root_K, rbind(c(0, 0),
#'                                                   c(0.5, 0.5),
#'                                                   c(0.5, 0),
#'                                                   c(0, 0.5)))
#' @seealso \code{\link{build_ss_norm}}, \code{\link{des_ss_norm}},
#' \code{\link{plot.multiarm_des_ss_norm}}, \code{\link{sim_ss_norm}}.
#' @export
opchar_ss_norm <- function(des = des_ss_norm(), tau, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_norm(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_opchar_ss(des, tau, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp          <- components_ss_norm(des$alpha, des$beta, des$correction,
                                      des$delta0, des$delta1, des$integer,
                                      des$K, des$power, des$ratio, des$sigma,
                                      des$n)
  comp$tau      <- tau
  comp$nrow_tau <- nrow(tau)
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
       summary = summary,
       tau     = tau)

}
