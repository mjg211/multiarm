#' Analytically determine the operating characteristics of a fixed-sample
#' multi-arm clinical trial for a normally distributed primary outcome
#'
#' \code{opchar_ma()} determines the operating characteristics of a specified
#' fixed-sample multi-arm clinical trial design assuming the primary outcome
#' variable is normally distributed, for given values of the true treatment
#' effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma"}, as
#' returned by \code{\link{build_ma}}, \code{\link{des_ma}}, or
#' \code{\link{des_int_ma}} (i.e., a fixed-sample multi-arm clinical trial
#' design for a normally distributed outcome). Defaults to \code{des_ma()}.
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
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The operating characteristics for the default parameters
#' opchar        <- opchar_ma()
#' # An A-optimal design
#' des_A         <- des_ma(ratio = "A")
#' opchar_A      <- opchar_ma(des_A)
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and specifying tau explicitly
#' des_root_K    <- des_ma(ratio      = rep(1/sqrt(2), 2),
#'                         correction = "holm_bonferroni",
#'                         power      = "disjunctive")
#' opchar_root_K <- opchar_ma(des_root_K, rbind(c(0, 0),
#'                                              c(0.5, 0.5),
#'                                              c(0.5, 0),
#'                                              c(0, 0.5)))
#' @seealso  \code{\link{an_ma}}, \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{plot.multiarm_des_ma}},
#' \code{\link{sim_ma}}.
#' @export
opchar_ma <- function(des = des_ma(), tau, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_opchar_ma(des, tau, "normal")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations", uc("two_elip"))
  }
  if (des$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
    opchar <- opchar_ma_single(tau, des$K, des$sigma, des$n, des$CovZ,
                               stats::qnorm(1 - des$gamma))
  } else {
    opchar <- opchar_ma_step(tau, des$K, des$sigma, des$correction, des$n,
                             des$CovZ, stats::qnorm(1 - des$gammaO))
  }
  if (summary) {
    message(uc("two_elip"), "completed the required calculations.")
    message("  Preparing for outputting", uc("two_elip"))
  }

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  list(des     = des,
       opchar  = opchar,
       summary = summary,
       tau     = tau)

}
