#' Analytically determine the operating characteristics of a fixed-sample
#' multi-arm clinical trial for a Bernoulli distributed primary outcome
#'
#' \code{opchar_ma_bern()} determines the operating characteristics of a
#' specified fixed-sample multi-arm clinical trial design assuming the primary
#' outcome variable is Bernoulli distributed, for given values of the true
#' treatment effects, using multivariate normal integration.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}, as
#' returned by \code{\link{des_ma_bern}} (i.e., a fixed-sample multi-arm clinical
#' trial design for a normally distributed outcome). Defaults to
#' \code{des_ma_bern()}.
#' @param pi A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}} at which to
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
#' opchar        <- opchar_ma_bern()
#' # An A-optimal design
#' des_A         <- des_ma_bern(ratio = "A")
#' opchar_A      <- opchar_ma_bern(des_A)
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and specifying p explicitly
#' des_root_K    <- des_ma_bern(ratio      = rep(1/sqrt(2), 2),
#'                              correction = "holm_bonferroni",
#'                              power      = "disjunctive")
#' opchar_root_K <- opchar_ma_bern(des_root_K, rbind(c(0.3, 0.3, 0.3),
#'                                                   c(0.3, 0.5, 0.5),
#'                                                   c(0.3, 0.5, 0.3),
#'                                                   c(0.3, 0.3, 0.5)))
#' @seealso \code{\link{an_ma_bern}}, \code{\link{build_ma_bern}},
#' \code{\link{des_ma_bern}},\code{\link{gui}},
#' \code{\link{plot.multiarm_des_ma_bern}}, \code{\link{sim_ma_bern}}.
#' @export
opchar_ma_bern <- function(des = des_ma_bern(), pi, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma_bern(des)
  pi <- check_pi(pi, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_opchar_ma(des, pi, "bernoulli")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations", uc("two_elip"))
  }
  if (des$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
    opchar <- opchar_ma_single_bern(pi, des$K, des$alpha, des$correction, des$n,
                                    stats::qnorm(1 - des$gamma))

  } else {
    opchar <- opchar_ma_step_bern(pi, des$K, des$alpha, des$correction, des$n,
                                  stats::qnorm(1 - des$gammaO))

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
       pi      = pi,
       summary = summary)

}
