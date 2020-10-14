#' Build a multi-stage drop-the-losers multi-arm clinical trial for a Bernoulli
#' distributed primary outcome
#'
#' \code{build_dtl_bern()} builds a multi-stage drop-the-losers multi-arm
#' clinical trial design object assuming the primary outcome variable is
#' Bernoulli distributed, like those returned by \code{\link{des_dtl_bern}}.
#'
#' @param n1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the total sample size
#' required in stage one of the trial. Defaults to \code{147}.
#' @param n10 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>n</i><sub>10</sub>}}{\eqn{n_{10}}}, the sample size
#' required in the control arm in stage one of the trial. Defaults to \code{49}.
#' @param e A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>e</i>}}{\eqn{e}}, the critical rejection boundary for
#' the final analysis. Defaults to \code{2.17}.
#' @param Kv A \code{\link{numeric}} \code{\link{vector}} of strictly decreasing
#' values, indicating the chosen value for
#' \ifelse{html}{\out{<b><i>K</i></b>}}{\eqn{\bold{K}}}, the number of
#' experimental treatment arms present in each stage. Defaults to
#' \code{c(2, 1)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (family-wise error-rate). Defaults to \code{0.025}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to \code{0.1}.
#' @param pi0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&pi;</i><sub>0</sub>}}{\eqn{\pi_0}}, the
#' response rate in the control arm. Defaults to \code{0.3}.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to \code{0.5}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
#' @param ratio A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>r</i>}}{\eqn{r}}, the stage-wise allocation ratio to
#' present experimental arms. Defaults to \code{1}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be \code{"disjunctive"} or
#' \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param type A \code{\link{character}} string indicating the choice for the
#' stage-wise sample size. Can be \code{"variable"} or \code{"fixed"}. Defaults
#' to \code{"variable"}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}}, with additional class
#' \code{"multiarm_des_dtl_bern"}, containing the following elements:
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{numeric}} in the slot \code{$maxN} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
#' \item A \code{\link{numeric}} in the slot \code{$n_factor}, for internal use
#' in other functions.
#' \item Each of the input variables.
#' }
#' @examples
#' # The design for the default parameters
#' des <- build_dtl_bern()
#' @seealso \code{\link{des_dtl_bern}}, \code{\link{gui}},
#' \code{\link{opchar_dtl_bern}}, \code{\link{plot.multiarm_des_dtl_bern}},
#' \code{\link{sim_dtl_bern}}.
#' @export
build_dtl_bern <- function(n1 = 210, n10 = 70, e = 2.17, Kv = c(2, 1),
                           alpha = 0.025, beta = 0.1, pi0 = 0.3, delta1 = 0.2,
                           delta0 = 0, ratio = 1, power = "marginal",
                           type = "variable", summary = FALSE) {

  ##### Check input variables ##################################################

  #check_n1_n10(n1, n10, "n1", "n10", int = F)
  check_real_range_strict(e, "e", c(-Inf, Inf), 1)
  Kv    <- check_Kv(Kv, "Kv")
  J     <- length(Kv)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("disjunctive", "marginal"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_build_dtl_bern(n1, n10, e, Kv, alpha, beta, pi0, delta1, delta0,
    #                       ratio, power, stopping, type)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message(" Building outputs..")
  }
  integer    <- all(c(n1, n10)%%1 == 0)
  if (type == "variable") {
    n_factor <- n10
  } else {
    n_factor <- n1
  }
  comp       <- components_dtl_init(alpha, beta, delta0, delta1, integer, Kv,
                                    power, ratio, summary, type, pi0 = pi0,
                                    n_factor = n_factor, e = e)
  tau        <- rbind(numeric(Kv[1]), rep(delta1, Kv[1]),
                      matrix(delta0, Kv[1], Kv[1]) +
                        (delta1 - delta0)*diag(Kv[1]))
  comp       <- components_dtl_update(comp, tau, cbind(pi0, tau + pi0))
  comp       <- opchar_dtl_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = e,
                        integer    = integer,
                        Kv         = Kv,
                        maxN       = comp$opchar$maxN[1],
                        n_factor   = n_factor,
                        n1         = n1,
                        n10        = n10,
                        opchar     = comp$opchar,
                        pi0        = pi0,
                        power      = power,
                        ratio      = ratio,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_dtl_bern", class(output))
  output

}
