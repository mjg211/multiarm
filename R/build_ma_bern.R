#' Build a fixed-sample multi-arm clinical trial for a Bernoulli distributed
#' primary outcome
#'
#' \code{build_ma_bern()} builds a fixed-sample multi-arm clinical trial design
#' object assuming the primary outcome variable is Bernoulli distributed, like
#' those returned by \code{\link{des_ma_bern}}.
#'
#' @param n A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the sample size in each
#' arm. Defaults to \code{rep(61, 3)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level.
#' Defaults to 0.05.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to 0.2.
#' @param pi0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&pi;</i><sub>0</sub>}}{\eqn{\pi_0}}, the
#' response rate in the control arm. Defaults to 0.3.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to 0.2.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to 0.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
#' \code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
#' \code{"dunnett"}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be one of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}
#' containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the chosen design.
#' \item A \code{\link{numeric}} in the slot \code{$N} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
#' \item A \code{\link{numeric}} in the slot \code{$gamma} specifying the
#' critical threshold for \emph{p}-values,
#' \ifelse{html}{\out{<i>&gamma;</i></b>}}{\eqn{\gamma}}, below which null
#' hypotheses would be rejected. Will be \code{\link{NA}} if \code{correction}
#' is not a single-step testing procedure.
#' \item A \code{\link{numeric}} vector in the slot \code{$gammaO} specifying
#' the critical thresholds for ordered \emph{p}-values,
#' \ifelse{html}{\out{<b><i>&gamma;</i></b>}}{\eqn{\bold{\gamma}}}, to use with
#' the chosen step-wise testing procedure. Will be \code{\link{NA}} if
#' \code{correction} is not a step-wise testing procedure.
#' \item A \code{\link{numeric}} vector in the slot \code{$ratio} specifying the
#' vector of allocation ratios,
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The design for the default parameters
#' des     <- build_ma_bern()
#' # Modifying the number of experimental treatments and the sample size in each
#' # arm
#' des_K_n <- build_ma_bern(n = rep(100, 4))
#' @seealso  \code{\link{an_ma_bern}}, \code{\link{des_ma_bern}},
#' \code{\link{opchar_ma_bern}}, \code{\link{plot.multiarm_des_ma_bern}},
#' \code{\link{sim_ma_bern}}.
#' @export
build_ma_bern <- function(n = rep(61, 3), alpha = 0.05, beta = 0.2, pi0 = 0.3,
                          delta1 = 0.2, delta0 = 0, correction = "dunnett",
                          power = "marginal", summary = F) {

  ##### Check input variables ##################################################

  check_n(n, name_n = "n", int = F)
  K <- length(n) - 1L
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1", pi0, "pi0")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_build_ma_bern(K, n, alpha, beta, pi0, delta1, delta0, correction,
                          power)
    message("")
  }

  ##### Perform main computations ##############################################

  sigma                                      <-
    c(sqrt(pi0*(1 - pi0)), rep(sqrt((pi0 + delta1)*(1 - pi0 - delta1)), K))
  CovZ                                       <- covariance_ma(K, n, sigma, T)
  gamma_u                                    <- gamma_ma(K, alpha, correction,
                                                         CovZ)
  gamma                                      <- gamma_u$gamma
  gammaO                                     <- gamma_u$gammaO
  u                                          <- gamma_u$u
  uO                                         <- gamma_u$uO
  pi                                         <-
    rbind(rep(pi0, K + 1), c(pi0, rep(pi0 + delta1, K)),
          cbind(rep(pi0, K), matrix(pi0 + delta0, K, K) +
                               (delta1 - delta0)*diag(K)))
  if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                        "holm_bonferroni", "holm_sidak", "step_down_dunnett")) {
    opchar                          <-
      opchar_ma_step_bern(pi, K, alpha, correction, n, uO)
  } else {
    opchar                          <-
      opchar_ma_single_bern(pi, K, alpha, correction, n, u)
  }

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  output        <- list(alpha          = alpha,
                        beta           = beta,
                        correction     = correction,
                        delta0         = delta0,
                        delta1         = delta1,
                        gamma          = gamma,
                        gammaO         = gammaO,
                        integer        = all(n%%1 == 0),
                        K              = K,
                        N              = sum(n),
                        n              = n,
                        opchar         = opchar,
                        pi0            = pi0,
                        power          = power,
                        ratio          = n[-1]/n[1],
                        ratio_scenario = "HG",
                        summary        = summary)
  class(output) <- c(class(output), "multiarm_des_ma_bern")
  output

}
