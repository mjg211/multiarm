#' Build a fixed-sample multi-arm clinical trial
#'
#' \code{build_ma()} builds a fixed-sample multi-arm clinical trial design
#' object, like those returned by \code{\link{des_ma}} and
#' \code{\link{des_int_ma}}.
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
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to 0.5.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to 0.
#' @param sigma A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>&sigma;</i></b>}}{\eqn{\bold{\sigma}}}, the vector
#' of the standard deviations of the responses in each arm. Defaults to
#' \code{rep(1, K + 1)}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"bonferroni"}, \code{"dunnett"}, \code{"hochberg"},
#' \code{"holm_bonferroni"}, \code{"holm_sidak"}, \code{"none"}, \code{"sidak"},
#' and \code{"step_down_dunnett"}. Defaults to \code{"dunnett"}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be one of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_des_ma"} containing the
#' following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the chosen design.
#' \item A \code{\link{numeric}} in the slot \code{$N} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
#' \item A \code{\link{numeric}} in the slot \code{$pi} specifying the critical
#' threshold for \emph{p}-values,
#' \ifelse{html}{\out{<i>&pi;</i></b>}}{\eqn{\pi}}, below which null hypotheses
#' would be rejected. Will be \code{\link{NA}} if \code{correction} is not a
#' single-step testing procedure.
#' \item A \code{\link{numeric}} vector in the slot \code{$piO} specifying the
#' critical thresholds for ordered \emph{p}-values,
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}}, to use with the
#' chosen step-wise testing procedure. Will be \code{\link{NA}} if
#' \code{correction} is not a step-wise testing procedure.
#' \item A \code{\link{matrix}} in the slot \code{$CovZ} specifying the
#' covariance matrix,
#' \ifelse{html}{\out{Cov(<b><i>Z</i></b>)}}{\eqn{Cov(\bold{Z})}}, of the
#' standardised test statistics.
#' \item A \code{\link{numeric}} vector in the slot \code{$rho} specifying the
#' vector of allocation ratios,
#' \ifelse{html}{\out{<b><i>&rho;</i></b>}}{\eqn{\bold{\rho}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The design for the default parameters
#' des     <- build_ma()
#' # Modifying the number of experimental treatments and the sample size in each
#' # arm
#' des_K_n <- build_ma(n = rep(100, 4))
#' @seealso  \code{\link{an_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}.
#' @export
build_ma <- function(n = rep(61, 3), alpha = 0.05, beta = 0.2, delta1 = 0.5,
                     delta0 = 0, sigma = rep(1, K + 1), correction = "dunnett",
                     power = "marginal", summary = F) {

  ##### Check input variables ##################################################

  check_n(n, name_n = "n", int = F)
  K <- length(n) - 1L
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_sigma(sigma, K, "sigma", "K")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "bonferroni", "dunnett", "hochberg",
                 "holm_bonferroni", "holm_sidak", "none", "sidak",
                 "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_build_ma(K, n, alpha, beta, delta1, delta0, sigma, correction,
                     power)
    message("")
  }

  ##### Perform main computations ##############################################

  CovTau                                     <- covariance_ma(K, n, sigma)
  diag_sqrt_I                                <- diag(1/sqrt(diag(CovTau)))
  CovZ                                       <-
    diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  pi_u                                       <- pi_ma(K, alpha, correction,
                                                      CovZ)
  pi                                         <- pi_u$pi
  piO                                        <- pi_u$piO
  u                                          <- pi_u$u
  uO                                         <- pi_u$uO
  tau                                        <-
    rbind(rep(0, K), rep(delta1, K), matrix(delta0, K, K) +
            (delta1 - delta0)*diag(K))
  if (correction %in% c("benjamini_hochberg", "hochberg", "holm_bonferroni",
                        "holm_sidak", "step_down_dunnett")) {
    opchar                                   <-
      opchar_ma_step(tau, K, sigma, correction, n, CovZ, uO)
  } else {
    opchar                                   <- opchar_ma_single(tau, K, sigma,
                                                                 n, CovZ, u)
  }

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        correction = correction,
                        CovZ       = CovZ,
                        delta0     = delta0,
                        delta1     = delta1,
                        integer    = all(n%%1 == 0),
                        K          = K,
                        N          = sum(n),
                        n          = n,
                        opchar     = opchar,
                        pi         = pi,
                        piO        = piO,
                        power      = power,
                        rho        = n/sum(n),
                        sigma      = sigma,
                        summary    = summary)
  class(output) <- c(class(output), "multiarm_des_ma")
  output

}
