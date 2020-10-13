#' Build a single-stage multi-arm clinical trial for a normally distributed
#' primary outcome
#'
#' \code{build_ss_norm()} builds a single-stage multi-arm clinical trial design
#' object assuming the primary outcome variable is normally distributed, like
#' those returned by \code{\link{des_ss_norm}}.
#'
#' @param n A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the sample size in each
#' arm. Defaults to \code{rep(61, 3)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level.
#' Defaults to \code{0.05}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to \code{0.2}.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to \code{0.5}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
#' @param sigma A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>&sigma;</i></b>}}{\eqn{\bold{\sigma}}}, the vector
#' of the standard deviations of the responses in each arm. Defaults to
#' \code{rep(1, K + 1)}.
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
#' @return A \code{\link{list}} of class \code{"multiarm_des_ss_norm"}
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
#' \item A \code{\link{matrix}} in the slot \code{$CovZ} specifying the
#' covariance matrix,
#' \ifelse{html}{\out{Cov(<b><i>Z</i></b>)}}{\eqn{Cov(\bold{Z})}}, of the
#' standardised test statistics.
#' \item A \code{\link{numeric}} vector in the slot \code{$ratio} specifying the
#' vector of allocation ratios,
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}.
#' \item Each of the input variables.
#' }
#' @examples
#' # The design for the default parameters
#' des     <- build_ss_norm()
#' # Modifying the number of experimental treatments and the sample size in each
#' # arm
#' des_K_n <- build_ss_norm(n = rep(100, 4))
#' @seealso \code{\link{des_ss_norm}}, \code{\link{opchar_ss_norm}},
#' \code{\link{plot.multiarm_des_ss_norm}}, \code{\link{sim_ss_norm}}.
#' @export
build_ss_norm <- function(n = rep(98, 3), alpha = 0.025, beta = 0.1,
                          delta1 = 0.5, delta0 = 0, sigma = rep(1, K + 1),
                          correction = "dunnett", power = "marginal",
                          summary = F) {

  ##### Check input variables ##################################################

  check_n(n, name_n = "n", int = F)
  K <- length(n) - 1L
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_sigma(sigma, K, "sigma", "K")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_build_ss_norm(K, n, alpha, beta, delta1, delta0, sigma, correction,
                          power)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message(" Building outputs..")
  }
  integer <- all(n%%1 == 0)
  comp    <- components_ss_norm(alpha, beta, correction, delta0, delta1,
                                integer, K, power, n[-1]/n[1], sigma, n)
  tau     <- rbind(numeric(K), rep(delta1, K),
                   matrix(delta0, K, K) + (delta1 - delta0)*diag(K))
  comp    <- components_ss_update(comp, tau)
  comp    <- opchar_ss_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        correction = correction,
                        CovZ       = comp$CovZ[[1]],
                        delta0     = delta0,
                        delta1     = delta1,
                        gamma      = comp$gamma,
                        gammaO     = comp$gammaO,
                        integer    = integer,
                        K          = K,
                        N          = comp$N,
                        n          = n,
                        opchar     = comp$opchar,
                        power      = power,
                        ratio      = comp$ratio,
                        sigma      = sigma,
                        summary    = summary)
  class(output) <- c("multiarm_des_ss_norm", class(output))
  output

}
