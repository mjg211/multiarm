#' Design a single-stage multi-arm clinical trial for a normally distributed
#' primary outcome
#'
#' \code{des_ss_norm()} determines single-stage multi-arm clinical trial designs
#' assuming the primary outcome variable is normally distributed. It supports a
#' variety of multiple comparison corrections, along with the determination of
#' \emph{A}-, \emph{D}-, and \emph{E}-optimal allocation ratios. In all
#' instances, \code{des_ss_norm()} computes the required sample size in each
#' arm, and returns information on key operating characteristics.
#'
#' @param K A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}, the number of experimental treatment
#' arms. Defaults to \code{2}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level.
#' Defaults to \code{0.025}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to \code{0.1}.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to \code{0.5}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
#' @param sigma A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>&sigma;</i></b>}}{\eqn{\bold{\sigma}}}, the vector
#' of standard deviations of the responses in each arm. Defaults to
#' \code{rep(1, K + 1)}.
#' @param ratio Either a \code{\link{numeric}} vector or a
#' \code{\link{character}} string indicating the chosen value for
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}, the vector of
#' allocation ratios to the experimental arms. Can be specified explicitly
#' as a \code{\link{numeric}} vector, or can be specified as \code{"A"},
#' \code{"D"}, or \code{"E"} to instruct \code{des_ss_norm()} to compute the
#' \emph{A}-, \emph{D}-, or \emph{E}-optimal value for
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}. Defaults to
#' \code{rep(1, K)}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
#' \code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
#' \code{"dunnett"}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be any of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param integer A \code{\link{logical}} variable indicating whether the
#' computed values in \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the
#' vector of sample sizes required in each arm, should be forced to be whole
#' numbers. Defaults to \code{FALSE}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}}, with additional class
#' \code{"multiarm_des_ss_norm"}, containing the following elements:
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{numeric}} in the slot \code{$N} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
#' \item A \code{\link{numeric}} vector in the slot \code{$n} specifying
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the vector of sample
#' sizes required in each arm.
#' \item A \code{\link{numeric}} in the slot \code{$gamma} specifying the
#' critical threshold for \emph{p}-values,
#' \ifelse{html}{\out{<i>&gamma;</i>}}{\eqn{\gamma}}, below which null
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
#' \item Each of the input variables.
#' }
#' @examples
#' # The design for the default parameters
#' des        <- des_ss_norm()
#' # An A-optimal design
#' des_A      <- des_ss_norm(ratio = "A")
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ss_norm(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' @seealso \code{\link{build_ss_norm}}, \code{\link{gui}},
#' \code{\link{opchar_ss_norm}}, \code{\link{plot.multiarm_des_ss_norm}},
#' \code{\link{sim_ss_norm}}.
#' @export
des_ss_norm <- function(K = 2, alpha = 0.025, beta = 0.1, delta1 = 0.5,
                        delta0 = 0, sigma = rep(1, K + 1), ratio = rep(1, K),
                        correction = "dunnett", power = "marginal",
                        integer = FALSE, summary = FALSE) {

  ##### Check input variables ##################################################

  K <- check_integer_range(K, "K", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_sigma(sigma, K, "sigma", "K")
  check_ratio(ratio, K, name_ratio = "ratio", name_K = "K")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Preliminaries ##########################################################

  comp <- components_ss_norm(alpha, beta, correction, delta0, delta1, integer,
                             K, power, ratio, sigma,
                             c(1, ratio)/(1 + sum(ratio)))
  if (summary) {
    summary_des_ss_norm(comp)
    message("")
  }

  ##### Main computations ######################################################

  if (is.character(ratio)) {
    if (summary) {
      message("  Identifying the ", ratio, "-optimal allocation ratio..")
    }
    if (ratio == "A") {
      rho0       <- sigma[1]*sqrt(K)
      comp$n     <- c(rho0, sigma[-1])/(rho0 + sum(sigma[-1]))
    } else {
      Kp1        <- K + 1L
      if (ratio == "D") {
        obj_func <- determinant_ss
      } else {
        obj_func <- max_eigenvalue_ss
      }
      comp$n     <- Rsolnp::solnp(pars    = rep(1/Kp1, Kp1),
                                  fun     = obj_func,
                                  eqfun   =
                                    function(rho, K, sigma) { sum(rho) - 1 },
                                  eqB     = 0L,
                                  LB      = integer(Kp1),
                                  UB      = rep(1L, Kp1),
                                  control = list(trace = 0L),
                                  K       = K,
                                  sigma   = sigma)$pars
    }
    comp$ratio   <- comp$n[-1]/comp$n[1]
    if (summary) {
      message("..identified the ", ratio, "-optimal allocation ratio.")
    }
  }
  if (summary) {
    message("  Building required design components..")
  }
  comp           <- components_ss_power_setup(comp)
  comp           <- components_ss_update(comp)
  if (summary) {
    message("..built required design components.")
    message("  Identifying the required sample size..")
  }
  comp           <- sample_size_ss(comp)
  if (summary) {
    message("..identified the required sample size.")
    message("  Preparing for outputting..")
  }
  comp           <- integer_ss(comp)
  comp           <-
    components_ss_update(comp, rbind(numeric(K), rep(delta1, K),
                                     matrix(delta0, K, K) +
                                       (delta1 - delta0)*diag(K)))
  comp           <- opchar_ss_internal(comp)

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
                        n          = comp$n,
                        opchar     = comp$opchar,
                        power      = power,
                        ratio      = comp$n[-1]/comp$n[1],
                        sigma      = sigma,
                        summary    = summary)
  class(output) <- c("multiarm_des_ss_norm", class(output))
  output

}
