#' Design a fixed-sample multi-arm clinical trial for a Bernoulli distributed
#' primary outcome
#'
#' \code{des_ma_bern()} determines fixed-sample multi-arm clinical trial designs
#' assuming the primary outcome variable is Bernoulli distributed. It supports a
#' variety of multiple comparison corrections, along with the determination of
#' \emph{A}-, \emph{D}-, and \emph{E}-optimal allocation ratios. In all
#' instances, \code{des_ma_bern()} computes the relevant required sample size in
#' each arm, and returns information on key operating characteristics.
#'
#' @param K A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}, the number of experimental treatment
#' arms. Defaults to 2.
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
#' @param ratio Either a \code{\link{numeric}} vector or a
#' \code{\link{character}} string indicating the chosen value for
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}, the vector of
#' allocation ratios to the experimental arms. Can be specified explicitly
#' as a \code{\link{numeric}} vector, or can be specified as \code{"A"},
#' \code{"D"}, or \code{"E"} to instruct \code{des_ma_bern()} to compute the
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
#' power to design the trial for. Can be one of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param integer A \code{\link{logical}} variable indicating whether the
#' computed values in \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the
#' vector of sample sizes required in each arm, should be forced to be whole
#' numbers. Defaults to \code{F}.
#' @param ratio_scenario A \code{\link{character}} string indicating the chosen
#' response rate scenario for which to optimise the allocation ratios. Can be
#' one of \code{"HG"} and \code{"HA"}. Only used if \code{ratio} is a
#' \code{\link{character}} string. Defaults to \code{"HG"}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}
#' containing the following elements
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
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The design for the default parameters
#' des        <- des_ma_bern()
#' # An A-optimal design
#' des_A      <- des_ma_bern(ratio = "A")
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ma_bern(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' @seealso \code{\link{an_ma_bern}}, \code{\link{build_ma_bern}},
#' \code{\link{gui}}, \code{\link{opchar_ma_bern}},
#' \code{\link{plot.multiarm_des_ma_bern}}, \code{\link{sim_ma_bern}}.
#' @export
des_ma_bern <- function(K = 2, alpha = 0.05, beta = 0.2, pi0 = 0.3,
                        delta1 = 0.2, delta0 = 0, ratio = rep(1, K),
                        correction = "dunnett", power = "marginal", integer = F,
                        ratio_scenario = "HG", summary = F) {

  ##### Check input variables ##################################################

  K <- check_integer_range(K, "K", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1", pi0, "pi0")
  check_ratio(ratio, K, name_ratio = "ratio", name_K = "K")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_belong(ratio_scenario, "ratio_scenario", c("HG", "HA"), 1)
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_ma_bern(K, alpha, beta, pi0, delta1, delta0, ratio, correction,
                        power, integer, ratio_scenario)
    message("")
  }

  ##### Perform main computations ##############################################

  rho_init                          <- rho <- ratio
  if (ratio_scenario == "HG") {
    sigma                           <- rep(sqrt(pi0*(1 - pi0)), K + 1)
  } else if (ratio_scenario == "HA") {
    sigma                           <-
      c(sqrt(pi0*(1 - pi0)), rep(sqrt((pi0 + delta1)*(1 - pi0 - delta1)), K))
  }
  if (is.character(rho)) {
    if (summary) {
      message("  Identifying the ", rho, "-optimal allocation ratio",
              uc("two_elip"))
    }
    if (rho == "A") {
      rho0                          <- sigma[1]*sqrt(K)
      rho                           <-
        c(rho0, sigma[-1])/(rho0 + sum(sigma[-1]))
    } else {
      Kp1                           <- K + 1L
      if (rho == "D") {
        obj_func                    <- determinant_ma
      } else {
        obj_func                    <- max_eigenvalue_ma
      }
      rho                           <-
        Rsolnp::solnp(pars    = rep(1/Kp1, Kp1),
                      fun     = obj_func,
                      eqfun   = function(rho, K, sigma) { sum(rho) - 1 },
                      eqB     = 0L,
                      LB      = integer(Kp1),
                      UB      = rep(1L, Kp1),
                      control = list(trace = 0L),
                      K       = K,
                      sigma   = sigma)$pars
    }
    if (summary) {
      message(uc("two_elip"), "identified the ", rho_init, "-optimal ",
              "allocation ratio.")
    }
  } else {
    rho                             <- c(1, rho)/(1 + sum(rho))
  }
  if (summary) {
    message("  Identifying the rejection rules", uc("two_elip"))
  }
  if (ratio_scenario == "HG") {
    sigma                           <-
      c(sqrt(pi0*(1 - pi0)), rep(sqrt((pi0 + delta1)*(1 - pi0 - delta1)), K))
  }
  CovZ                              <- covariance_ma(K, rho, sigma, T)
  gamma_u                           <- gamma_ma(K, alpha, correction, CovZ)
  gamma                             <- gamma_u$gamma
  gammaO                            <- gamma_u$gammaO
  u                                 <- gamma_u$u
  uO                                <- gamma_u$uO
  if (summary) {
    message(uc("two_elip"), "identified the rejection rules.")
    message("  Identifying the required sample size", uc("two_elip"))
  }
  tau_power                         <- rep(delta1, K)
  sigma_power                       <-
    c(sqrt(pi0*(1 - pi0)), rep(sqrt((pi0 + delta1)*(1 - pi0 - delta1)), K))
  div_by                            <- sqrt(sigma_power[1]^2/rho[1] +
                                              sigma_power[-1]^2/rho[-1])
  power_index                       <- NA
  if (power == "marginal") {
    if (length(unique(div_by)) == 1) {
      power_index                   <- 1
    } else if (correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
      power_index                   <- which(div_by == max(div_by))[1]
    } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                                 "hochberg")) {
      power_index                   <- which(div_by == min(div_by))[1]
    }
    tau_power[-power_index]         <- delta0
    sigma_power[-c(1, power_index +
                     1)]            <- sqrt((pi0 + delta0)*(1 - pi0 - delta0))
    div_by                          <- sqrt(sigma_power[1]^2/rho[1] +
                                                sigma_power[-1]^2/rho[-1])
  }
  EZ_div_sqrt_N                     <- tau_power/div_by
  CovZ_power                        <- covariance_ma(K, rho, sigma_power, T)
  if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                        "holm_bonferroni", "holm_sidak", "step_down_dunnett")) {
    components                      <- outcomes_ma_step(EZ_div_sqrt_N, K,
                                                        correction, CovZ_power,
                                                        uO)
  } else {
    components                      <- outcomes_ma_single(K, u)
  }
  if (all(power == "marginal",
          correction %in% c("bonferroni", "dunnett", "none", "sidak"))) {
    if (correction == "dunnett") {
      gamma_u                       <- gamma_ma(K, alpha, correction,
                                                CovZ_power)
      gamma_power                   <- gamma_u$gamma
      u_power                       <- gamma_u$u
    } else {
      u_power                       <- u
    }
    N                               <-
      ((u_power + stats::qnorm(1 - beta))*max(div_by)/delta1)^2
  } else {
    Nmax                            <-
      3*((stats::qnorm(1 - alpha/K) + stats::qnorm(1 - beta))*
           sqrt(sigma_power[1]^2/rho[1] +
                  max(sigma_power[-1]^2/rho[-1]))/delta1)^2
    N                               <-
      stats::uniroot(f = power_ma, interval = c(1e-16, Nmax), K = K,
                     beta = beta, correction = correction, power = power,
                     CovZ = CovZ_power, u = u, uO = uO,
                     EZ_div_sqrt_N = EZ_div_sqrt_N, type = "bernoulli",
                     power_index = power_index, components = components)$root
  }
  if (summary) {
    message(uc("two_elip"), "identified the required sample size.")
    message("  Preparing for outputting", uc("two_elip"))
  }
  n                                 <- N*rho
  pi                                <- rbind(rep(pi0, K + 1),
                                             c(pi0, rep(pi0 + delta1, K)),
                                             cbind(rep(pi0, K),
                                                   matrix(pi0 + delta0, K, K) +
                                                     (delta1 - delta0)*diag(K)))
  if (integer) {
    n                               <- ceiling(n)
    N                               <- sum(n)
    rho                             <- n/N
    CovZ                            <- covariance_ma(K, n, sigma, T)
    gamma_u                         <- gamma_ma(K, alpha, correction, CovZ)
    gamma                           <- gamma_u$gamma
    gammaO                          <- gamma_u$gammaO
    u                               <- gamma_u$u
    uO                              <- gamma_u$uO
  }
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
                        integer        = integer,
                        K              = K,
                        N              = N,
                        n              = n,
                        opchar         = opchar,
                        pi0            = pi0,
                        power          = power,
                        ratio          = n[-1]/n[1],
                        ratio_scenario = ratio_scenario,
                        summary        = summary)
  class(output) <- c(class(output), "multiarm_des_ma_bern")
  output

}
