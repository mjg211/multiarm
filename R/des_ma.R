#' Design a fixed-sample multi-arm clinical trial
#'
#' \code{des_ma()} determines fixed-sample multi-arm clinical trial designs. It
#' supports a variety of multiple comparison corrections, along with the
#' determination of \emph{A}-, \emph{D}-, and \emph{E}-optimal allocation
#' ratios. In all instances, \code{des_ma()} computes the relevant required
#' sample size in each arm, and returns information on key operating
#' characteristics.
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
#' @param ratio Either a \code{\link{numeric}} vector or a
#' \code{\link{character}} string indicating the chosen value for
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}, the vector of
#' allocation ratios to the experimental arms. Can be specified explicitly
#' as a \code{\link{numeric}} vector, or can be specified as \code{"A"},
#' \code{"D"}, or \code{"E"} to instruct \code{des_ma()} to compute the
#' \emph{A}-, \emph{D}-, or \emph{E}-optimal value for
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}. Defaults to
#' \code{rep(1, K)}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"bonferroni"}, \code{"dunnett"}, \code{"hochberg"}, \code{"holm"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}.
#' Defaults to \code{"dunnett"}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be one of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param integer A \code{\link{logical}} variable indicating whether the
#' computed values in \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the
#' vector of sample sizes required in each arm, should be forced to be whole
#' numbers. Defaults to \code{F}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_des_ma"} containing the
#' following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{numeric}} in the slot \code{$N} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
#' \item A \code{\link{numeric}} vector in the slot \code{$n} specifying
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the vector of sample
#' sizes required in each arm.
#' \item A \code{\link{numeric}} in the slot \code{$pi} specifying the critical
#' threshold for \emph{p}-values,
#' \ifelse{html}{\out{<i>&pi;</i>}}{\eqn{\pi}}, below which null hypotheses
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
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The design for the default parameters
#' des        <- des_ma()
#' # An A-optimal design
#' des_A      <- des_ma(ratio = "A")
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ma(ratio      = rep(1/sqrt(2), 2),
#'                      correction = "holm",
#'                      power      = "disjunctive")
#' @seealso \code{\link{an_ma}}, \code{\link{build_ma}},
#' \code{\link{des_int_ma}}, \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}.
#' @export
des_ma <- function(K = 2, alpha = 0.05, beta = 0.2, delta1 = 0.5, delta0 = 0,
                   sigma = rep(1, K + 1), ratio = rep(1, K),
                   correction = "dunnett", power = "marginal", integer = F,
                   summary = F) {

  ##### Check input variables ##################################################

  K <- check_integer_range(K, "K", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_sigma(sigma, K, "sigma", "K")
  check_ratio(ratio, K, name_ratio = "ratio", name_K = "K")
  check_belong(correction, "correction",
               c("benjamini_hochberg", "bonferroni", "dunnett", "hochberg",
                 "holm", "none", "sidak", "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_ma(K, alpha, beta, delta1, delta0, sigma, ratio, correction,
                   power, integer)
    message("")
  }

  ##### Perform main computations ##############################################

  rho_init                          <- rho <- ratio
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
  pi                                <- piO <- NA
  CovTau                            <- covariance_ma(K, rho, sigma)
  diag_sqrt_I                       <- diag(1/sqrt(diag(CovTau)))
  CovZ                              <- diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  if (correction == "benjamini_hochberg") {
    piO                           <- (1:K)*alpha/K
  } else if (correction == "bonferroni") {
    pi                            <- alpha/K
  } else if (correction == "dunnett") {
    pi                            <-
      stats::pnorm(mvtnorm::qmvnorm(1 - alpha, sigma = CovZ)$quantile,
                   lower.tail = F)
  } else if (correction %in% c("hochberg", "holm")) {
    piO                           <- alpha/(K:1)
  } else if (correction == "none") {
    pi                            <- alpha
  } else if (correction == "sidak") {
    pi                            <- 1 - (1 - alpha)^(1/K)
  } else if (correction == "step_down_dunnett") {
    Ksq                           <- K^2
    if (length(unique(CovZ[(1:Ksq)[-seq(1, Ksq, K + 1)]])) > 1) {
      stop("The step-down Dunnett correction is only supported for scenarios ",
           "with a shared covariance between the test statistics")
    }
    corr                          <- CovZ[2, 1]
    one_min_alpha                 <- 1 - alpha
    piO                           <-
      sapply(1:K,
             function(k) {
               dim                <- K - (k - 1L)
               stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                             sigma =
                                               matrix(corr, dim, dim) +
                                               (1 - corr)*diag(dim))$quantile,
                            lower.tail = F) })
  }
  u                                 <- stats::qnorm(1 - pi)
  uO                                <- stats::qnorm(1 - piO)
  if (summary) {
    message(uc("two_elip"), "identified the rejection rules.")
    message("  Identifying the required sample size", uc("two_elip"))
  }
  tau_power                         <- rep(delta1, K)
  div_by                            <- sqrt(sigma[1]^2/rho[1] +
                                              sigma[-1]^2/rho[-1])
  power_index                       <- NA
  if (power == "marginal") {
    if (length(unique(div_by)) == 1) {
      power_index                   <- 1
    } else if (correction %in% c("holm", "step_down_dunnett")) {
      power_index                   <- which(div_by == max(div_by))[1]
    } else if (correction %in% c("hochberg")) {
      power_index                   <- which(div_by == min(div_by))[1]
    }
    tau_power[-power_index]         <- delta0
  }
  EZ_div_sqrt_N                     <- tau_power/div_by
  if (correction %in% c("benjamini_hochberg", "hochberg", "holm",
                        "step_down_dunnett")) {
    components                      <- outcomes_ma_step(EZ_div_sqrt_N, K,
                                                        correction, CovZ, uO)
  } else {
    components                      <- outcomes_ma_single(K, u)
  }
  if (all(power == "marginal",
          correction %in% c("bonferroni", "dunnett", "none", "sidak"))) {
    N                               <-
      ((u + stats::qnorm(1 - beta))*max(div_by)/delta1)^2
  } else {
    Nmax                            <-
      3*((stats::qnorm(1 - alpha/K) + stats::qnorm(1 - beta))*
           sqrt(sigma[1]^2/rho[1] + max(sigma[-1]^2/rho[-1]))/delta1)^2
    N                               <-
      stats::uniroot(f = power_ma, interval = c(1e-16, Nmax), K = K,
                     beta = beta, correction = correction, power = power,
                     CovZ = CovZ, u = u, uO = uO, EZ_div_sqrt_N = EZ_div_sqrt_N,
                     power_index = power_index, components = components)$root
  }
  if (summary) {
    message(uc("two_elip"), "identified the required sample size.")
    message("  Preparing for outputting", uc("two_elip"))
  }
  n                                 <- N*rho
  tau                               <- rbind(rep(0, K), rep(delta1, K),
                                             matrix(delta0, K, K) +
                                               (delta1 - delta0)*diag(K))
  if (integer) {
    n                               <- ceiling(n)
    N                               <- sum(n)
    rho                             <- n/N
    pi                                <- piO <- NA
    CovTau                            <- covariance_ma(K, rho, sigma)
    diag_sqrt_I                     <- diag(1/sqrt(diag(CovTau)))
    CovZ                              <- diag_sqrt_I%*%CovTau%*%diag_sqrt_I
    if (correction == "benjamini_hochberg") {
      piO                           <- (1:K)*alpha/K
    } else if (correction == "bonferroni") {
      pi                            <- alpha/K
    } else if (correction == "dunnett") {
      pi                            <-
        stats::pnorm(mvtnorm::qmvnorm(1 - alpha, sigma = CovZ)$quantile,
                     lower.tail = F)
    } else if (correction %in% c("hochberg", "holm")) {
      piO                           <- alpha/(K:1)
    } else if (correction == "none") {
      pi                            <- alpha
    } else if (correction == "sidak") {
      pi                            <- 1 - (1 - alpha)^(1/K)
    } else if (correction == "step_down_dunnett") {
      Ksq                           <- K^2
      if (length(unique(CovZ[(1:Ksq)[-seq(1, Ksq, K + 1)]])) > 1) {
        stop("The step-down Dunnett correction is only supported for scenarios ",
             "with a shared covariance between the test statistics")
      }
      corr                          <- CovZ[2, 1]
      one_min_alpha                 <- 1 - alpha
      piO                           <-
        sapply(1:K,
               function(k) {
                 dim                <- K - (k - 1L)
                 stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                               sigma =
                                                 matrix(corr, dim, dim) +
                                                 (1 - corr)*diag(dim))$quantile,
                              lower.tail = F) })
    }
    u                                 <- stats::qnorm(1 - pi)
    uO                                <- stats::qnorm(1 - piO)
  }
  if (correction %in% c("benjamini_hochberg", "hochberg", "holm",
                        "step_down_dunnett")) {
    opchar                          <- opchar_ma_step(tau, K, sigma, correction,
                                                      n, CovZ, uO)
  } else {
    opchar                          <- opchar_ma_single(tau, K, sigma, n, CovZ,
                                                        u)
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
                        integer    = integer,
                        K          = K,
                        N          = N,
                        n          = n,
                        opchar     = opchar,
                        pi         = pi,
                        piO        = piO,
                        power      = power,
                        ratio      = n[-1]/n[1],
                        sigma      = sigma,
                        summary    = summary)
  class(output) <- c(class(output), "multiarm_des_ma")
  output

}
