an_ma_norm_internal       <- function(comp, pval) {
  nrow_pval                          <- nrow(pval)
  reject                             <- matrix(0L, nrow_pval, comp$K)
  for (i in 1:nrow_pval) {
    if (comp$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      reject[i, ]                    <- (pval[i, ] < comp$gamma)
    } else if (comp$correction %in% c("holm_bonferroni", "holm_sidak",
                                      "step_down_dunnett")) {
      order_pval                     <- order(pval[i, ])
      k                              <- check <- 1
      while (all(k <= comp$K, check == 1)) {
        if (pval[i, order_pval[k]] < comp$gammaO[k]) {
          reject[i, order_pval[k]]   <- reject[i, order_pval[k]] + 1
          k                          <- k + 1
        }
        else {
          check                      <- 0
        }
      }
    } else if (comp$correction %in% c("benjamini_hochberg",
                                      "benjamini_yekutieli", "hochberg")) {
      order_pval                     <- order(pval[i, ])
      for (k in comp$K:1) {
        if (pval[i, order_pval[k]] < comp$gammaO[k]) {
          reject[i, order_pval[1:k]] <- rep(1, k)
          break
        }
      }
    }
  }
  an                                 <- cbind(pval, reject)
  colnames(an)                       <- c(paste("p", 1:comp$K, sep = ""),
                                          paste("H", 1:comp$K, sep = ""))
  tibble::as_tibble(an)
}

components_ma_update_bern <- function(CovZ, components) {
  seq_K                       <- 1:K
  if (correction == "dunnett") {
    gamma                     <-
      stats::pnorm(mvtnorm::qmvnorm(1 - alpha, sigma = CovZ)$quantile,
                   lower.tail = F)
    u                         <- stats::qnorm(1 - gamma)
    two_to_K                  <- 2^K
    components$ls             <- rep(list(rep(-Inf, K)), two_to_K)
    components$us             <- rep(list(rep(Inf, K)), two_to_K)
    components$us[[1]]        <- components$ls[[two_to_K]] <- rep(u, K)
    for (i in 2:(two_to_K - 1)) {
      rej                     <- which(components$outcomes[i, seq_K] == 1)
      components$ls[[i]][rej] <- components$us[[i]][-rej] <- u
    }
  } else if (correction == "step_down_dunnett") {
    Ksq                       <- K^2
    if (length(unique(CovZ[(1:Ksq)[-seq(1, Ksq, K + 1)]])) > 1) {
      stop("The step-down Dunnett correction is only supported for scenarios ",
           "with a shared covariance between the test statistics")
    }
    corr                      <- CovZ[2, 1]
    one_min_alpha             <- 1 - alpha
    gammaO                    <-
      sapply(1:K,
             function(k) {
               dim            <- K - (k - 1L)
               stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                             sigma =
                                               matrix(corr, dim, dim) +
                                               (1 - corr)*diag(dim))$quantile,
                            lower.tail = F) })
    uO                        <- stats::qnorm(1 - gammaO)
    rows_outcomes             <- factorial(K + 1)
    for (i in 1:factorial(K + 1)) {
      sum_i                   <- sum(components$outcomes[i, K + seq_K])
      if (sum_i < K) {
        components$ls[[i]]    <- c(numeric(K - 1), uO[seq_len(sum_i)],
                                   -uO[sum_i + 1])
      } else {
        components$ls[[i]]    <- c(numeric(K - 1), uO)
      }
    }
  }
  components
}

fdr_ma_single             <- function(tau, K, u, n, sigma, st_Lambda) {
  rows_tau       <- nrow(tau)
  fdr            <- numeric(rows_tau)
  if (any(tau <= 0)) {
    components   <- outcomes_ma_single(K, u)
    two_to_K     <- 2L^K
    seq_K        <- 1:K
    expZs        <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                               rows_tau, K, byrow = T)
    discoveries  <- Rfast::rowsums(components$outcomes)
    discoveries[which(discoveries == 0)] <- 1
    for (t in 1:rows_tau) {
      if (any(tau[t, ] <= 0)) {
        false_discoveries <- Rfast::rowsums(matrix(tau[t, ] <= 0, two_to_K, K,
                                                   byrow = T)*components$outcomes)
        for (i in which(false_discoveries > 0L)) {
          fdr[t]   <- fdr[t] + mvtnorm::pmvnorm(components$ls[[i]],
                                                components$us[[i]], expZs[t, ],
                                                sigma = st_Lambda)[1]*
            false_discoveries[i]/discoveries[i]
        }
      }
    }
  }
  fdr           <- cbind(tau, fdr)
  colnames(fdr) <- c(paste("tau", seq_K, sep = ""), "FDR(tau)")
  fdr           <- tibble::as_tibble(fdr)
  fdr
}

fdr_ma_step               <- function(tau, K, correction, uO, n, sigma,
                                      st_Lambda) {
  rows_tau        <- nrow(tau)
  fdr             <- numeric(rows_tau)
  if (any(tau <= 0)) {
    expZs         <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                rows_tau, K, byrow = T)
    components    <- outcomes_ma_step(expZs[1, ], K, correction, uO, st_Lambda)
    rows_outcomes <- factorial(K)*(K + 1L)
    two_to_K      <- 2L^K
    seq_KpK       <- K + (seq_K <- 1:K)
    discoveries  <- Rfast::rowsums(components$outcomes)
    discoveries[which(discoveries == 0)] <- 1
    for (t in 1:rows_tau) {
      components  <- outcomes_ma_step(expZs[t, ], K, correction, uO, st_Lambda,
                                      components)
      if (any(tau[t, ] <= 0)) {
        false_discoveries <- Rfast::rowsums(matrix(tau[t, ] <= 0, two_to_K, K,
                                                   byrow = T)*components$outcomes)
        for (i in which(false_discoveries > 0L)) {
          fdr[t]      <- fdr[t] + mvtnorm::pmvnorm(components$ls[[i]],
                                                   components$us[[i]],
                                                   components$means[[i]],
                                                   sigma =
                                                     components$st_Lambdas[[i]])[1]*
            false_discoveries[i]/discoveries[i]
        }
      }
    }
  }
  fdr            <- cbind(tau, fdr)
  colnames(fdr)  <- c(paste("tau", seq_K, sep = ""), "FDR(tau)")
  fdr            <- tibble::as_tibble(fdr)
  fdr
}

fwer_ma_single            <- function(tau, K, u, n, sigma, st_Lambda) {
  rows_tau       <- nrow(tau)
  P              <- numeric(rows_tau)
  if (any(tau <= 0)) {
    components   <- outcomes_ma_single(K, u)
    two_to_K     <- 2L^K
    seq_K        <- 1:K
    expZs        <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                               rows_tau, K, byrow = T)
    for (t in 1:rows_tau) {
      if (any(tau[t, ] <= 0)) {
        for (i in which(Rfast::rowsums(matrix(tau[t, ] <= 0, two_to_K, K,
                                              byrow = T)*
                                       components$outcomes[, seq_K]) > 0L)) {
          P[t]   <- P[t] + mvtnorm::pmvnorm(components$ls[[i]],
                                            components$us[[i]], expZs[t, ],
                                            sigma = st_Lambda)[1]
        }
      }
    }
  }
  fwer           <- cbind(tau, P)
  colnames(fwer) <- c(paste("tau", seq_K, sep = ""), "FWER(tau)")
  fwer           <- tibble::as_tibble(fwer)
  fwer
}

fwer_ma_step              <- function(tau, K, correction, uO, n, sigma,
                                      st_Lambda) {
  rows_tau        <- nrow(tau)
  P               <- numeric(rows_tau)
  if (any(tau <= 0)) {
    expZs         <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                rows_tau, K, byrow = T)
    components    <- outcomes_ma_step(expZs[1, ], K, correction, uO, st_Lambda)
    rows_outcomes <- factorial(K)*(K + 1L)
    seq_KpK       <- K + (seq_K <- 1:K)
    for (t in 1:rows_tau) {
      components  <- outcomes_ma_step(expZs[t, ], K, correction, uO, st_Lambda,
                                      components)
      for (i in which(Rfast::rowsums(matrix(tau[t, ] <= 0, rows_outcomes, K,
                                            byrow = T)*
                                     components$outcomes[, seq_KpK]) > 0L)) {
        P[t]      <- P[t] + mvtnorm::pmvnorm(components$ls[[i]],
                                             components$us[[i]],
                                             components$means[[i]],
                                             sigma =
                                               components$st_Lambdas[[i]])[1]
      }
    }
  }
  fwer            <- cbind(tau, P)
  colnames(fwer)  <- c(paste("tau", seq_K, sep = ""), "FWER(tau)")
  fwer            <- tibble::as_tibble(fwer)
  fwer
}

obj_fn_gen_mams           <- function(pars, components) {
  bounds                                                 <-
    c(pars[components$pars_seq1],
      pars[components$pars_seq2] + pars[components$pars_seq3],
      pars[components$Jp1], -Inf, Inf)
  for (i in 1:components$nrow_thetas_d_HG_a_fwer) {
    components$thetas_d_HG_a_fwer[i, components$twoKp4]  <-
      mvtnorm::pmvnorm(bounds[components$l_indices_HG[[i]]],
                       bounds[components$u_indices_HG[[i]]],
                       components$means_HG[[i]],
                       sigma = components$Lambdas_HG[[i]])[1]
  }
  for (i in 1:components$nrow_thetas_bc_LFC_power) {
    components$thetas_bc_LFC_power[i, components$twoKp4] <-
      mvtnorm::pmvnorm(bounds[components$l_indices_LFC[[i]]],
                       bounds[components$u_indices_LFC[[i]]],
                       sqrt(pars[1])*components$mean_factors_LFC[[i]],
                       sigma = components$Lambdas_LFC[[i]])[1]
  }
  a_fwer                           <-
    sum(components$thetas_d_HG_a_fwer[, components$twoKp2]*
          components$thetas_d_HG_a_fwer[, components$twoKp4])
  bc_typeII                        <-
    1 - sum(components$thetas_bc_LFC_power[, components$twoKp2]*
              components$thetas_bc_LFC_power[, components$twoKp4])
  ESS_HG                           <-
    sum(components$thetas_d_HG_a_fwer[, components$twoKp1]*
          components$thetas_d_HG_a_fwer[, components$twoKp3]*
          components$thetas_d_HG_a_fwer[, components$twoKp4])
  ESS_LFC                          <-
    sum(components$thetas_bc_LFC_power[, components$twoKp1]*
          components$thetas_bc_LFC_power[, components$twoKp3]*
          components$thetas_bc_LFC_power[, components$twoKp4])
  sum(w*pars[1]*c(ESS_HG, ESS_LFC, components$max_N_factor)) +
    components$N_fixed*(as.numeric(a_fwer > components$alpha)*
                          (a_fwer - components$alpha)/components$alpha +
                          as.numeric(bc_typeII > components$beta)*
                          (bc_typeII - components$beta)/components$beta)
}

root_bound_gen_ma         <- function(pars, K, alpha, components) {
  bounds                                     <- c(pars, pars, -Inf, Inf)
  for (i in 1:components$nrow_thetas_d_HG_a_fwer) {
    components$thetas_d_HG_a_fwer[i, components$twoKp4] <-
      mvtnorm::pmvnorm(bounds[components$l_indices_HG[[i]]],
                       bounds[components$u_indices_HG[[i]]],
                       components$means_HG[[i]],
                       sigma = components$Lambdas_HG[[i]])[1]
  }
  sum(components$thetas_d_HG_a_fwer[, components$twoKp2]*
        components$thetas_d_HG_a_fwer[, components$twoKp4]) - alpha
}

root_ss_gen_ma            <- function(pars, K, beta, components, e) {
  bounds                                      <- c(e, e, -Inf, Inf)
  for (i in 1:components$nrow_thetas_bc_LFC_power) {
    components$thetas_bc_LFC_power[i, components$twoKp4] <-
      mvtnorm::pmvnorm(bounds[components$l_indices_LFC[[i]]],
                       bounds[components$u_indices_LFC[[i]]],
                       sqrt(pars)*components$means_div_par_LFC[[i]],
                       sigma = components$Lambdas_LFC[[i]])[1]
  }
  sum(components$thetas_bc_LFC_power[, components$twoKp2]*
        components$thetas_bc_LFC_power[, components$twoKp4]) - (1 - beta)
}

summary_an_ma             <- function(des, pval, type) {
  message("  ", rep("-", 79))
  message("  ", des$K, "-experimental treatment fixed-sample multi-arm trial ",
          "analysis")
  message("  ", rep("-", 79))
  if (type == "normal") {
    message("  The outcome variables will be assumed to be normally ",
            "distributed.\n")
    text       <- "mu"
  } else {
    message("  The outcome variables will be assumed to be Bernoulli ",
            "distributed.\n")
    text       <- "pi"
  }
  message("  The hypotheses to be tested will be:\n")
  if (des$K == 2) {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, H2 : tau2 = ",
            text, "2 - ", text, "0 <= 0.\n")
  } else {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, ... , H", des$K,
            " : tau", des$K, " = ", text, des$K, " - ", text, "0 <= 0.\n")
  }
  if (des$correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(des$alpha, 4), ".")
  } else if (des$correction %in% c("benjamini_hochberg",
                                   "benjamini_yekutieli")) {
    if (des$correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ",
            round(des$alpha, 4), " using the Benjamini-", sub_type,
            " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[des$correction]
    message("  Strong control of the FWER to level alpha = ",
            round(des$alpha, 4), " will be achieved using ", segment, "\n")
  }
  if (des$K == 2) {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p1,p2).")
  } else if (des$K == 3) {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p1,p2,p3).")
  } else {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p1,...,p", des$K, ").")
  }
}

trace_ma                  <- function(n, K, sigma) {
  K*sigma[1]^2/n[1] + sum(sigma[-1]^2/n[-1])
}

##### an_ma_bern ###############################################################
#' Analyse results from a fixed-sample multi-arm clinical trial for a Bernoulli
#' distributed primary outcome
#'
#' \code{an_ma_bern()} analyses results from a specified fixed-sample multi-arm
#' clinical trial design for a Bernoulli distributed primary outcome variable,
#' to determine which null hypotheses to reject.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}, as
#' returned by \code{\link{build_ma_bern}} or \code{\link{des_ma_bern}} (i.e., a
#' fixed-sample multi-arm clinical trial design for a Bernoulli distributed
#' outcome). Defaults to \code{des_ma_bern()}.
#' @param pval A \code{\link{matrix}} (or \code{\link{numeric}} vector that can
#' be coerced in to \code{\link{matrix}} form) whose rows indicate the values of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}} for which to determine
#' which null hypotheses would be rejected. Defaults to
#' \code{rep(0.5*des$alpha, des$K)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (i.e., \code{alpha} can be used to modify/update \code{des} if desired).
#' Defaults to \code{des$alpha}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
#' \code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
#' \code{"des$correction"} (i.e., \code{correction} can be used to modify/update
#' \code{des} if desired).
#' @param sigmas A \code{\link{matrix}} (or \code{\link{numeric}} vector that
#' can be coerced in to \code{\link{matrix}} form) indicating the assumed values
#' of the standard deviations in each of the arms, for each row of \code{pval}.
#' Should either have dimension equal to \code{nrow(pval)}
#' \ifelse{html}{\out{x}}{\eqn{\times}} \code{des$K + 1}, or be coercible in to
#' a form with dimension 1
#' \ifelse{html}{\out{x}}{\eqn{\times}} \code{des$K + 1} (which will then be
#' replicated to have \code{nrow(pval)} rows). Only used if \code{correction} is
#' equal to \code{"dunnett"} or \code{"step_down_dunnett"}. Defaults internally
#' to the standard deviations for the global null hypothesis.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$an} detailing which null
#' hypotheses would be rejected for each value of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The results for the default parameters
#' an         <- an_ma_bern()
#' # An A-optimal design and a selection of possible p-values.
#' des_A      <- des_ma_bern(ratio = "A")
#' an_A       <- an_ma_bern(des_A, rbind(c(0.01, 0.01),
#'                                       c(0.025, 0.025),
#'                                       c(0.04, 0.04),
#'                                       c(0.05, 0.05),
#'                                       c(0.01, 0.06)))
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and a selection of possible
#' # p-values
#' des_root_K <- des_ma_bern(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' an_root_K  <- an_ma_bern(des_root_K, rbind(c(0.01, 0.01),
#'                                            c(0.025, 0.025),
#'                                            c(0.04, 0.04),
#'                                            c(0.05, 0.05),
#'                                            c(0.01, 0.06)))
#' @seealso \code{\link{build_ma_bern}}, \code{\link{des_ma_bern}},
#' \code{\link{opchar_ma_bern}}, \code{\link{plot.multiarm_des_ma_bern}},
#' \code{\link{sim_ma_bern}}.
#' @noRd
an_ma_bern <- function(des = des_ma_bern(), pval = rep(0.5*des$alpha, des$K),
                       alpha = des$alpha, correction = des$correction,
                       sigmas, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma_bern(des)
  pval <- check_pval(pval, des$K, "pval", "des$K")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  if (missing(sigmas)) {
    sigmas <- matrix(sqrt(des$pi0*(1 - des$pi0)), nrow(pval), des$K + 1)
  } else {
    check_sigmas(sigmas, pval)
  }
  check_logical(summary, "summary")

  ##### Update des #############################################################

  if (any(correction != des$correction, alpha != des$alpha)) {
    des$correction                   <- correction
    des$alpha                        <- alpha
    sigma                            <-
      c(sqrt(des$pi0*(1 - des$pi0)),
        rep(sqrt((des$pi0 + des$delta1)*(1 - des$pi0 - des$delta1)), des$K))
    CovZ                             <- covariance_ma(des$K, des$n, sigma, T)
    gamma_u                          <- gamma_ma(des$K, des$alpha,
                                                 des$correction, CovZ)
    des$gamma                        <- gamma_u$gamma
    des$gammaO                       <- gamma_u$gammaO
    pi                               <-
      rbind(rep(des$pi0, des$K + 1), c(des$pi0,
                                       rep(des$pi0 + des$delta1, des$K)),
            cbind(rep(des$pi0, des$K),
                  matrix(des$pi0 + des$delta0, des$K, des$K) +
                    (des$delta1 - des$delta0)*diag(des$K)))
    if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                          "hochberg", "holm_bonferroni", "holm_sidak",
                          "step_down_dunnett")) {
      opchar                          <-
        opchar_ma_step_bern(pi, des$K, des$alpha, des$correction, des$n,
                            gamma_u$uO)
    } else {
      opchar                          <-
        opchar_ma_single_bern(des$pi, des$K, des$alpha, des$correction, des$n,
                              gamma_u$u)
    }
  }

  ##### Print summary ##########################################################

  if (summary) {
    summary_an_ma(des, pval, "bernoulli")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Identifying which hypotheses to reject", uc("two_elip"))
  }
  nrow_pval                          <- nrow(pval)
  reject                             <- matrix(0L, nrow_pval, des$K)
  gamma                              <- des$gamma
  gammaO                             <- des$gammaO
  seq_K                              <- 1:des$K
  Kp1                                <- des$K + 1L
  one_min_alpha                      <- 1 - des$alpha
  for (i in 1:nrow_pval) {
    if (des$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      if (des$correction == "dunnett") {
        gamma                        <-
          stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                        sigma = covariance_ma(des$K, des$n,
                                                              sigmas[i, ],
                                                              T))$quantile,
                       lower.tail = F)
      }
      reject[i, ]                    <- (pval[i, ] < gamma)
    } else if (des$correction %in% c("holm_bonferroni", "holm_sidak",
                                     "step_down_dunnett")) {
      if (des$correction == "step_down_dunnett") {
        corr                         <- covariance_ma(des$K, des$n, sigmas[i, ],
                                                      T)[2, 1]
        gammaO                       <-
          sapply(seq_K,
                 function(k) {
                   dim               <- Kp1 - k
                   stats::pnorm(
                     mvtnorm::qmvnorm(one_min_alpha,
                                      sigma = matrix(corr, dim, dim) +
                                        (1 - corr)*diag(dim))$quantile,
                     lower.tail = F) })
      }
      order_pval                     <- order(pval[i, ])
      k                              <- check <- 1
      while (all(k <= des$K, check == 1)) {
        if (pval[i, order_pval[k]] < gammaO[k]) {
          reject[i, order_pval[k]]   <- reject[i, order_pval[k]] + 1
          k                          <- k + 1
        }
        else {
          check                      <- 0
        }
      }
    } else if (des$correction %in% c("benjamini_hochberg",
                                     "benjamini_yekutieli", "hochberg")) {
      order_pval                     <- order(pval[i, ])
      for (k in des$K:1) {
        if (pval[i, order_pval[k]] < des$gammaO[k]) {
          reject[i, order_pval[1:k]] <- rep(1, k)
          break
        }
      }
    }
  }
  if (summary) {
    message(uc("two_elip"), "hypotheses to reject identified.")
    message("  Preparing for outputting", uc("two_elip"))
  }
  an                                 <- cbind(pval, reject)
  colnames(an)                       <- c(paste("p", 1:des$K, sep = ""),
                                          paste("H", 1:des$K, sep = ""))
  an                                 <- tibble::as_tibble(an)

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  list(alpha      = alpha,
       an         = an,
       correction = correction,
       des        = des,
       pval       = pval,
       sigmas     = sigmas,
       summary    = summary)

}

##### an_ma_norm ###############################################################
#' Analyse results from a fixed-sample multi-arm clinical trial for a normally
#' distributed primary outcome
#'
#' \code{an_ma_norm()} analyses results from a specified fixed-sample multi-arm
#' clinical trial design for a normally distributed primary outcome variable,
#' to determine which null hypotheses to reject.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma_norm"}, as
#' returned by \code{\link{build_ma_norm}} or \code{\link{des_ma_norm}} (i.e., a
#' fixed-sample multi-arm clinical trial design for a normally distributed
#' outcome). Defaults to \code{des_ma_norm()}.
#' @param pval A \code{\link{matrix}} (or \code{\link{numeric}} vector that can
#' be coerced in to \code{\link{matrix}} form) whose rows indicate the values of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}} for which to determine
#' which null hypotheses would be rejected. Defaults to
#' \code{rep(0.5*des$alpha, des$K)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (i.e., \code{alpha} can be used to modify/update \code{des} if desired).
#' Defaults to \code{des$alpha}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
#' \code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
#' \code{"des$correction"} (i.e., \code{correction} can be used to modify/update
#' \code{des} if desired).
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$an} detailing which null
#' hypotheses would be rejected for each value of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The results for the default parameters
#' an         <- an_ma_norm()
#' # An A-optimal design and a selection of possible p-values.
#' des_A      <- des_ma_norm(ratio = "A")
#' an_A       <- an_ma_norm(des_A, rbind(c(0.01, 0.01),
#'                                       c(0.025, 0.025),
#'                                       c(0.04, 0.04),
#'                                       c(0.05, 0.05),
#'                                       c(0.01, 0.06)))
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and a selection of possible
#' # p-values
#' des_root_K <- des_ma_norm(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' an_root_K  <- an_ma_norm(des_root_K, rbind(c(0.01, 0.01),
#'                                            c(0.025, 0.025),
#'                                            c(0.04, 0.04),
#'                                            c(0.05, 0.05),
#'                                            c(0.01, 0.06)))
#' @seealso \code{\link{build_ma_norm}}, \code{\link{des_ma_norm}},
#' \code{\link{opchar_ma_norm}}, \code{\link{plot.multiarm_des_ma_norm}},
#'  \code{\link{sim_ma_norm}}.
#' @noRd
an_ma_norm <- function(des = des_ma_norm(), pval = rep(0.5*des$alpha, des$K),
                       alpha = des$alpha, correction = des$correction,
                       summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma_norm(des)
  pval <- check_pval(pval, des$K, "pval", "des$K")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_logical(summary, "summary")

  ##### Update des #############################################################

  if (any(correction != des$correction, alpha != des$alpha)) {
    comp   <- components_ma_norm(alpha, des$beta, correction, des$delta0,
                                 des$delta1, des$integer, des$K, des$power,
                                 des$ratio, des$sigma)
    comp$n <- des$n
    comp   <- components_ma_update(comp)
  }

  ##### Print summary ##########################################################

  if (summary) {
    summary_an_ma(des, pval, "norm")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Identifying which hypotheses to reject..")
  }
  an <- an_ma_norm_internal(comp, pval)
  if (summary) {
    message("..hypotheses to reject identified.")
    message("  Preparing for outputting..")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  list(alpha      = alpha,
       an         = an,
       correction = correction,
       des        = des,
       pval       = pval,
       summary    = summary)

}
