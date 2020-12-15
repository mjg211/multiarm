summary_build_ss_bern        <- function(K, n, alpha, beta, pi0, delta1,
                                         delta0, correction, power) {
  message("  ", rep("-", 60))
  message("  ", K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be Bernoulli ",
          "distributed.")
  message("")
  message("  The hypotheses to be tested will be:\n")
  if (K == 2) {
    message("    H1 : tau1 = pi1 - pi0 <= 0, H2 : tau2 = pi2 - pi0 <= 0.\n")
  } else {
    message("    H1 : tau1 = pi1 - pi0 <= 0, ... , H", K, " : tau", K, " = pi",
            K, " - pi0 <= 0.\n")
  }
  if (correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(alpha, 4), ".")
  } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli")) {
    if (correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ", round(alpha, 4),
            " using the Benjamini-", sub_type, " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[correction]
    message("  Strong control of the FWER to level alpha = ", round(alpha, 4),
            " will be achieved using ", segment, "\n")
  }
  if (power == "conjunctive") {
    message("  The trial will desire conjunctive  power of at least 1 - beta",
            " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: pi0 = ", pi0, ", pi1 = pi2 = pi0 + delta1 = ",
              round(pi0 + delta1, 4), ".")
    } else {
      message("    HA: pi0 = ", pi0, ", pi1 = ... = pi", K,
              " = pi0 + delta1 = ", round(pi0 + delta1, 4), ".")
    }
  } else if (power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta",
            " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: pi0 = ", pi0, ", pi1 = pi2 = pi0 + delta1 = ",
              round(pi0 + delta1, 4), ".")
    } else {
      message("    HA: pi0 = ", pi0, ", pi1 = ... = pi", K,
              " = pi0 + delta1 = ", round(pi0 + delta1, 4), ".")
    }
  } else if (power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is, ",
            "e.g., given by:\n")
    if (K == 2) {
      message("    LFC1 : pi0 = ", pi0, ", pi1 = pi0 + delta1 = ",
              round(pi0 + delta1, 4), ", pi2 = pi0 + delta0 = ",
              round(pi0 + delta0, 4), ".\n")
    } else if (K == 3) {
      message("    LFC1 : pi0 = ", pi0, ", pi1 = pi0 + delta1 = ",
              round(pi0 + delta1, 4), ", pi2 = pi3 = pi0 + delta0 = ",
              round(pi0 + delta0, 4), ".\n")
    } else {
      message("    LFC1 : pi0 = ", pi0, ", pi1 = pi0 + delta1 = ",
              round(pi0 + delta1, 4), ", pi2 = ... = pi", K,
              " = pi0 + delta0 = ", round(pi0 + delta0, 4), ".\n")
    }
  }
  message("  The group size in each arm will be:")
  if (length(unique(n)) == 1) {
    if (K == 2) {
      message("    n0 = n1 = n2 = ", round(n[1], 4), ".")
    } else {
      message("    n0 = n1 = ... = n", K, " = ", round(n[1], 4), ".")
    }
  } else {
    if (K == 2) {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4), ", n2 = ",
              round(n[3], 4), ".")
    } else {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4),
              ", ..., n", K, " = ", round(n[K + 1], 4), ".")
    }
  }
}

summary_build_ss_norm        <- function(K, n, alpha, beta, delta1, delta0,
                                         sigma, correction, power) {
  message("  ", rep("-", 60))
  message("  ", K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be normally distributed.",
          "\n")
  message("  The hypotheses to be tested will be:\n")
  if (K == 2) {
    message("    H1 : tau1 = mu1 - mu0 <= 0, H2 : tau2 = mu2 - mu0 <= 0.\n")
  } else {
    message("    H1 : tau1 = mu1 - mu0 <= 0, ... , H", K, " : tau", K, " = mu",
            K, " - mu0 <= 0.\n")
  }
  if (correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(alpha, 4), ".")
  } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli")) {
    if (correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ", round(alpha, 4),
            " using the Benjamini-", sub_type, " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[correction]
    message("  Strong control of the FWER to level alpha = ", round(alpha, 4),
            " will be achieved using ", segment, "\n")
  }
  if (power == "conjunctive") {
    message("  The trial will desire conjunctive power of at least 1 - beta = ",
            round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: tau1 = tau2 = delta1 = ", round(delta1, 4), ".")
    } else {
      message("    HA: tau1 = ... = tau", K, " = delta1 = ", round(delta1, 4),
              ".")
    }
  } else if (power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta = ",
            round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: tau1 = tau2 = delta1 = ", round(delta1, 4), ".")
    } else {
      message("    HA: tau1 = ... = tau", K, " = delta1 = ", round(delta1, 4),
              ".")
    }
  } else if (power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is, ",
            "e.g., given by:\n")
    if (K == 2) {
      message("    LFC1 : tau1 = delta1 = ", round(delta1, 4), ", tau2 = delta",
              "0 = ", round(delta0, 4), ".")
    } else if (K == 3) {
      message("    LFC1 : tau1 = delta1 = ", round(delta1, 4),
              ", tau2 = tau3 = delta0 = ", round(delta0, 4), ".")
    } else {
      message("    LFC1 : tau1 = delta1 = ", round(delta1, 4),
              ", tau2 = ... = tau", K, " = delta0 = ", round(delta0, 4), ".")
    }
  }
  message("\n  The standard deviation of the patient responses in each arm ",
          "will be assumed to be:\n")
  if (length(unique(sigma)) == 1) {
    if (K == 2) {
      message("    sigma0 = sigma1 = ", round(sigma[1], 4), ".")
    } else {
      message("    sigma0 = ... = sigma", K, " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2) {
      message("    sigma0 = ", round(sigma[1], 4), ", sigma1 = ",
              round(sigma[2], 4), ".\n")
    } else {
      message("    sigma0 = ", round(sigma[1], 4), ", ..., sigma", K, " = ",
              round(sigma[K + 1], 4), ".\n")
    }
  }
  message("  The group size in each arm will be:")
  if (length(unique(n)) == 1) {
    if (K == 2) {
      message("    n0 = n1 = n2 = ", round(n[1], 4), ".")
    } else {
      message("    n0 = n1 = ... = n", K, " = ", round(n[1], 4), ".")
    }
  } else {
    if (K == 2) {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4), ", n2 = ",
              round(n[3], 4), ".")
    } else {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4),
              ", ..., n", K, " = ", round(n[K + 1], 4), ".")
    }
  }
}

summary_build_ss_pois        <- function(K, n, alpha, beta, lambda0, delta1,
                                         delta0, correction, power) {
  message("  ", rep("-", 60))
  message("  ", K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be Poisson ",
          "distributed.")
  message("")
  message("  The hypotheses to be tested will be:\n")
  if (K == 2) {
    message("    H1 : tau1 = lambda1 - lambda0 <= 0, H2 : tau2 = lambda2 - ",
            "lambda0 <= 0.\n")
  } else {
    message("    H1 : tau1 = lambda1 - lambda0 <= 0, ... , H", K, " : tau", K,
            " = lambda", K, " - lambda0 <= 0.\n")
  }
  if (correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(alpha, 4), ".")
  } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli")) {
    if (correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ", round(alpha, 4),
            " using the Benjamini-", sub_type, " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[correction]
    message("  Strong control of the FWER to level alpha = ", round(alpha, 4),
            " will be achieved using ", segment, "\n")
  }
  if (power == "conjunctive") {
    message("  The trial will desire conjunctive  power of at least 1 - beta",
            " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: lambda0 = ", lambda0, ", lambda1 = lambda2 = ",
              "lambda0 + delta1 = ", round(lambda0 + delta1, 4), ".")
    } else {
      message("    HA: lambda0 = ", lambda0, ", lambda1 = ... = lambda", K,
              " = lambda0 + delta1 = ", round(lambda0 + delta1, 4), ".")
    }
  } else if (power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta",
            " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2) {
      message("    HA: lambda0 = ", lambda0, ", lambda1 = lambda2 = ",
              "lambda0 + delta1 = ", round(lambda0 + delta1, 4), ".")
    } else {
      message("    HA: lambda0 = ", lambda0, ", lambda1 = ... = lambda", K,
              " = lambda0 + delta1 = ", round(lambda0 + delta1, 4), ".")
    }
  } else if (power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is, ",
            "e.g., given by:\n")
    if (K == 2) {
      message("    LFC1 : lambda0 = ", lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(lambda0 + delta1, 4), ", lambda2 = lambda0 + delta0 = ",
              round(lambda0 + delta0, 4), ".\n")
    } else if (K == 3) {
      message("    LFC1 : lambda0 = ", lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(lambda0 + delta1, 4),
              ", lambda2 = lambda3 = lambda0 + delta0 = ",
              round(lambda0 + delta0, 4), ".\n")
    } else {
      message("    LFC1 : lambda0 = ", lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(lambda0 + delta1, 4), ", lambda2 = ... = lambda", K,
              " = lambda0 + delta0 = ", round(lambda0 + delta0, 4), ".\n")
    }
  }
  message("  The group size in each arm will be:")
  if (length(unique(n)) == 1) {
    if (K == 2) {
      message("    n0 = n1 = n2 = ", round(n[1], 4), ".")
    } else {
      message("    n0 = n1 = ... = n", K, " = ", round(n[1], 4), ".")
    }
  } else {
    if (K == 2) {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4), ", n2 = ",
              round(n[3], 4), ".")
    } else {
      message("    n0 = ", round(n[1], 4), ", n1 = ", round(n[2], 4),
              ", ..., n", K, " = ", round(n[K + 1], 4), ".")
    }
  }
}

summary_des_ss_bern          <- function(comp) {
  message("  ", rep("-", 60))
  message("  ", comp$K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be Bernoulli ",
          "distributed.")
  message("")
  message("  The hypotheses to be tested will be:\n")
  if (comp$K == 2) {
    message("    H1 : tau1 = pi1 - pi0 <= 0, H2 : tau2 = pi2 - pi0 <= 0.\n")
  } else {
    message("    H1 : tau1 = pi1 - pi0 <= 0, ... , H", comp$K, " : tau", comp$K, " = pi",
            comp$K, " - pi0 <= 0.\n")
  }
  if (comp$correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(comp$alpha, 4), ".")
  } else if (comp$correction %in% c("benjamini_hochberg", "benjamini_yekutieli")) {
    if (comp$correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ", round(comp$alpha, 4),
            " using the Benjamini-", sub_type, " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[comp$correction]
    message("  Strong control of the FWER to level alpha = ", round(comp$alpha, 4),
            " will be achieved using ", segment, "\n")
  }
  if (comp$power == "conjunctive") {
    message("  The trial will desire conjunctive  power of at least 1 - beta",
            " = ", round(1 - comp$beta, 4),
            " under the global alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: pi0 = ", comp$pi0, ", pi1 = pi2 = pi0 + delta1 = ",
              round(comp$pi0 + comp$delta1, 4), ".")
    } else {
      message("    HA: pi0 = ", comp$pi0, ", pi1 = ... = pi", comp$K,
              " = pi0 + delta1 = ", round(comp$pi0 + comp$delta1, 4), ".")
    }
  } else if (comp$power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta",
            " = ", round(1 - comp$beta, 4),
            " under the global alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: pi0 = ", comp$pi0, ", pi1 = pi2 = pi0 + delta1 = ",
              round(comp$pi0 + comp$delta1, 4), ".")
    } else {
      message("    HA: pi0 = ", comp$pi0, ", pi1 = ... = pi", comp$K,
              " = pi0 + delta1 = ", round(comp$pi0 + comp$delta1, 4), ".")
    }
  } else if (comp$power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - comp$beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is, ",
            "e.g., given by:\n")
    if (comp$K == 2) {
      message("    LFC1 : pi0 = ", comp$pi0, ", pi1 = pi0 + delta1 = ",
              round(comp$pi0 + comp$delta1, 4), ", pi2 = pi0 + delta0 = ",
              round(comp$pi0 + comp$delta0, 4), ".\n")
    } else if (comp$K == 3) {
      message("    LFC1 : pi0 = ", comp$pi0, ", pi1 = pi0 + delta1 = ",
              round(comp$pi0 + comp$delta1, 4), ", pi2 = pi3 = pi0 + delta0 = ",
              round(comp$pi0 + comp$delta0, 4), ".\n")
    } else {
      message("    LFC1 : pi0 = ", comp$pi0, ", pi1 = pi0 + delta1 = ",
              round(comp$pi0 + comp$delta1, 4), ", pi2 = ... = pi", comp$K,
              " = pi0 + delta0 = ", round(comp$pi0 + comp$delta0, 4), ".\n")
    }
  }
  if (comp$integer) {
    message("  The sample sizes in each will be forced to be whole numbers.\n")
  } else {
    message("  The sample sizes in each will not be forced to be whole ",
            "numbers.\n")
  }
  if (is.character(comp$ratio)) {
    message("  The allocation ratios will be chosen as those which are ", comp$ratio,
            "-optimal, under ", comp$ratio_scenario, ".")
  } else {
    message("  The (approximate) allocation ratios to each experimental arm ",
            "will be:\n")
    if (length(unique(comp$ratio)) == 1) {
      if (comp$K == 2) {
        message("    r1 = r2 = ", round(comp$ratio, 3), ".")
      } else {
        message("    r1 = ... = r", comp$K, " = ", round(comp$ratio, 3), ".")
      }
    } else {
      if (comp$K == 2) {
        message("    r1 = ", round(comp$ratio[1], 3), ", r2 = ",
                round(comp$ratio[2], 3), ".")
      } else {
        message("    r1 = ", round(comp$ratio[1], 3), ", ..., r", comp$K,
                " = ", round(comp$ratio[comp$K], 3), ".")
      }
    }
  }
}

summary_des_ss_norm          <- function(comp) {
  message("  ", rep("-", 60))
  message("  ", comp$K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be normally distributed.",
          "\n")
  message("  The hypotheses to be tested will be:\n")
  if (comp$K == 2) {
    message("    H1 : tau1 = mu1 - mu0 <= 0, H2 : tau2 = mu2 - mu0 <= 0.\n")
  } else {
    message("    H1 : tau1 = mu1 - mu0 <= 0, ... , H", comp$K, " : tau", comp$K,
            " = mu", comp$K, " - mu0 <= 0.\n")
  }
  if (comp$correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(comp$alpha, 4), ".")
  } else if (comp$correction %in% c("benjamini_hochberg",
                                    "benjamini_yekutieli")) {
    if (comp$correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ",
            round(comp$alpha, 4), " using the Benjamini-", sub_type,
            " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[comp$correction]
    message("  Strong control of the FWER to level alpha = ",
            round(comp$alpha, 4), " will be achieved using ", segment, "\n")
  }
  if (comp$power == "conjunctive") {
    message("  The trial will desire conjunctive power of at least 1 - beta = ",
            round(1 - comp$beta, 4),
            " under the global\n  alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: tau1 = tau2 = delta1 = ", round(comp$delta1, 4), ".")
    } else {
      message("    HA: tau1 = ... = tau", comp$K, " = delta1 = ",
              round(comp$delta1, 4), ".")
    }
  } else if (comp$power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta = ",
            round(1 - comp$beta, 4),
            " under the global\n  alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: tau1 = tau2 = delta1 = ", round(comp$delta1, 4), ".")
    } else {
      message("    HA: tau1 = ... = tau", comp$K, " = delta1 = ",
              round(comp$delta1, 4), ".")
    }
  } else if (comp$power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - comp$beta, 4),
            " to reject each of the\n  null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is,\n  ",
            "e.g., given by:\n")
    if (comp$K == 2) {
      message("    LFC1 : tau1 = delta1 = ", round(comp$delta1, 4),
              ", tau2 = delta0 = ", round(comp$delta0, 4), ".")
    } else if (comp$K == 3) {
      message("    LFC1 : tau1 = delta1 = ", round(comp$delta1, 4),
              ", tau2 = tau3 = delta0 = ", round(comp$delta0, 4), ".")
    } else {
      message("    LFC1 : tau1 = delta1 = ", round(comp$delta1, 4),
              ", tau2 = ... = tau", comp$K, " = delta0 = ", round(comp$delta0, 4),
              ".")
    }
  }
  message("\n  The standard deviation of the patient responses in each arm ",
          "will be assumed to be:\n")
  if (length(unique(comp$sigma)) == 1) {
    if (comp$K == 2) {
      message("    sigma0 = sigma1 = ", round(comp$sigma[1], 4), ".\n")
    } else {
      message("    sigma0 = ... = sigma", comp$K, " = ", round(comp$sigma[1], 4),
              ".\n")
    }
  } else {
    if (comp$K == 2) {
      message("    sigma0 = ", round(comp$sigma[1], 4), ", sigma1 = ",
              round(comp$sigma[2], 4), ".\n")
    } else {
      message("    sigma0 = ", round(comp$sigma[1], 4), ", ..., sigma", comp$K,
              " = ", round(comp$sigma[comp$K + 1], 4), ".\n")
    }
  }
  if (comp$integer) {
    message("  The sample sizes in each will be forced to be whole numbers.")
  } else {
    message("  The sample sizes in each will not be forced to be whole ",
            "numbers.")
  }
  message("")
  if (is.character(comp$ratio)) {
    message("  The allocation ratios will be chosen as those which are ",
            comp$ratio, "-optimal.")
  } else {
    message("  The (approximate) allocation ratios to each experimental arm ",
            "will be:\n")
    if (length(unique(comp$ratio)) == 1) {
      if (comp$K == 2) {
        message("    r1 = r2 = ", round(comp$ratio, 3), ".")
      } else {
        message("    r1 = ... = r", comp$K, " = ", round(comp$ratio, 3), ".")
      }
    } else {
      if (comp$K == 2) {
        message("    r1 = ", round(comp$ratio[1], 3), ", r2 = ",
                round(comp$ratio[2], 3), ".")
      } else {
        message("    r1 = ", round(comp$ratio[1], 3), ", ..., r", comp$K,
                " = ", round(comp$ratio[comp$K], 3), ".")
      }
    }
  }
}

summary_des_ss_pois          <- function(comp) {
  message("  ", rep("-", 60))
  message("  ", comp$K, "-experimental treatment single-stage multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The outcome variables will be assumed to be Poisson ",
          "distributed.")
  message("")
  message("  The hypotheses to be tested will be:\n")
  if (comp$K == 2) {
    message("    H1 : tau1 = lambda1 - lambda0 <= 0, ",
            "H2 : tau2 = lambda2 - lambda0 <= 0.\n")
  } else {
    message("    H1 : tau1 = lambda1 - lambda0 <= 0, ... , H", comp$K, " : tau",
            comp$K, " = lambda", comp$K, " - lambda0 <= 0.\n")
  }
  if (comp$correction == "none") {
    message("  No multiple comparison correction will be applied. Each ",
            "hypothesis will be tested at\n  significance level alpha = ",
            round(comp$alpha, 4), ".")
  } else if (comp$correction %in% c("benjamini_hochberg", "benjamini_yekutieli")) {
    if (comp$correction == "benjamini_hochberg") {
      sub_type <- "Hochberg"
    } else {
      sub_type <- "Yekutieli"
    }
    message("  The FDR will be controlled to level alpha = ",
            round(comp$alpha, 4), " using the Benjamini-", sub_type,
            " procedure.")
  } else {
    segment   <- c("bonferroni"        = "Bonferroni's\n  correction.",
                   "dunnett"           = "Dunnett's\n  correction.",
                   "hochberg"          = "Hochberg's\n  correction.",
                   "holm_bonferroni"   = "the\n  Holm-Bonferroni procedure.",
                   "holm_sidak"        = "the\n  Holm-Sidak procedure.",
                   "sidak"             = "Sidak's correction.",
                   "step_down_dunnett" =
                     "the step-down\n  Dunnett correction.")[comp$correction]
    message("  Strong control of the FWER to level alpha = ", round(comp$alpha, 4),
            " will be achieved using ", segment, "\n")
  }
  if (comp$power == "conjunctive") {
    message("  The trial will desire conjunctive power of at least 1 - beta",
            " = ", round(1 - comp$beta, 4),
            " under the global alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: lambda0 = ", comp$lambda0,
              ", lambda1 = lambda2 = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4), ".")
    } else {
      message("    HA: lambda0 = ", comp$lambda0, ", lambda1 = ... = lambda",
              comp$K, " = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4), ".")
    }
  } else if (comp$power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - beta",
            " = ", round(1 - comp$beta, 4),
            " under the global alternative, HA, given by:\n")
    if (comp$K == 2) {
      message("    HA: lambda0 = ", comp$lambda0,
              ", lambda1 = lambda2 = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4), ".")
    } else {
      message("    HA: lambda0 = ", comp$lambda0, ", lambda1 = ... = lambda",
              comp$K, " = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4), ".")
    }
  } else if (comp$power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - beta = ",
            round(1 - comp$beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for each null hypothesis. The LFC for H1, LFC1, is, ",
            "e.g., given by:\n")
    if (comp$K == 2) {
      message("    LFC1 : lambda0 = ", comp$lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4),
              ", lambda2 = lambda0 + delta0 = ",
              round(comp$lambda0 + comp$delta0, 4), ".\n")
    } else if (comp$K == 3) {
      message("    LFC1 : lambda0 = ", comp$lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4),
              ", lambda2 = lambda3 = lambda0 + delta0 = ",
              round(comp$lambda0 + comp$delta0, 4), ".\n")
    } else {
      message("    LFC1 : lambda0 = ", comp$lambda0,
              ", lambda1 = lambda0 + delta1 = ",
              round(comp$lambda0 + comp$delta1, 4), ", lambda2 = ... = lambda",
              comp$K, " = lambda0 + delta0 = ",
              round(comp$lambda0 + comp$delta0, 4), ".\n")
    }
  }
  if (comp$integer) {
    message("  The sample sizes in each will be forced to be whole numbers.\n")
  } else {
    message("  The sample sizes in each will not be forced to be whole ",
            "numbers.\n")
  }
  if (is.character(comp$ratio)) {
    message("  The allocation ratios will be chosen as those which are ", comp$ratio,
            "-optimal, under ", comp$ratio_scenario, ".")
  } else {
    message("  The (approximate) allocation ratios to each experimental arm ",
            "will be:\n")
    if (length(unique(comp$ratio)) == 1) {
      if (comp$K == 2) {
        message("    r1 = r2 = ", round(comp$ratio, 3), ".")
      } else {
        message("    r1 = ... = r", comp$K, " = ", round(comp$ratio, 3), ".")
      }
    } else {
      if (comp$K == 2) {
        message("    r1 = ", round(comp$ratio[1], 3), ", r2 = ",
                round(comp$ratio[2], 3), ".")
      } else {
        message("    r1 = ", round(comp$ratio[1], 3), ", ..., r", comp$K,
                " = ", round(comp$ratio[comp$K], 3), ".")
      }
    }
  }
}

summary_opchar_ss            <- function(des, tau_pi_lambda, type) {
  message("  ", rep("-", 79))
  message("  ", des$K, "-experimental treatment single-stage multi-arm trial ",
          "operating characteristics")
  message("  ", rep("-", 79))
  if (type == "norm") {
    message("  The outcome variables will be assumed to be normally ",
            "distributed.\n")
    text <- "mu"
  } else if (type == "bern") {
    message("  The outcome variables will be assumed to be Bernoulli ",
            "distributed.\n")
    text <- "pi"
  } else if (type == "pois") {
    message("  The outcome variables will be assumed to be Poisson ",
            "distributed.\n")
    text <- "lambda"
  }
  message("  The hypotheses to be tested will be:\n")
  if (des$K == 2) {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, H2 : tau2 = ",
            text, "2 - ", text, "0 <= 0.\n")
  } else {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, ... , H", des$K,
            " : tau", des$K, " = ", text, des$K, " - ", text, "0 <= 0.\n")
  }
  if (type == "norm") {
    message("\n  For all error-rate calculations, the standard deviation of ",
            "the patient responses in\n  each arm will be assumed to be ",
            "known:\n")
    if (length(unique(des$sigma)) == 1) {
      if (des$K == 2) {
        message("    sigma0 = sigma1 = ", round(des$sigma[1], 4), ".")
      } else {
        message("    sigma0 = ... = sigma", des$K, " = ",
                round(des$sigma[1], 4), ".")
      }
    } else {
      if (des$K == 2) {
        message("    sigma0 = ", round(des$sigma[1], 4), ", sigma1 = ",
                round(des$sigma[2], 4), ".\n")
      } else {
        message("    sigma0 = ", round(des$sigma[1], 4), ", ..., sigma", des$K,
                " = ", round(des$sigma[des$K + 1], 4), ".\n")
      }
    }
  }
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1) {
    if (des$K == 2) {
      message("    n0 = n1 = n2 = ", round(des$n[1], 4), ".\n")
    } else {
      message("    n0 = n1 = ... = n", des$K, " = ", round(des$n[1], 4), ".\n")
    }
  } else {
    if (des$K == 2) {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", n2 = ", round(des$n[3], 4), ".\n")
    } else {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", ..., n", des$K, " = ", round(des$n[des$K + 1], 4), ".\n")
    }
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
  if (type == "norm") {
    text <- "tau"
  }
  if (des$K == 2) {
    message("  The operating characteristics will be evaluated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,", text,
            "2).")
  } else if (des$K == 3) {
    message("  The operating characteristics will be evaluated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,", text,
            "2,", text, "3).")
  } else {
    message("  The operating characteristics will be evaluated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,...,", text,
            des$K, ").")
  }
}

summary_plot_multiarm_des_ss <- function(des, delta_min, delta_max, delta,
                                         density, type) {
  message("  ", rep("-", 79))
  message("  ", des$K, "-experimental treatment single-stage multi-arm trial ",
          "operating characteristics")
  message("  ", rep("-", 79))
  if (type == "norm") {
    message("  The outcome variables will be assumed to be normally ",
            "distributed.\n")
    text <- "mu"
  } else if (type == "bern") {
    message("  The outcome variables will be assumed to be Bernoulli ",
            "distributed.\n")
    text <- "pi"
  } else if (type == "pois") {
    message("  The outcome variables will be assumed to be Poisson ",
            "distributed.\n")
    text <- "lambda"
  }
  message("  The hypotheses to be tested will be:\n")
  if (des$K == 2) {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, H2 : tau2 = ",
            text, "2 - ", text, "0 <= 0.\n")
  } else {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, ... , H", des$K,
            " : tau", des$K, " = ", text, des$K, " - ", text, "0 <= 0.\n")
  }
  if (type == "norm") {
    message("\n  For all error-rate calculations, the standard deviation of ",
            "the patient responses in\n  each arm will be assumed to be ",
            "known:\n")
    if (length(unique(des$sigma)) == 1) {
      if (des$K == 2) {
        message("    sigma0 = sigma1 = ", round(des$sigma[1], 4), ".")
      } else {
        message("    sigma0 = ... = sigma", des$K, " = ",
                round(des$sigma[1], 4), ".")
      }
    } else {
      if (des$K == 2) {
        message("    sigma0 = ", round(des$sigma[1], 4), ", sigma1 = ",
                round(des$sigma[2], 4), ".\n")
      } else {
        message("    sigma0 = ", round(des$sigma[1], 4), ", ..., sigma", des$K,
                " = ", round(des$sigma[des$K + 1], 4), ".\n")
      }
    }
  }
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1) {
    if (des$K == 2) {
      message("    n0 = n1 = n2 = ", round(des$n[1], 4), ".\n")
    } else {
      message("    n0 = n1 = ... = n", des$K, " = ", round(des$n[1], 4), ".\n")
    }
  } else {
    if (des$K == 2) {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", n2 = ", round(des$n[3], 4), ".\n")
    } else {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", ..., n", des$K, " = ", round(des$n[des$K + 1], 4), ".\n")
    }
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
  message("  The operating characteristics will be evaluated for treatment ",
          "effects over the range ", delta_min, " to ", delta_max,
          ", producing each line curve using ", density, " points and with a",
          " treatment effect shift of ", delta)
}

summary_sim_ss               <- function(des, tau_pi_lambda, replicates, type) {
  message("  ", rep("-", 64))
  message("  ", des$K, "-experimental treatment single-stage multi-arm trial ",
          "simulation")
  message("  ", rep("-", 64))
  if (type == "norm") {
    message("  The outcome variables will be assumed to be normally ",
            "distributed.\n")
    text       <- "mu"
  } else if (type == "bern") {
    message("  The outcome variables will be assumed to be Bernoulli ",
            "distributed.\n")
    text       <- "pi"
  } else if (type == "pois") {
    message("  The outcome variables will be assumed to be Poisson ",
            "distributed.\n")
    text       <- "lambda"
  }
  message("  The hypotheses to be tested will be:\n")
  if (des$K == 2) {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, H2 : tau2 = ",
            text, "2 - ", text, "0 <= 0.\n")
  } else {
    message("    H1 : tau1 = ", text, "1 - ", text, "0 <= 0, ... , H", des$K,
            " : tau", des$K, " = ", text, des$K, " - ", text, "0 <= 0.\n")
  }
  if (type == "norm") {
    message("\n  For all error-rate calculations, the standard deviation of ",
            "the patient responses in\n  each arm will be assumed to be ",
            "known:\n")
    if (length(unique(des$sigma)) == 1) {
      if (des$K == 2) {
        message("    sigma0 = sigma1 = ", round(des$sigma[1], 4), ".")
      } else {
        message("    sigma0 = ... = sigma", des$K, " = ",
                round(des$sigma[1], 4), ".")
      }
    } else {
      if (des$K == 2) {
        message("    sigma0 = ", round(des$sigma[1], 4), ", sigma1 = ",
                round(des$sigma[2], 4), ".\n")
      } else {
        message("    sigma0 = ", round(des$sigma[1], 4), ", ..., sigma", des$K,
                " = ", round(des$sigma[des$K + 1], 4), ".\n")
      }
    }
  }
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1) {
    if (des$K == 2) {
      message("    n0 = n1 = n2 = ", round(des$n[1], 4), ".\n")
    } else {
      message("    n0 = n1 = ... = n", des$K, " = ", round(des$n[1], 4), ".\n")
    }
  } else {
    if (des$K == 2) {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", n2 = ", round(des$n[3], 4), ".\n")
    } else {
      message("    n0 = ", round(des$n[1], 4), ", n1 = ", round(des$n[2], 4),
              ", ..., n", des$K, " = ", round(des$n[des$K + 1], 4), ".\n")
    }
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
    message("  The operating characteristics will be estimated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,", text,
            "2).")
  } else if (des$K == 3) {
    message("  The operating characteristics will be estimated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,", text,
            "2,", text,
            "3).")
  } else {
    message("  The operating characteristics will be estimated at ",
            nrow(tau_pi_lambda), " possible values of (", text, "1,...,", text,
            des$K, ").")
  }
  message("\n  ", replicates, " simulations will be used for each scenario.")
}
