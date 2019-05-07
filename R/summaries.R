summary_an_ma                 <- function(des, pval) {
  K <- des$K
  message("  ", rep("-", 79))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "analysis")
  message("  ", rep("-", 79))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("")
  if (des$correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(des$alpha, 4), ".")
  } else if (des$correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (des$correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (des$correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (des$correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (des$correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (des$correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " will be achieved using ", segment)
  }
  message("")
  if (K == 2L) {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p", uc_sub(1), ",p", uc_sub(2), ").")
  } else if (K == 3L) {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p", uc_sub(1), ",p", uc_sub(2), ",p",
            uc_sub(3), ").")
  } else {
    message("  The rejection decisions will be determined for ", nrow(pval),
            " possible values of (p", uc_sub(1), ",...,p", uc_sub(K), ").")
  }
}

summary_build_ma              <- function(K, n, alpha, beta, delta1, delta0,
                                          sigma, correction, power) {
  message("  ", rep("-", 60))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("")
  if (correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(alpha, 4), ".")
  } else if (correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(alpha, 4), " will be achieved using ", segment)
  }
  message("")
  if (power == "conjunctive") {
    message("  The trial will desire conjunctive  power of at least 1 - ",
            uc("beta"), " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2L) {
      message("    HA: ", uc("tau"), uc_sub(1), " = ", uc("tau"), uc_sub(2),
              " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
    } else {
      message("    HA: ", uc("tau"), uc_sub(1), " = ... = ", uc("tau"),
              uc_sub(K), " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4),
              ".")
    }
  } else if (power == "disjunctive") {
    message("  The trial will desire disjunctive power of at least 1 - ",
            uc("beta"), " = ", round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2L) {
      message("    HA: ", uc("tau"), uc_sub(1), " = ", uc("tau"), uc_sub(2),
              " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
    } else {
      message("    HA: ", uc("tau"), uc_sub(1), " = ... = ", uc("tau"),
              uc_sub(K), " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4),
              ".")
    }
  } else if (power == "marginal") {
    message("  The trial will desire marginal power of at least 1 - ",
            uc("beta"), " = ", round(1 - beta, 4), " to reject each of the ",
            "null hypotheses, under the LFC for each null hypothesis. The LFC ",
            "for H", uc_sub(1), ", LFC", uc_sub(1), ", is, e.g., given by:\n")
    if (K == 2L) {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ", uc("delta"), uc_sub(0), " = ", round(delta0, 4),
              ".")
    } else if (K == 3L) {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ", uc("tau"), uc_sub(3), " = ", uc("delta"),
              uc_sub(0), " = ", round(delta0, 4), ".")
    } else {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ... = ", uc("tau"), uc_sub(K), " = ", uc("delta"),
              uc_sub(0), " = ", round(delta0, 4), ".")
    }
  }
  message("\n  The standard deviation of the patient responses in each arm ",
          "will be assumed to be:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("")
  message("  The group size in each arm will be:")
  if (length(unique(n)) == 1L) {
    if (K == 2L) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = n", uc_sub(2), " = ",
              round(n[1], 4), ".")
    } else if (K %in% c(3L, 4L, 5L, 7L)) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(n[1], 4), ".")
    } else {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(n[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    n", uc_sub(0), " = ", round(n[1], 4), ", n", uc_sub(1),
              " = ", round(n[2], 4), ", n", uc_sub(2), " = ", round(n[3], 4),
              ".")
    } else {
      message("    n", uc_sub(0), " = ", round(n[1], 4), ", n", uc_sub(1),
              " = ", round(n[2], 4), ", ..., n", uc_sub(K), " = ",
              round(n[K + 1L], 4), ".")
    }
  }
}

summary_des_int_ma            <- function(K, N, alpha, beta, delta1, delta0,
                                          sigma, ratio, correction) {
  message("  ", rep("-", 80))
  message("  ", K, "-experimental treatment optimal constrained fixed-sample ",
          "multi-arm trial design")
  message("  ", rep("-", 80))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("")
  if (correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(alpha, 4), ".")
  } else if (correction == "benjamini-hochberg") {
    message("  The maximal FDR will be controlled to level ", uc("alpha"),
            " = ", round(alpha, 4),
            " using the Benjamini-Hochberg\n  procedure.")
  } else {
    if (correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(alpha, 4), " will be achieved using ", segment)
  }
  message("\n  The trial's power will be evaluated under the global ",
          "alternative, HA, given by:\n")
  if (K == 2L) {
    message("    HA: ", uc("tau"), uc_sub(1), " = ", uc("tau"), uc_sub(2),
            " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
  } else {
    message("   HA: ", uc("tau"), uc_sub(1), " = ... = ", uc("tau"), uc_sub(K),
            " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
  }
  message("\n  It will also be evaluated under the LFC for each null ",
          "hypothesis. The LFC for H", uc_sub(1), ",\n  LFC", uc_sub(1),
          " is, e.g., given by:\n")
  if (K == 2L) {
    message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
            uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
            uc_sub(2), " = ", uc("delta"), uc_sub(0), " = ", round(delta0, 4),
            ".")
  } else if (K == 3L) {
    message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
            uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
            uc_sub(2), " = ", uc("tau"), uc_sub(3), " = ", uc("delta"),
            uc_sub(0), " = ", round(delta0, 4), ".")
  } else {
    message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
            uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
            uc_sub(2), " = ... = ", uc("tau"), uc_sub(K), " = ", uc("delta"),
            uc_sub(0), " = ", round(delta0, 4), ".")
  }
  message("\n  For all error-rate calculations, the standard deviation of the ",
          "patient responses in\n  each arm will be assumed to be known:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("\n  The allocation ratios will be chosen as those which are ", ratio,
          "-optimal, constraining\n  the sample size across the control ",
          "and experimental arms to N = ", N, ".")
}

summary_des_ma                <- function(K, alpha, beta, delta1, delta0, sigma,
                                          ratio, correction, power, integer) {
  message("  ", rep("-", 60))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "design")
  message("  ", rep("-", 60))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("")
  if (correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(alpha, 4), ".")
  } else if (correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(alpha, 4), " will be achieved using ", segment)
  }
  message("")
  if (power == "conjunctive") {
    message("  The trial's total sample-size will be chosen so that the trial ",
            "has conjunctive\n  power of at least 1 - ", uc("beta"), " = ",
            round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2L) {
      message("    HA: ", uc("tau"), uc_sub(1), " = ", uc("tau"), uc_sub(2),
              " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
    } else {
      message("    HA: ", uc("tau"), uc_sub(1), " = ... = ", uc("tau"),
              uc_sub(K), " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4),
              ".")
    }
  } else if (power == "disjunctive") {
    message("  The trial's total sample-size will be chosen so that the trial ",
            "has disjunctive\n  power of at least 1 - ", uc("beta"), " = ",
            round(1 - beta, 4),
            " under the global alternative, HA, given by:\n")
    if (K == 2L) {
      message("    HA: ", uc("tau"), uc_sub(1), " = ", uc("tau"), uc_sub(2),
              " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4), ".")
    } else {
      message("    HA: ", uc("tau"), uc_sub(1), " = ... = ", uc("tau"),
              uc_sub(K), " = ", uc("delta"), uc_sub(1), " = ", round(delta1, 4),
              ".")
    }
  } else if (power == "marginal") {
    message("  The trial's total sample-size will be chosen so that the trial ",
            "has marginal power\n  of at least 1 - ", uc("beta"), " = ",
            round(1 - beta, 4), " to reject each of the null hypotheses, under",
            " the LFC for\n  each null hypothesis. The LFC for H", uc_sub(1),
            ", LFC", uc_sub(1), ", is, e.g., given by:\n")
    if (K == 2L) {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ", uc("delta"), uc_sub(0), " = ", round(delta0, 4),
              ".")
    } else if (K == 3L) {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ", uc("tau"), uc_sub(3), " = ", uc("delta"),
              uc_sub(0), " = ", round(delta0, 4), ".")
    } else {
      message("    LFC", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ",
              uc("delta"), uc_sub(1), " = ", round(delta1, 4), ", ", uc("tau"),
              uc_sub(2), " = ... = ", uc("tau"), uc_sub(K), " = ", uc("delta"),
              uc_sub(0), " = ", round(delta0, 4), ".")
    }
  }
  message("\n  For all error-rate calculations, the standard deviation of the ",
          "patient responses in\n  each arm will be assumed to be known:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("")
  if (integer) {
    message("  The sample sizes in each will be forced to be whole numbers.")
  } else {
    message("  The sample sizes in each will not be forced to be whole ",
            "numbers.")
  }
  message("")
  if (is.character(ratio)) {
    message("  The allocation ratios will be chosen as those which are ", ratio,
            "-optimal.")
  } else {
    message("  The (approximate) allocation ratios to each experimental arm ",
            "will be:\n")
    if (length(unique(ratio)) == 1L) {
      chars <- c("na", "third", "quarter", "fifth", "sixth", "na", "eigth")
      if (K == 2L) {
        message("    r", uc_sub(1), " = r", uc_sub(2), " = ", round(ratio, 3),
                ".")
      } else {
        message("    r", uc_sub(1), " = ... = r", uc_sub(K), " = ",
                round(ratio, 3), ".")
      }
    } else {
      if (K == 2L) {
        message("    r", uc_sub(1), " = ", round(ratio[1], 3), ", r", uc_sub(2),
                " = ", round(ratio[2], 3), ".")
      } else {
        message("    r", uc_sub(1), " = ", round(ratio[1], 3), ", ..., r",
                uc_sub(K), " = ", round(ratio[K], 3), ".")
      }
    }
  }
}

summary_opchar_ma             <- function(des, tau) {
  K <- des$K
  sigma <- des$sigma
  message("  ", rep("-", 79))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "operating characteristics")
  message("  ", rep("-", 79))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("\n  For all error-rate calculations, the standard deviation of the ",
          "patient responses in\n  each arm will be assumed to be known:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("")
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1L) {
    if (K == 2L) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = n", uc_sub(2), " = ",
              round(des$n[1], 4), ".")
    } else if (K %in% c(3L, 4L, 5L, 7L)) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    } else {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", n", uc_sub(2), " = ",
              round(des$n[3], 4), ".")
    } else {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", ..., n", uc_sub(K), " = ",
              round(des$n[K + 1L], 4), ".")
    }
  }
  message("")
  if (des$correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(des$alpha, 4), ".")
  } else if (des$correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (des$correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (des$correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (des$correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (des$correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (des$correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " will be achieved using ", segment)
  }
  message("")
  if (K == 2L) {
    message("  The operating characteristics will be evaluated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",", uc("tau"),
            uc_sub(2), ").")
  } else if (K == 3L) {
    message("  The operating characteristics will be evaluated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",", uc("tau"),
            uc_sub(2), ",", uc("tau"), uc_sub(3), ").")
  } else {
    message("  The operating characteristics will be evaluated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",...,", uc("tau"),
            uc_sub(K), ").")
  }
}

summary_plot_multiarm_des_ma  <- function(des, delta_min, delta_max, delta,
                                          density) {
  K     <- des$K
  sigma <- des$sigma
  message("  ", rep("-", 79))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "operating characteristics")
  message("  ", rep("-", 79))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("\n  For all error-rate calculations, the standard deviation of the ",
          "patient responses in\n  each arm will be assumed to be known:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("")
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1L) {
    if (K == 2L) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = n", uc_sub(2), " = ",
              round(des$n[1], 4), ".")
    } else if (K %in% c(3L, 4L, 5L, 7L)) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    } else {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", n", uc_sub(2), " = ",
              round(des$n[3], 4), ".")
    } else {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", ..., n", uc_sub(K), " = ",
              round(des$n[K + 1L], 4), ".")
    }
  }
  message("")
  if (des$correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(des$alpha, 4), ".")
  } else if (des$correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (des$correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (des$correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (des$correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (des$correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (des$correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " will be achieved using ", segment)
  }
  message("")
  message("  The operating characteristics will be evaluated over the range ",
          delta_min, " to ", delta_max, ", producing each line curve using ",
          density, " points and with a treatment effect shift of ", delta)
}

summary_sim_ma                <- function(des, tau, replicates) {
  K <- des$K
  sigma <- des$sigma
  message("  ", rep("-", 64))
  message("  ", K, "-experimental treatment fixed-sample multi-arm trial ",
          "simulation")
  message("  ", rep("-", 64))
  message("  The hypotheses to be tested will be:\n")
  if (K == 2L) {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, H",
            uc_sub(2), ": ", uc("tau"), uc_sub(2), " = ", uc("mu"), uc_sub(2),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  } else {
    message("    H", uc_sub(1), ": ", uc("tau"), uc_sub(1), " = ", uc("mu"),
            uc_sub(1), " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0, ..., H",
            uc_sub(K), ": ", uc("tau"), uc_sub(K), " = ", uc("mu"), uc_sub(K),
            " - ", uc("mu"), uc_sub(0), " ", uc("le"), " 0.")
  }
  message("\n  For all error-rate calculations, the standard deviation of the ",
          "patient responses in\n  each arm will be assumed to be known:\n")
  if (length(unique(sigma)) == 1L) {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", uc("sigma"), uc_sub(1),
              " = ", round(sigma[1], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ... = ", uc("sigma"),
              uc_sub(K), " = ", round(sigma[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4), ", ",
              uc("sigma"), uc_sub(1), " = ", round(sigma[2], 4), ".")
    } else {
      message("    ", uc("sigma"), uc_sub(0), " = ", round(sigma[1], 4),
              ", ..., ", uc("sigma"), uc_sub(K), " = ", round(sigma[K + 1], 4),
              ".")
    }
  }
  message("")
  message("  The group size in each arm will be:\n")
  if (length(unique(des$n)) == 1L) {
    if (K == 2L) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = n", uc_sub(2), " = ",
              round(des$n[1], 4), ".")
    } else if (K %in% c(3L, 4L, 5L, 7L)) {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    } else {
      message("    n", uc_sub(0), " = n", uc_sub(1), " = ... = n", uc_sub(K),
              " = ", round(des$n[1], 4), ".")
    }
  } else {
    if (K == 2L) {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", n", uc_sub(2), " = ",
              round(des$n[3], 4), ".")
    } else {
      message("    n", uc_sub(0), " = ", round(des$n[1], 4), ", n", uc_sub(1),
              " = ", round(des$n[2], 4), ", ..., n", uc_sub(K), " = ",
              round(des$n[K + 1L], 4), ".")
    }
  }
  message("")
  if (des$correction == "none") {
    message("  No multiplity correction will be applied. Each hypothesis will ",
            "be tested at\n  significance level ", uc("alpha"), " = ",
            round(des$alpha, 4), ".")
  } else if (des$correction == "benjamini_hochberg") {
    message("  The FDR will be controlled to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " using the Benjamini-Hochberg procedure.")
  } else {
    if (des$correction == "bonferroni") {
      segment <- "Bonferroni's\n  correction."
    } else if (des$correction == "dunnett") {
      segment <- "Dunnett's\n  correction."
    } else if (des$correction == "hochberg") {
      segment <- "Hochberg's\n  correction."
    } else if (des$correction == "holm_bonferroni") {
      segment <- "the\n  Holm-Bonferroni procedure."
    } else if (des$correction == "holm_sidak") {
      segment <- "the\n  Holm-\u0160id\u00e1k procedure."
    } else if (des$correction == "sidak") {
      segment <- "\u0160id\u00e1k's\n  correction."
    } else if (des$correction == "step_down_dunnett") {
      segment <- "the step-down\n  Dunnett correction."
    }
    message("  Strong control of the FWER to level ", uc("alpha"), " = ",
            round(des$alpha, 4), " will be achieved using ", segment)
  }
  message("")
  if (K == 2L) {
    message("  The operating characteristics will be estimated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",", uc("tau"),
            uc_sub(2), ").")
  } else if (K == 3L) {
    message("  The operating characteristics will be estimated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",", uc("tau"),
            uc_sub(2), ",", uc("tau"), uc_sub(3), ").")
  } else {
    message("  The operating characteristics will be estimated at ", nrow(tau),
            " possible values of (", uc("tau"), uc_sub(1), ",...,", uc("tau"),
            uc_sub(K), ").")
  }
  message("\n  ", replicates, " simulations will be used for each scenario.")
}

uc                            <- function(char) {
  lookup <- matrix(c("alpha",    "\u03B1",
                     "beta",     "\u03B2",
                     "gamma",    "\u03B3",
                     "delta",    "\u03B4",
                     "epsilon",  "\u03B5",
                     "zeta",     "\u03B6",
                     "eta",      "\u03B7",
                     "theta",    "\u03B8",
                     "iota",     "\u03B9",
                     "kappa",    "\u03BA",
                     "lambda",   "\u03BB",
                     "mu",       "\u03BC",
                     "nu",       "\u03BD",
                     "xi",       "\u03BE",
                     "omicron",  "\u03BF",
                     "pi",       "\u03C0",
                     "rho",      "\u03C1",
                     "sigma",    "\u03C3",
                     "tau",      "\u03C4",
                     "upsilon",  "\u03C5",
                     "phi",      "\u03C6",
                     "chi",      "\u03C7",
                     "psi",      "\u03C8",
                     "omega",    "\u03C9",
                     "Alpha",    "\u0391",
                     "Beta",     "\u0392",
                     "Gamma",    "\u0393",
                     "Delta",    "\u0394",
                     "Epsilon",  "\u0395",
                     "Zeta",     "\u0396",
                     "Eta",      "\u0397",
                     "Theta",    "\u0398",
                     "Iota",     "\u0399",
                     "Kappa",    "\u039A",
                     "Lambda",   "\u039B",
                     "Mu",       "\u039C",
                     "Nu",       "\u039D",
                     "Xi",       "\u039E",
                     "Omicron",  "\u039F",
                     "Pi",       "\u03A0",
                     "Rho",      "\u03A1",
                     "Sigma",    "\u03A3",
                     "Tau",      "\u03A4",
                     "Upsilon",  "\u03A5",
                     "Phi",      "\u03A6",
                     "Chi",      "\u03A7",
                     "Psi",      "\u03A8",
                     "Omega",    "\u03A9",
                     "le",       "\u2264",
                     "third",    "\u2153",
                     "quarter",  "\u00BC",
                     "fifth",    "\u2155",
                     "sixth",    "\u2159",
                     "eigth",    "\u215B",
                     "two_elip", "\u2026\u2026"), ncol = 2, byrow = T)
  lookup[which(lookup[, 1] == char), 2]
}

uc_sub                        <- function(n) {
  codes  <- c("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085",
              "\u2086", "\u2087", "\u2088", "\u2089")
  if (n < 10) {
    codes[n + 1]
  } else {
    paste(codes[n%/%10 + 1], codes[n%%10 + 1], sep = "")
  }
}
