check_belong               <- function(value, name, allowed, len) {
  if (length(unique(value)) < length(value)) {
    stop("Elements of ", name, " must be unique")
  }
  if (!all(value %in% allowed)) {
    stop("Elements of ", name, " must be one of the following: ",
         paste(allowed, collapse = ", "))
  }
  if (all(is.finite(len), length(value) != len)) {
    stop(name, " must have length equal to ", len)
  }
}

check_default              <- function(condition, condition_name, value, name,
                                       default) {
  if (condition) {
    if (!all(value == default)) {
      warning(name, " has been changed from its default value, but this will ",
              "have no effect given the chosen value of ", condition_name)
    }
  }
}

check_CovZ                 <- function(CovZ, K, sigma, n, name_CovZ, name_K,
                                       name_sigma, name_n) {
  CovTau        <- matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
  diag_sqrt_I   <- diag(1/sqrt(diag(CovTau)))
  CovZ_internal <- diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  if (!all.equal(CovZ, CovZ_internal)) {
    stop(name_CovZ, " must correspond to the values of ", name_sigma, " and ",
         name_n)
  }
}

check_delta0_delta1        <- function(delta0, delta1, name_delta0, name_delta1,
                                       p0, name_p0) {
  if (missing(p0)) {
    if (any(length(delta1) != 1, delta1 <= 0, is.infinite(delta1))) {
      stop(name_delta1, " must be a single numeric that belongs to (0,Inf)")
    }
    if (any(length(delta0) != 1, delta0 >= delta1, is.infinite(delta0))) {
      stop(name_delta0, " must be a single numeric that belongs to (-Inf,",
           name_delta1, ")")
    }
  } else {
    if (any(length(delta1) != 1, delta1 <= 0, p0 + delta1 >= 1)) {
      stop(name_delta1, " must be a single numeric that belongs to (0,1 - ",
           name_p0, ") = (0,", 1 - p0, ")")
    }
    if (any(length(delta0) != 1, delta0 >= delta1, p0 + delta0 <= 0)) {
      stop(name_delta0, " must be a single numeric that belongs to (-", name_p0,
           ",", name_delta1, ") = (", -p0, ",", delta1, ")")
    }
  }
}

check_multiarm_des_ma      <- function(des, int = F) {
  if (!("multiarm_des_ma" %in% class(des))) {
    stop("des must be of class multiarm_des_ma")
  }
  des$K <- check_integer_range(des$K, "des$K", c(1, Inf), 1)
  check_real_range_strict(des$alpha, "des$alpha", c(0, 1), 1)
  check_real_range_strict(des$beta, "des$beta", c(0, 1), 1)
  check_delta0_delta1(des$delta0, des$delta1, "des$delta0", "des$delta1")
  check_sigma(des$sigma, des$K, "des$sigma", "des$K")
  check_n_N(des$n, des$N, des$K, "des$n", "des$N", "des$K", int)
  check_ratio(des$ratio, des$K, des$n, "des$ratio", "des$K", "des$n")
  check_belong(des$correction, "des$correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(des$power, "des$power",
               c("conjunctive", "disjunctive", "marginal"), 1)
  check_CovZ(des$CovZ, des$K, des$sigma, des$n, "des$CovZ", "des$K",
             "des$sigma", "des$n")
  check_gamma_gammaO(des$gamma, des$gammaO, des$K, des$correction, "des$gamma", "des$gammaO",
               "des$K", "des$correction")
  check_opchar(des$opchar, des$K, des$delta0, des$delta1, "des$opchar",
               "des$K", "des$delta0", "des$delta1")
}

check_multiarm_des_ma_bern <- function(des, int = F) {
  if (!("multiarm_des_ma_bern" %in% class(des))) {
    stop("des must be of class multiarm_des_ma_bern")
  }
  des$K <- check_integer_range(des$K, "des$K", c(1, Inf), 1)
  check_real_range_strict(des$alpha, "des$alpha", c(0, 1), 1)
  check_real_range_strict(des$beta, "des$beta", c(0, 1), 1)
  check_real_range_strict(des$pi0, "des$pi0", c(0, 1), 1)
  check_delta0_delta1(des$delta0, des$delta1, "des$delta0", "des$delta1",
                      des$pi0, "des$pi0")
  check_n_N(des$n, des$N, des$K, "des$n", "des$N", "des$K", int)
  check_ratio(des$ratio, des$K, des$n, "des$ratio", "des$K", "des$n")
  check_belong(des$ratio_scenario, "des$ratio_scenario", c("HG", "HA"), 1)
  check_belong(des$correction, "des$correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(des$power, "des$power",
               c("conjunctive", "disjunctive", "marginal"), 1)
  check_gamma_gammaO(des$gamma, des$gammaO, des$K, des$correction, "des$gamma",
                     "des$gammaO", "des$K", "des$correction")
  check_opchar_bern(des$opchar, des$K, des$pi0, des$delta0, des$delta1,
                    "des$opchar", "des$K", "des$pi0", "des$delta0",
                    "des$delta1")
}

check_integer_range        <- function(value, name, range, len) {
  check         <- F
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value%%1 != 0,
            value <= range[1], value >= range[2])) {
      check     <- T
      if (len == 1) {
        segment <- " must be a single integer that belongs to {"
      } else {
        segment <- paste("must be an integer vector of length", len,
                         "whose elements all belong to {")
      }
    }
  } else if (any(value%%1 != 0, !is.numeric(value), value <= range[1],
                 value >= range[2])) {
    check       <- T
    segment     <- " must be an integer vector whose elements all belong to {"
  }
  if (check) {
    if (range[1] + 2 == range[2]) {
      stop(name, " must be equal to ", range[1] + 1)
    } else if (range[1] + 3 == range[2]) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, "}")
    } else if (range[1] + 4 == range[2]) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, ", ", range[1] + 3,
           "}")
    } else if (all(is.infinite(range))) {
      stop(name, segment, "..., -1, 0, 1, ...}")
    } else if (is.infinite(range[1])) {
      stop(name, segment, "..., ", range[2] - 2, ", ", range[2] - 1, "}")
    } else if (is.infinite(range[2])) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, ", ...}")
    } else {
      stop(name, segment, range[1] + 1, ", ..., ", range[2] - 1, "}")
    }
  }
  return(as.integer(value))
}

check_logical              <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_n                    <- function(n, K, name_n, name_K, int) {
  if (int) {
    if (missing(K)) {
      if (any(!is.numeric(n), n < 1, n%%1 != 0, is.infinite(n))) {
        stop(name_n, " must be a numeric vector whose elements all belong to ",
             "{1, 2, ...}.")
      }
    } else {
      if (any(length(n) != K + 1L, !is.numeric(n), n < 1, n%%1 != 0,
              is.infinite(n))) {
        stop(name_n, " must be a numeric vector of length ", name_K, " + 1, ",
             "whose elements all belong to {1, 2, ...}.")
      }
    }
    as.integer(n)
  } else {
    if (missing(K)) {
      if (any(!is.numeric(n), n < 1, is.infinite(n))) {
        stop(name_n, " must be a numeric vector whose elements all belong to ",
             "(0, \u221E).")
      }
    } else {
      if (any(length(n) != K + 1L, !is.numeric(n), n < 1, is.infinite(n))) {
        stop(name_n, " must be a numeric vector of length ", name_K, " + 1, ",
             "whose elements all belong to (0, \u221E).")
      }
    }
  }
}

check_n_N                  <- function(n, N, K, name_n, name_N, name_K, int) {
  check_n(n, K, name_n, name_K, int)
  if (int) {
    check_integer_range(N, name_N, c(K, Inf), 1)
  } else {
    check_real_range_strict(N, name_N, c(0, Inf), 1)
  }
  if (!all.equal(sum(n), N)) {
    stop("The elements of ", name_n, " must add up to ", name_N)
  }
}

check_opchar               <- function(opchar, K, delta0, delta1, name_opchar,
                                       name_K, name_delta0, name_delta1) {
  if (any(nrow(opchar) != K + 2L, ncol(opchar) != 4*K + 8L)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_delta0,
         ", and ", name_delta1)
  }
  tau <- rbind(rep(0, K), rep(delta1, K), matrix(delta0, K, K) +
                                            (delta1 - delta0)*diag(K))
  if (any(tau - opchar[, 1:K] > 1e-10)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_delta0,
         ", and ", name_delta1)
  }
  if (any(tau[, -(1:K)] > 1, tau[, -(1:K)] < 0)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_delta0,
         ", and ", name_delta1)
  }
}

check_opchar_bern          <- function(opchar, K, pi0, delta0, delta1,
                                       name_opchar, name_K, name_pi0,
                                       name_delta0, name_delta1) {
  if (any(nrow(opchar) != K + 2L, ncol(opchar) != 4*K + 9L)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_pi0, ", ",
         name_delta0, ", and ", name_delta1)
  }
  pi <- rbind(rep(pi0, K + 1),
              c(pi0, rep(pi0 + delta1, K)),
              cbind(rep(pi0, K),
                    matrix(pi0 + delta0, K, K) + (delta1 - delta0)*diag(K)))
  if (any(pi - opchar[, 1:(K + 1)] > 1e-10)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_pi0, ", ",
         name_delta0, ", and ", name_delta1)
  }
  if (any(pi[, -(1:(K + 1))] > 1, pi[, -(1:(K + 1))] < 0)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_pi0, ", ",
         name_delta0, ", and ", name_delta1)
  }
}

check_gamma_gammaO         <- function(gamma, gammaO, K, correction, name_gamma,
                                       name_gammaO, name_K, name_correction) {
  if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
    if (!is.na(gammaO)) {
      warning("For the given value of ", name_correction, ", ", name_gammaO,
              " should in general be NA")
    }
    check_real_range_strict(gamma, name_gamma, c(0, 1), 1)
  } else {
    if (!is.na(gamma)) {
      warning("For the given value of ", name_correction, ", ", name_gamma,
              " should in general be NA")
    }
    check_real_range_strict(gammaO, name_gammaO, c(0, 1), K)
  }
}

check_pval                 <- function(pval, K, name_pval, name_K) {
  if (!is.numeric(pval)) {
    message(name_pval, " must be numeric")
  }
  if (is.vector(pval)) {
    pval <- matrix(pval, 1)
  }
  if (ncol(pval) != K) {
    stop(name_pval, " must have ", name_K, " columns")
  }
  if (any(pval < 0, pval > 1)) {
    stop("Values in ", name_pval, " must belong to (0,1)")
  }
  if (sum(duplicated(pval)) > 0) {
    warning("pval contains duplicated rows.")
  }
  pval
}

check_real_range           <- function(value, name, range, len) {
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value < range[1],
            value > range[2])) {
      if (len == 1) {
        stop(name, " must be a single numeric that belongs to [", range[1], ",",
             range[2], "]")
      } else {
        stop(name, " must be a numeric vector of length ", len, ", whose ",
             "elements all belong to [", range[1], ",", range[2], "]")
      }
    }
  } else {
    if (any(!is.numeric(value), value < range[1], value > range[2])) {
      stop(name, " must be a numeric vector whose elements all belong to [",
           range[1], ",", range[2], "]")
    }
  }
}

check_real_range_strict    <- function(value, name, range, len) {
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value <= range[1],
            value >= range[2])) {
      if (len == 1) {
        stop(name, " must be a single numeric that belongs to (", range[1], ",",
             range[2], ")")
      } else {
        stop(name, " must be a numeric vector of length ", len, ", whose ",
             "elements all belong to (", range[1], ",", range[2], ")")
      }
    }
  } else {
    if (any(!is.numeric(value), value <= range[1], value >= range[2])) {
      stop(name, " must be a numeric vector whose elements all belong to (",
           range[1], ",", range[2], ")")
    }
  }
}

check_ratio                <- function(ratio, K, n, name_ratio, name_K,
                                       name_n) {
  if (missing(n)) {
    if (is.character(ratio)) {
      if (any(length(ratio) > 1, !(ratio %in% c("A", "D", "E")))) {
        stop("ratio must be either \"A\", \"D\", \"E\", or a numeric vector of",
             " length K, whose elements all belong to (0, Inf).")
      }
    } else if (any(length(ratio) != K, !is.numeric(ratio), ratio <= 0,
                   is.infinite(ratio))) {
      stop("ratio must be either \"A\", \"D\", \"E\", or a numeric vector of ",
           "length K, whose elements all belong to (0, Inf).")
    }
  } else {
    if (!all.equal(ratio, n[-1]/n[1])) {
      stop(name_ratio, " must correspond to ", name_n)
    }
  }
}

check_sigma                <- function(sigma, K, name_sigma, name_K) {
  if (any(length(sigma) != K + 1L, !is.numeric(sigma), sigma <= 0,
          is.infinite(sigma))) {
    stop(name_sigma, " must be a numeric vector of length ", name_K, " + 1, ",
         "whose elements all belong to (0, \u221E).")
  }
}

check_sigmas               <- function(sigmas, pval) {
  if (is.vector(sigmas)) {
    sigmas <- t(matrix(sigmas))
  }
  if (nrow(sigmas) != nrow(pval)) {
    stop("If specified, sigmas must either be a vector of length equal to ",
         "des$K + 1, or a matrix of dimension nrow(pval) x (des$K + 1)")
  }
  if (ncol(sigmas) != ncol(pval) + 1) {
    stop("If specified, sigmas must either be a vector of length equal to ",
         "des$K + 1, or a matrix of dimension nrow(pval) x (des$K + 1)")
  }
}

check_pi                   <- function(pi, des) {
  if (missing(pi)) {
    pi <- rbind(rep(des$pi0, des$K + 1),
               c(des$pi0, rep(des$pi0 + des$delta1, des$K)),
               cbind(rep(des$pi0, des$K),
                     matrix(des$pi0 + des$delta0, des$K, des$K) +
                       (des$delta1 - des$delta0)*diag(des$K)))
  } else {
    if (!is.numeric(pi)) {
      stop("If specified, pi must be numeric.")
    } else if (any(pi < 0, pi > 1)) {
      stop("If specified, pi must contain only values in [0,1].")
    }
    if (is.vector(pi)) {
      pi <- matrix(pi, 1)
    }
    if (ncol(pi) != des$K + 1) {
      stop("If specified, pi must have des$K + 1 columns.")
    }
    if (sum(duplicated(pi)) > 0) {
      warning("pi contains duplicated rows.")
    }
  }
  pi
}

check_tau                  <- function(tau, des) {
  if (missing(tau)) {
    tau <- rbind(rep(0, des$K), rep(des$delta1, des$K),
                 matrix(des$delta0, des$K, des$K) +
                   (des$delta1 - des$delta0)*diag(des$K))
  } else {
    if (!is.numeric(tau)) {
      stop("If specified, tau must be numeric.")
    } else if (any(is.infinite(tau))) {
      stop("If specified, tau must contain only finite values.")
    }
    if (is.vector(tau)) {
      tau <- matrix(tau, 1)
    }
    if (ncol(tau) != des$K) {
      stop("If specified, tau must have des$K columns.")
    }
    if (sum(duplicated(tau)) > 0) {
      warning("tau contains duplicated rows.")
    }
  }
  tau
}
