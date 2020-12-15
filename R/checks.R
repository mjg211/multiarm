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

check_boundaries           <- function(fshape, eshape, ffix, efix, fshape_name,
                                       eshape_name, ffix_name, efix_name) {
  check_belong(fshape, fshape_name,
               c("fixed", "obf", "pocock", "triangular"), 1)
  check_belong(eshape, eshape_name,
               c("fixed", "obf", "pocock", "triangular"), 1)
  if (fshape == "fixed") {
    check_real_range_strict(ffix, ffix_name, c(-Inf,Inf), 1)
  } else {
    if (ffix != -3) {
      warning(ffix_name, " has been changed from default, but this will have ",
              "no effect given the value of ", fshape_name)
    }
  }
  if (eshape == "fixed") {
    check_real_range_strict(efix, efix_name, c(-Inf,Inf), 1)
  } else {
    if (efix != 3) {
      warning(efix_name, " has been changed from default, but this will have ",
              "no effect given the value of ", eshape_name)
    }
  }
  if (all(c(fshape, eshape) == "fixed")) {
    if (efix <= ffix) {
      stop(efix_name, " must be strictly smaller than ", ffix_name)
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

check_default              <- function(condition, condition_name, value, name,
                                       default) {
  if (condition) {
    if (!all(value == default)) {
      warning(name, " has been changed from its default value, but this will ",
              "have no effect given the chosen value of ", condition_name)
    }
  }
}

check_delta0_delta1        <- function(delta0, delta1, name_delta0, name_delta1,
                                       pi0, name_pi0, lambda0, name_lambda0) {
  if (all(missing(pi0), missing(lambda0))) {
    if (any(length(delta1) != 1, delta1 <= 0, is.infinite(delta1))) {
      stop(name_delta1, " must be a single numeric that belongs to (0,Inf)")
    }
    if (any(length(delta0) != 1, delta0 >= delta1, is.infinite(delta0))) {
      stop(name_delta0, " must be a single numeric that belongs to (-Inf,",
           name_delta1, ")")
    }
  } else if (missing(lambda0)) {
    if (any(length(delta1) != 1, delta1 <= 0, pi0 + delta1 >= 1)) {
      stop(name_delta1, " must be a single numeric that belongs to (0,1 - ",
           name_pi0, ") = (0,", 1 - pi0, ")")
    }
    if (any(length(delta0) != 1, delta0 >= delta1, pi0 + delta0 <= 0)) {
      stop(name_delta0, " must be a single numeric that belongs to (-", name_pi0,
           ",", name_delta1, ") = (", -pi0, ",", delta1, ")")
    }
  } else if (missing(pi0)) {
    if (any(length(delta1) != 1, delta1 <= 0, is.infinite(delta1))) {
      stop(name_delta1, " must be a single numeric that belongs to (0,Inf)")
    }
    if (any(length(delta0) != 1, delta0 >= delta1, lambda0 + delta0 <= 0)) {
      stop(name_delta0, " must be a single numeric that belongs to (-",
           name_lambda0, ",", name_delta1, ") = (", -lambda0, ",", delta1, ")")
    }
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

check_integer_range        <- function(value, name, range, len) {
  check         <- FALSE
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value%%1 != 0,
            value <= range[1], value >= range[2])) {
      check     <- TRUE
      if (len == 1) {
        segment <- " must be a single integer that belongs to {"
      } else {
        segment <- paste("must be an integer vector of length", len,
                         "whose elements all belong to {")
      }
    }
  } else if (any(value%%1 != 0, !is.numeric(value), value <= range[1],
                 value >= range[2])) {
    check       <- TRUE
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
  as.integer(value)
}

check_Kv                   <- function(Kv, name_Kv) {
  if (any(length(Kv) == 1, !is.numeric(Kv), Kv%%1 != 0)) {
    stop(name_Kv, " must be a numeric vector of monotonically decreasing whole",
         " numbers, whose final element is 1")
  } else if (any(Kv[-1] >= Kv[-length(Kv)])) {
    stop(name_Kv, " must be a numeric vector of monotonically decreasing whole",
         " numbers, whose final element is 1")
  }
  as.integer(Kv)
}

check_lambda               <- function(lambda, des) {
  if (missing(lambda)) {
    if (any(c("multiarm_des_dtl_pois", "multiarm_des_dtl_pois") %in%
            class(des))) {
      lambda <- rbind(rep(des$lambda0, des$Kv[1] + 1),
                  c(des$lambda0, rep(des$lambda0 + des$delta1, des$Kv[1])),
                  cbind(rep(des$lambda0, des$Kv[1]),
                        matrix(des$lambda0 + des$delta0, des$Kv[1], des$Kv[1]) +
                          (des$delta1 - des$delta0)*diag(des$Kv[1])))
    } else {
      lambda <- rbind(rep(des$lambda0, des$K + 1),
                  c(des$lambda0, rep(des$lambda0 + des$delta1, des$K)),
                  cbind(rep(des$lambda0, des$K),
                        matrix(des$lambda0 + des$delta0, des$K, des$K) +
                          (des$delta1 - des$delta0)*diag(des$K)))
    }
  } else {
    if (!is.numeric(lambda)) {
      stop("If specified, lambda must be numeric.")
    } else if (any(lambda < 0, is.infinite(lambda))) {
      stop("If specified, lambda must contain only values in [0,Inf).")
    }
    if (is.vector(lambda)) {
      lambda <- matrix(lambda, 1)
    }
    if (any(c("multiarm_des_dtl_pois", "multiarm_des_dtl_pois") %in%
            class(des))) {
      if (ncol(lambda) != des$Kv[1] + 1) {
        stop("If specified, lambda must have des$K + 1 columns.")
      }
    } else {
      if (ncol(lambda) != des$K + 1) {
        stop("If specified, lambda must have des$K + 1 columns.")
      }
    }
    if (sum(duplicated(lambda)) > 0) {
      warning("lambda contains duplicated rows.")
    }
  }
  lambda
}

check_logical              <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_multiarm_des_ss_bern <- function(des, int = FALSE, name = "des") {
  if (!("multiarm_des_ss_bern" %in% class(des))) {
    stop(name, " must be of class multiarm_des_ss_bern")
  }
  des$K <- check_integer_range(des$K, paste0(name, "$K"), c(1, Inf), 1)
  check_real_range_strict(des$alpha, paste0(name, "$alpha"), c(0, 1), 1)
  check_real_range_strict(des$beta, paste0(name, "$beta"), c(0, 1), 1)
  check_real_range_strict(des$pi0, paste0(name, "$pi0"), c(0, 1), 1)
  check_delta0_delta1(des$delta0, des$delta1, paste0(name, "$delta0"),
                      paste0(name, "$delta1"), des$pi0, paste0(name, "$pi0"))
  check_n_N(des$n, des$N, des$K, paste0(name, "$n"), paste0(name, "$N"),
            paste0(name, "$K"), int)
  check_ratio(des$ratio, des$K, des$n, paste0(name, "$ratio"),
              paste0(name, "$K"), paste0(name, "$n"))
  check_belong(des$ratio_scenario, paste0(name, "$ratio_scenario"),
               c("HG", "HA"), 1)
  check_belong(des$correction, paste0(name, "$correction"),
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(des$power, paste0(name, "$power"),
               c("conjunctive", "disjunctive", "marginal"), 1)
  check_gamma_gammaO(des$gamma, des$gammaO, des$K, des$correction,
                     paste0(name, "$gamma"), paste0(name, "$gammaO"),
                     paste0(name, "$K"), paste0(name, "$correction"))
  check_opchar_bern(des$opchar, des$K, des$pi0, des$delta0, des$delta1,
                    paste0(name, "$opchar"), paste0(name, "$K"),
                    paste0(name, "$pi0"), paste0(name, "$delta0"),
                    paste0(name, "$delta1"))
}

check_multiarm_des_ss_norm <- function(des, int = FALSE, name = "des") {
  if (!("multiarm_des_ss_norm" %in% class(des))) {
    stop(name, " must be of class multiarm_des_ss_norm")
  }
  des$K <- check_integer_range(des$K, paste0(name, "$K"), c(1, Inf), 1)
  check_real_range_strict(des$alpha, paste0(name, "$alpha"), c(0, 1), 1)
  check_real_range_strict(des$beta, paste0(name, "$beta"), c(0, 1), 1)
  check_delta0_delta1(des$delta0, des$delta1, paste0(name, "$delta0"),
                      paste0(name, "$delta1"))
  check_sigma(des$sigma, des$K, paste0(name, "$sigma"), paste0(name, "$K"))
  check_n_N(des$n, des$N, des$K, paste0(name, "$n"), paste0(name, "$N"),
            paste0(name, "$K"), int)
  check_ratio(des$ratio, des$K, des$n, paste0(name, "$ratio"),
              paste0(name, "$K"), paste0(name, "$n"))
  check_belong(des$correction, paste0(name, "$correction"),
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(des$power, paste0(name, "$power"),
               c("conjunctive", "disjunctive", "marginal"), 1)
  check_CovZ(des$CovZ, des$K, des$sigma, des$n, paste0(name, "$CovZ"),
             paste0(name, "$K"), paste0(name, "$sigma"), paste0(name, "$n"))
  check_gamma_gammaO(des$gamma, des$gammaO, des$K, des$correction,
                     paste0(name, "$gamma"), paste0(name, "$gammaO"),
                     paste0(name, "$K"), paste0(name, "$correction"))
  check_opchar(des$opchar, des$K, des$delta0, des$delta1,
               paste0(name, "$opchar"), paste0(name, "$K"),
               paste0(name, "$delta0"), paste0(name, "$delta1"))
}

check_multiarm_des_ss_pois <- function(des, int = FALSE, name = "des") {
  if (!("multiarm_des_ss_pois" %in% class(des))) {
    stop(name, " must be of class multiarm_des_ss_pois")
  }
  des$K <- check_integer_range(des$K, paste0(name, "$K"), c(1, Inf), 1)
  check_real_range_strict(des$alpha, paste0(name, "$alpha"), c(0, 1), 1)
  check_real_range_strict(des$beta, paste0(name, "$beta"), c(0, 1), 1)
  check_delta0_delta1(des$delta0, des$delta1, paste0(name, "$delta0"),
                      paste0(name, "$delta1"))
  check_n_N(des$n, des$N, des$K, paste0(name, "$n"), paste0(name, "$N"),
            paste0(name, "$K"), int)
  check_ratio(des$ratio, des$K, des$n, paste0(name, "$ratio"),
              paste0(name, "$K"), paste0(name, "$n"))
  check_belong(des$correction, paste0(name, "$correction"),
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  check_belong(des$power, paste0(name, "$power"),
               c("conjunctive", "disjunctive", "marginal"), 1)
  check_gamma_gammaO(des$gamma, des$gammaO, des$K, des$correction,
                     paste0(name, "$gamma"), paste0(name, "$gammaO"),
                     paste0(name, "$K"), paste0(name, "$correction"))
  check_opchar_pois(des$opchar, des$K, des$lambda0, des$delta0, des$delta1,
                    paste0(name, "$opchar"), paste0(name, "$K"),
                    paste0(name, "$lambda0"), paste0(name, "$delta0"),
                    paste0(name, "$delta1"))
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

check_opchar_pois          <- function(opchar, K, lambda0, delta0, delta1,
                                       name_opchar, name_K, name_lambda0,
                                       name_delta0, name_delta1) {
  if (any(nrow(opchar) != K + 2L, ncol(opchar) != 4*K + 9L)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_lambda0, ", ",
         name_delta0, ", and ", name_delta1)
  }
  lambda <- rbind(rep(lambda0, K + 1),
                  c(lambda0, rep(lambda0 + delta1, K)),
                  cbind(rep(lambda0, K),
                        matrix(lambda0 + delta0, K, K) +
                          (delta1 - delta0)*diag(K)))
  if (any(lambda - opchar[, 1:(K + 1)] > 1e-10)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_lambda0, ", ",
         name_delta0, ", and ", name_delta1)
  }
  if (any(lambda[, -(1:(K + 1))] > 1, lambda[, -(1:(K + 1))] < 0)) {
    stop(name_opchar, " must correspond to ", name_K, ", ", name_lambda0, ", ",
         name_delta0, ", and ", name_delta1)
  }
}

check_pi                   <- function(pi, des) {
  if (missing(pi)) {
    if (any(c("multiarm_des_dtl_bern", "multiarm_des_dtl_norm") %in%
            class(des))) {
      pi <- rbind(rep(des$pi0, des$Kv[1] + 1),
                  c(des$pi0, rep(des$pi0 + des$delta1, des$Kv[1])),
                  cbind(rep(des$pi0, des$Kv[1]),
                        matrix(des$pi0 + des$delta0, des$Kv[1], des$Kv[1]) +
                          (des$delta1 - des$delta0)*diag(des$Kv[1])))
    } else {
      pi <- rbind(rep(des$pi0, des$K + 1),
                  c(des$pi0, rep(des$pi0 + des$delta1, des$K)),
                  cbind(rep(des$pi0, des$K),
                        matrix(des$pi0 + des$delta0, des$K, des$K) +
                          (des$delta1 - des$delta0)*diag(des$K)))
    }
  } else {
    if (!is.numeric(pi)) {
      stop("If specified, pi must be numeric.")
    } else if (any(pi < 0, pi > 1)) {
      stop("If specified, pi must contain only values in [0,1].")
    }
    if (is.vector(pi)) {
      pi <- matrix(pi, 1)
    }
    if (any(c("multiarm_des_dtl_bern", "multiarm_des_dtl_norm") %in%
            class(des))) {
      if (ncol(pi) != des$Kv[1] + 1) {
        stop("If specified, pi must have des$K + 1 columns.")
      }
    } else {
      if (ncol(pi) != des$K + 1) {
        stop("If specified, pi must have des$K + 1 columns.")
      }
    }
    if (sum(duplicated(pi)) > 0) {
      warning("pi contains duplicated rows.")
    }
  }
  pi
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

check_ratio                <- function(ratio, K, n, name_ratio, name_K,
                                       name_n, des = "fixed") {
  if (des == "fixed") {
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
  } else {
    if (missing(n)) {
      if (any(length(ratio) != 1, !is.numeric(ratio), ratio <= 0,
              is.infinite(ratio))) {
        stop("ratio must be a single numeric that belongs to (0,Inf).")
      }
    } else {
      if (!all.equal(ratio, n[-1]/n[1])) {
        stop(name_ratio, " must correspond to ", name_n)
      }
    }
  }
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

check_sigma                <- function(sigma, K, name_sigma, name_K,
                                       des = "fixed") {
  if (des == "fixed") {
    if (any(length(sigma) != K + 1L, !is.numeric(sigma), sigma <= 0,
            is.infinite(sigma))) {
      stop(name_sigma, " must be a numeric vector of length ", name_K, " + 1, ",
           "whose elements all belong to (0, \u221E).")
    }
  } else {
    if (any(!(length(sigma) %in% 1:2), !is.numeric(sigma), sigma <= 0,
            is.infinite(sigma))) {
      stop(name_sigma, " must be a numeric vector of length 1 or 2, whose ",
           "elements all belong to (0, \u221E).")
    }
    if (length(sigma) == 1) {
      rep(sigma, 2)
    } else {
      sigma
    }
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

check_tau                  <- function(tau, des) {
  if (missing(tau)) {
    if (any(c("multiarm_des_dtl_bern", "multiarm_des_dtl_norm") %in%
            class(des))) {
      tau <- rbind(rep(0, des$Kv[1]), rep(des$delta1, des$Kv[1]),
                   matrix(des$delta0, des$Kv[1], des$Kv[1]) +
                     (des$delta1 - des$delta0)*diag(des$Kv[1]))
    } else {
      tau <- rbind(rep(0, des$K), rep(des$delta1, des$K),
                   matrix(des$delta0, des$K, des$K) +
                     (des$delta1 - des$delta0)*diag(des$K))
    }
  } else {
    if (!is.numeric(tau)) {
      stop("If specified, tau must be numeric.")
    } else if (any(is.infinite(tau))) {
      stop("If specified, tau must contain only finite values.")
    }
    if (is.vector(tau)) {
      tau <- matrix(tau, 1)
    }
    if (any(c("multiarm_des_dtl_bern", "multiarm_des_dtl_norm") %in%
            class(des))) {
      if (ncol(tau) != des$Kv[1]) {
        stop("If specified, tau must have des$Kv[1] columns.")
      }
    } else {
      if (ncol(tau) != des$K) {
        stop("If specified, tau must have des$K columns.")
      }
    }
    if (sum(duplicated(tau)) > 0) {
      warning("tau contains duplicated rows.")
    }
  }
  tau
}
