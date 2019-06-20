components_update_bern <- function(components, K, alpha, correction, CovZ) {
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

covariance_ma          <- function(K, n, sigma, standardise = F) {
  if (standardise) {
    CovTau      <- matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
    diag_sqrt_I <- diag(1/sqrt(diag(CovTau)))
    diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  } else {
    matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
  }
}

determinant_ma         <- function(n, K, sigma) {
  sum(sapply(1:(K + 1), function(k) { n[k]*prod(sigma[-k]^2) }))/prod(n)
}

gamma_ma               <- function(K, alpha, correction, CovZ) {
  gamma            <- gammaO <- NA
  if (correction == "benjamini_hochberg") {
    gammaO         <- (1:K)*alpha/K
  } else if (correction == "benjamini_yekutieli") {
    gammaO         <- (1:K)*alpha/(K*sum(1/(1:K)))
  } else if (correction == "bonferroni") {
    gamma          <- alpha/K
  } else if (correction == "dunnett") {
    gamma          <-
      stats::pnorm(mvtnorm::qmvnorm(1 - alpha, sigma = CovZ)$quantile,
                   lower.tail = F)
  } else if (correction %in% c("hochberg", "holm_bonferroni")) {
    gammaO         <- alpha/(K:1)
  } else if (correction == "holm_sidak") {
    gammaO         <- 1 - (1 - alpha)^(1/(K:1))
  } else if (correction == "none") {
    gamma          <- alpha
  } else if (correction == "sidak") {
    gamma          <- 1 - (1 - alpha)^(1/K)
  } else if (correction == "step_down_dunnett") {
    Ksq            <- K^2
    if (length(unique(CovZ[(1:Ksq)[-seq(1, Ksq, K + 1)]])) > 1) {
      stop("The step-down Dunnett correction is only supported for scenarios ",
           "with a shared covariance between the test statistics")
    }
    corr           <- CovZ[2, 1]
    one_min_alpha  <- 1 - alpha
    gammaO         <-
      sapply(1:K,
             function(k) {
               dim <- K - (k - 1L)
               stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                             sigma =
                                               matrix(corr, dim, dim) +
                                               (1 - corr)*diag(dim))$quantile,
                            lower.tail = F) })
  }
  list(gamma = gamma, gammaO = gammaO, u = stats::qnorm(1 - gamma),
       uO = stats::qnorm(1 - gammaO))
}

max_eigenvalue_ma      <- function(n, K, sigma) {
  max(eigen(matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1]))$values)
}

opchar_ma_single       <- function(tau, K, sigma, n, CovZ, u) {
  components          <- outcomes_ma_single(K, u)
  rows_tau            <- nrow(tau)
  opchar              <- cbind(tau, matrix(0, rows_tau, 3*K + 8))
  EZs                 <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                    rows_tau, K, byrow = T)
  seq_K               <- 1:K
  for (i in 1:rows_tau) {
    opchar[i, -seq_K] <- power_fdr_ma_single(EZs[i, ], K, CovZ, u, components)
  }
  colnames(opchar)    <- c(paste0("tau", seq_K), "Pdis", "Pcon",
                           paste0("P", seq_K), paste0("FWERI", seq_K),
                           paste0("FWERII", seq_K), "PHER", "FDR", "pFDR",
                           "FNDR", "Sens", "Spec")
  tibble::as_tibble(opchar)
}

opchar_ma_single_bern  <- function(p, K, alpha, correction, n, u) {
  components                    <- outcomes_ma_single(K, u)
  rows_p                        <- nrow(p)
  sigmas                        <- sqrt(p*(1 - p))
  opchar                        <- cbind(p, matrix(0, rows_p, 3*K + 8))
  EZs                           <-
    (p[, -1] - p[, 1])/sqrt(matrix(sigmas[, 1]^2/n[1], rows_p, K) +
                              sigmas[, -1]^2/matrix(n[-1], rows_p, K,
                                                    byrow = T))
  seq_K                         <- 1:K
  for (i in 1:rows_p) {
    CovZ                        <- covariance_ma(K, n, sigmas[i, ], T)
    components                  <- components_update_bern(components, K, alpha,
                                                          correction, CovZ)
    opchar[i, -c(seq_K, K + 1)] <- power_fdr_ma_single(EZs[i, ], K, CovZ, u,
                                                       components)
  }
  colnames(opchar)              <-
    c(paste0("pi", c(0, seq_K)), "Pdis", "Pcon", paste0("P", seq_K),
      paste0("FWERI", seq_K), paste0("FWERII", seq_K), "PHER", "FDR", "pFDR",
      "FNDR", "Sens", "Spec")
  tibble::as_tibble(opchar)
}

opchar_ma_step         <- function(tau, K, sigma, correction, n, CovZ, uO) {
  rows_tau              <- nrow(tau)
  EZs                   <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                      rows_tau, K, byrow = T)
  seq_K                 <- 1:K
  if (rows_tau == 1) {
    opchar              <-
      matrix(c(tau, power_fdr_ma_step(EZs[1, ], K, CovZ, uO, "normal")), 1,
             4*K + 8, byrow = T)
  } else {
    components          <- outcomes_ma_step(EZs[1, ], K, correction, CovZ, uO)
    opchar              <- cbind(tau, matrix(0, rows_tau, 3*K + 8))
    for (i in 1:rows_tau) {
      opchar[i, -seq_K] <- power_fdr_ma_step(EZs[i, ], K, correction, CovZ, uO,
                                             "normal", components)
    }
  }
  colnames(opchar)      <- c(paste0("tau", seq_K), "Pdis", "Pcon",
                             paste0("P", seq_K), paste0("FWERI", seq_K),
                             paste0("FWERII", seq_K), "PHER", "FDR", "pFDR",
                             "FNDR", "Sens", "Spec")
  tibble::as_tibble(opchar)
}

opchar_ma_step_bern    <- function(p, K, alpha, correction, n, uO) {
  seq_K                   <- 1:K
  rows_p                  <- nrow(p)
  sigmas                  <- sqrt(p*(1 - p))
  EZs                     <-
    (p[, -1] - p[, 1])/sqrt(matrix(sigmas[, 1]^2/n[1], rows_p, K) +
                              sigmas[, -1]^2/matrix(n[-1], rows_p, K,
                                                    byrow = T))
  if (rows_p == 1) {
    CovZ                  <- covariance_ma(K, n, sigmas[1, ], T)
    components            <- outcomes_ma_step(EZs[1, ], K, correction, CovZ, uO)
    components            <- components_update_bern(components, K, alpha,
                                                    correction, CovZ)
    opchar                <-
      matrix(c(p, power_fdr_ma_step(EZs[1, ], K, CovZ, uO, "bernoulli",
                                    components)), 1, 4*K + 9, byrow = T)
  } else {
    CovZ                  <- covariance_ma(K, n, sigmas[1, ], T)
    components            <- outcomes_ma_step(EZs[1, ], K, correction, CovZ, uO)
    components            <- components_update_bern(components, K, alpha,
                                                    correction, CovZ)
    opchar                <- cbind(p, matrix(0, rows_p, 3*K + 8))
    seq_Kp1               <- c(seq_K, K + 1)
    opchar[1, -seq_Kp1]   <- power_fdr_ma_step(EZs[1, ], K, correction, CovZ,
                                               uO, "bernoulli", components)
    for (i in 2:rows_p) {
      CovZ                <- covariance_ma(K, n, sigmas[i, ], T)
      components          <- components_update_bern(components, K, alpha,
                                                    correction, CovZ)
      opchar[i, -seq_Kp1] <- power_fdr_ma_step(EZs[i, ], K, correction, CovZ,
                                               uO, "bernoulli", components)
    }
  }
  colnames(opchar)        <- c(paste0("pi", c(0, seq_K)), "Pdis", "Pcon",
                               paste0("P", seq_K), paste0("FWERI", seq_K),
                               paste0("FWERII", seq_K), "PHER", "FDR", "pFDR",
                               "FNDR", "Sens", "Spec")
  tibble::as_tibble(opchar)
}

outcomes_ma_single     <- function(K, u) {
  two_to_K        <- 2^K
  outcomes        <- iterpc::getall(iterpc::iterpc(2, K, 0:1, T, T))
  discoveries     <- Rfast::rowsums(outcomes)
  ls              <- rep(list(rep(-Inf, K)), two_to_K)
  us              <- rep(list(rep(Inf, K)), two_to_K)
  us[[1]]         <- ls[[two_to_K]] <- rep(u, K)
  seq_K           <- 1:K
  for (i in 2:(two_to_K - 1)) {
    rej           <- which(outcomes[i, seq_K] == 1)
    ls[[i]][rej]  <- us[[i]][-rej] <- u
  }
  list(discoveries = discoveries, inv_outcomes = (outcomes == 0), ls = ls,
       non_discoveries = K - discoveries, outcomes = outcomes, us = us)
}

outcomes_ma_step       <- function(EZ, K, correction, CovZ, uO, type,
                                   components) {
  if (missing(components)) {
    seq_K                            <- 1:K
    rankings                         <-
      iterpc::getall(iterpc::iterpc(K, K, ordered = T))
    rows_rankings                    <- factorial(K)
    rows_outcomes                    <- rows_rankings*(K + 1)
    outcomes                         <- matrix(0L, rows_outcomes, 2*K)
    outcomes[1:rows_rankings, seq_K] <- rankings
    for (k in seq_K[-K]) {
      range                          <-
        (1 + k*rows_rankings):((k + 1)*rows_rankings)
      outcomes[range, seq_K]         <- rankings
      for (i in range) {
        outcomes[i, K + which(outcomes[i, seq_K] %in% 1:k)]            <- 1L
      }
    }
    range                            <-
      (1 + K*rows_rankings):((K + 1)*rows_rankings)
    outcomes[range, seq_K]           <- rankings
    outcomes[range, -seq_K]          <- 1L
    discoveries                      <- rep(0:K, each = rows_rankings)
    As                               <- ls <- means <- covariances <- list()
    for (i in 1:rows_outcomes) {
      sum_i                          <- sum(outcomes[i, K + seq_K])
      if (correction %in% c("holm_bonferroni", "holm_sidak",
                            "step_down_dunnett")) {
        As[[i]]                      <-
          matrix(0L, K - 1 + sum_i + as.numeric(sum_i < K), K)
      } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                                   "hochberg")) {
        As[[i]]                      <-
          matrix(0L, 2*K - 1 - sum_i + as.numeric(sum_i > 0), K)
      }
      for (k in seq_K[-K]) {
        As[[i]][k, which(outcomes[i, seq_K] == k)]                     <- 1L
        As[[i]][k, which(outcomes[i, seq_K] == k + 1)]                 <- -1L
      }
      if (correction %in% c("holm_bonferroni", "holm_sidak",
                            "step_down_dunnett")) {
        if (sum_i > 0) {
          for (k in 1:sum_i) {
            As[[i]][K - 1 + k, which(outcomes[i, seq_K] == k)]         <- 1L
          }
        }
        if (sum_i < K) {
          As[[i]][K + sum_i,
                  which(outcomes[i, seq_K] == sum_i + 1)]              <- -1L
          ls[[i]]                    <- c(numeric(K - 1), uO[seq_len(sum_i)],
                                          -uO[sum_i + 1])
        } else {
          ls[[i]]                    <- c(numeric(K - 1), uO)
        }
      } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                                   "hochberg")) {
        if (sum_i < K) {
          for (k in (sum_i + 1):K) {
            As[[i]][K - 1 + k - sum_i, which(outcomes[i, seq_K] == k)] <- -1L
          }
        }
        if (sum_i > 0) {
          As[[i]][2*K - sum_i, which(outcomes[i, seq_K] == sum_i)]     <- 1L
          if (sum_i < K) {
            ls[[i]]                  <- c(numeric(K - 1), -uO[(sum_i + 1):K],
                                          uO[sum_i])
          } else {
            ls[[i]]                  <- c(numeric(K - 1), uO[K])
          }
        } else {
          ls[[i]]                    <- c(numeric(K - 1), -uO)
        }
      }
      means[[i]]                     <- as.numeric(As[[i]]%*%EZ)
      covariances[[i]]               <- As[[i]]%*%CovZ%*%t(As[[i]])
    }
    list(As = As, covariances = covariances, discoveries = discoveries,
         inv_outcomes = (outcomes[, -seq_K] == 0), ls = ls, means = means,
         non_discoveries = K - discoveries, outcomes = outcomes)
  } else {
    if (type == "normal") {
      for (i in 1:factorial(K + 1)) {
        components$means[[i]]        <- as.numeric(components$As[[i]]%*%EZ)
      }
    } else {
      for (i in 1:factorial(K + 1)) {
        components$means[[i]]        <- as.numeric(components$As[[i]]%*%EZ)
        components$covariances[[i]]  <-
          components$As[[i]]%*%CovZ%*%t(components$As[[i]])
      }
    }
    components
  }
}

power_all_ma_single    <- function(EZ, K, CovZ, u) {
  mvtnorm::pmvnorm(lower = rep(u, K), mean = EZ, sigma = CovZ)[1]
}

power_all_ma_step      <- function(EZ, K, correction, CovZ, uO, type,
                                   components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, type, components)
  P          <- 0
  for (i in which(components$discoveries == K)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

power_any_ma_single    <- function(EZ, K, CovZ, u) {
  1 - mvtnorm::pmvnorm(upper = rep(u, K), mean = EZ, sigma = CovZ)[1]
}

power_any_ma_step      <- function(EZ, K, correction, CovZ, uO, type,
                                   components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, type, components)
  P          <- 0
  for (i in which(components$discoveries > 0)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

power_fdr_ma_single    <- function(EZ, K, CovZ, u, components) {
  if (missing(components)) {
    components                          <- outcomes_ma_single(K, u)
  }
  two_to_K                              <- 2^K
  P                                     <- numeric(two_to_K)
  for (i in 1:two_to_K) {
    P[i]                                <- mvtnorm::pmvnorm(components$ls[[i]],
                                                            components$us[[i]],
                                                            EZ, sigma = CovZ)[1]
  }
  neg_mat                               <- matrix(EZ <= 0, two_to_K, K,
                                                  byrow = T)
  pos_mat                               <- !neg_mat
  false_discoveries                     <-
    Rfast::rowsums(components$outcomes*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums(components$inv_outcomes*pos_mat)
  which_discoveries                     <- which(components$discoveries > 0)
  components$discoveries[-which_discoveries]                  <- 1
  components$non_discoveries[components$non_discoveries == 0] <- 1
  seq_K                                 <- 1:K
  c(sum(P[-1]), P[two_to_K],
    sapply(seq_K, function(k) { sum(P[components$outcomes[, k] > 0]) }),
    sapply(seq_K, function(k) { sum(P[false_discoveries >= k]) }),
    sapply(seq_K, function(k) { sum(P[false_non_discoveries >= k]) }),
    sum(P*false_discoveries)/K,
    sum(P*false_discoveries/components$discoveries),
    sum(P[which_discoveries]*false_discoveries[which_discoveries]/
          components$discoveries[which_discoveries])/sum(P[which_discoveries]),
    sum(P*false_non_discoveries/components$non_discoveries),
    sum(P*Rfast::rowsums(components$outcomes*pos_mat))/max(sum(EZ > 0), 1),
    sum(P*Rfast::rowsums(components$inv_outcomes*neg_mat))/max(sum(EZ <= 0), 1))
}

power_fdr_ma_step      <- function(EZ, K, correction, CovZ, uO, type,
                                   components) {
  components                            <-
    outcomes_ma_step(EZ, K, correction, CovZ, uO, type, components)
  seq_K                                 <- 1:K
  rows_outcomes                         <- factorial(K)*(K + 1)
  P                                     <- numeric(rows_outcomes)
  for (i in 1:rows_outcomes) {
    P[i]                                <-
      mvtnorm::pmvnorm(components$ls[[i]], mean  = components$means[[i]],
                       sigma = components$covariances[[i]])[1]
  }
  neg_mat                               <- matrix(EZ <= 0, rows_outcomes, K,
                                                  byrow = T)
  pos_mat                               <- !neg_mat
  false_discoveries                     <-
    Rfast::rowsums(components$outcomes[, -seq_K]*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums(components$inv_outcomes*pos_mat)
  which_discoveries                     <- which(components$discoveries > 0)
  components$discoveries[-which_discoveries]       <- 1

  components$non_discoveries[components$non_discoveries == 0] <- 1
  c(sum(P[which_discoveries]), sum(P[components$discoveries == K]),
    sapply(seq_K, function(k) { sum(P[components$outcomes[, K + k] > 0]) }),
    sapply(seq_K, function(k) { sum(P[false_discoveries >= k]) }),
    sapply(seq_K, function(k) { sum(P[false_non_discoveries >= k]) }),
    sum(P*false_discoveries)/K,
    sum(P*false_discoveries/components$discoveries),
    sum(P[which_discoveries]*false_discoveries[which_discoveries]/
          components$discoveries[which_discoveries])/sum(P[which_discoveries]),
    sum(P*false_non_discoveries/components$non_discoveries),
    sum(P*Rfast::rowsums(components$outcomes[, -seq_K]*pos_mat))/
      max(sum(EZ > 0), 1),
    sum(P*Rfast::rowsums(components$inv_outcomes*neg_mat))/max(sum(EZ <= 0), 1))
}

power_ma               <- function(N, K, beta, correction, power, CovZ, u, uO,
                                   type, EZ_div_sqrt_N, power_index,
                                   components) {
  if (power == "conjunctive") {
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P <- power_all_ma_single(sqrt(N)*EZ_div_sqrt_N, K, CovZ, u)
    } else {
      P <- power_all_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                             type, components)
    }
  } else if (power == "disjunctive") {
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P <- power_any_ma_single(sqrt(N)*EZ_div_sqrt_N, K, CovZ, u)
    } else {
      P <- power_any_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                             type, components)
    }
  } else if (power == "marginal") {
    P   <- power_marg_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                              type, power_index, components)
  }
  P - (1 - beta)
}

power_marg_ma_step     <- function(EZ, K, correction, CovZ, uO, type,
                                   power_index, components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, type, components)
  P          <- 0
  for (i in which(components$outcomes[, K + power_index] == 1)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

sim_ma_internal        <- function(tau, n, sigma, correction, sigma_z, t_test,
                                   pooled, replicates, gamma, gammaO,
                                   completed_replicates, total_replicates,
                                   summary) {
  summary_i                             <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  K                                     <- length(tau)
  Kp1                                   <- K + 1
  rej_mat                               <- matrix(0L, replicates, K)
  if (!t_test) {
    denominator                         <- sqrt(sigma_z[1]^2/n[1] +
                                                  sigma_z[-1]^2/n[-1])
    sds                                 <- sqrt(sigma^2/n)
  } else {
    seq_Kp1                             <- 1:(K + 1)
    N                                   <- sum(n)
  }
  means                                 <- c(0, tau)
  for (i in 1:replicates) {
    if (t_test) {
      X                                 <-
        lapply(seq_Kp1, function(k) { stats::rnorm(n[k], means[k], sigma[k]) })
      X_bar                             <-
        sapply(seq_Kp1, function(k) { sum(X[[k]])/n[k] })
      if (pooled) {
        sigma_hat                       <-
          sapply(seq_Kp1,
                 function(k) { sum((X[[k]] - X_bar[k])^2)/(n[k] - 1L) })
      } else {
        X                               <- unlist(X)
        sigma_hat                       <- rep(sum((X - sum(X)/N)^2)/(N - 1),
                                               Kp1)
      }
      pvals                             <-
        stats::pnorm((X_bar[-1] - X_bar[1])/sqrt(sigma_hat[1]^2/n[1] +
                                                   sigma_hat[-1]^2/n[-1]),
                     lower.tail = F)
    } else {
      X_bar                             <- stats::rnorm(Kp1, means, sds)
      pvals                             <-
        stats::pnorm((X_bar[-1] - X_bar[1])/denominator, lower.tail = F)
    }
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      rej_mat[i, ]                      <- (pvals <= gamma)
    } else if (correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
      order_pvals                       <- order(pvals)
      k                                 <- check <- 1
      while (all(k <= K, check == 1)) {
        if (pvals[order_pvals[k]] <= gammaO[k]) {
          rej_mat[i, order_pvals[k]]    <- rej_mat[i, order_pvals[k]] + 1
          k                             <- k + 1
        } else {
          check                         <- 0
        }
      }
    } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                                 "hochberg")) {
      order_pvals                       <- order(pvals)
      for (k in K:1) {
        if (pvals[order_pvals[k]] <= gammaO[k]) {
          rej_mat[i, order_pvals[1:k]]  <- rep(1, k)
          break
        }
      }
    }
    if (all((completed_replicates + i) %in% summary_i, summary)) {
      message(uc("two_elip"), "approximately ",
              10*which(summary_i == (completed_replicates + i)),
              "% through the required simulations", uc("two_elip"))
    }
  }
  discoveries                           <- Rfast::rowsums(rej_mat)
  non_discoveries                       <- K - discoveries
  neg_mat                               <- matrix(tau <= 0, replicates, K,
                                                  byrow = T)
  pos_mat                               <- !neg_mat
  false_discoveries                     <- Rfast::rowsums(rej_mat*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums((rej_mat == 0)*pos_mat)
  which_discoveries                     <- which(discoveries > 0)
  discoveries[-which_discoveries]       <- 1
  non_discoveries[non_discoveries == 0] <- 1
  c(tau, length(which_discoveries)/replicates, sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sapply(1:K, function(k) { sum(false_discoveries >= k) })/replicates,
    sapply(1:K, function(k) { sum(false_non_discoveries >= k) })/replicates,
    sum(false_discoveries)/(K*replicates),
    sum(false_discoveries/discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat[which_discoveries, ]*
                         matrix(tau <= 0, length(which_discoveries), K,
                                byrow = T))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(tau > 0), 1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(tau <= 0), 1)*replicates))
}

sim_ma_bern_internal   <- function(pi, n, alpha, correction, replicates, gamma,
                                   gammaO, completed_replicates,
                                   total_replicates, summary) {
  summary_i                             <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  K                                     <- length(pi) - 1
  Kp1                                   <- K + 1
  rej_mat                               <- matrix(0L, replicates, K)
  seq_K                                 <- 1:K
  seq_Kp1                               <- c(seq_K, Kp1)
  one_min_alpha                         <- 1 - alpha
  for (i in 1:replicates) {
    pi_hat <- stats::rbinom(Kp1, n, pi)/n
    if (correction %in% c("dunnett", "step_down_dunnett")) {
      sigma  <- sqrt(pi_hat*(1 - pi_hat))
      pvals  <- stats::pnorm((pi_hat[-1] - pi_hat[1])/
                               sqrt(sigma[-1]^2/n[-1] + sigma[1]^2/n[1]),
                             lower.tail = F)
    } else {
      pvals  <- stats::pnorm((pi_hat[-1] - pi_hat[1])/
                               sqrt(pi_hat[-1]*(1 - pi_hat[-1])/n[-1] +
                                      pi_hat[1]*(1 - pi_hat[1])/n[1]),
                             lower.tail = F)
    }
    if (sum(is.na(pvals)) > 0) {
      pvals[which(is.na(pvals))] <- 0.5
    }
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      if (correction == "dunnett") {
        gamma                     <-
          stats::pnorm(mvtnorm::qmvnorm(1 - alpha,
                                        sigma = covariance_ma(K, n, sigma,
                                                              T))$quantile,
                       lower.tail = F)
      }
      rej_mat[i, ]                      <- (pvals <= gamma)
    } else if (correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
      if (correction == "step_down_dunnett") {
        corr                      <- covariance_ma(K, n, sigma, T)[2, 1]
        gammaO                    <-
          sapply(seq_K,
                 function(k) {
                   dim            <- Kp1 - k
                   stats::pnorm(
                     mvtnorm::qmvnorm(one_min_alpha,
                                      sigma = matrix(corr, dim, dim) +
                                                (1 - corr)*diag(dim))$quantile,
                     lower.tail = F) })
      }
      order_pvals                       <- order(pvals)
      k                                 <- check <- 1
      while (all(k <= K, check == 1)) {
        if (pvals[order_pvals[k]] <= gammaO[k]) {
          rej_mat[i, order_pvals[k]]    <- rej_mat[i, order_pvals[k]] + 1
          k                             <- k + 1
        } else {
          check                         <- 0
        }
      }
    } else if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                                 "hochberg")) {
      order_pvals                       <- order(pvals)
      for (k in K:1) {
        if (pvals[order_pvals[k]] <= gammaO[k]) {
          rej_mat[i, order_pvals[1:k]]  <- rep(1, k)
          break
        }
      }
    }
    if (all((completed_replicates + i) %in% summary_i, summary)) {
      message(uc("two_elip"), "approximately ",
              10*which(summary_i == (completed_replicates + i)),
              "% through the required simulations", uc("two_elip"))
    }
  }
  discoveries                           <- Rfast::rowsums(rej_mat)
  non_discoveries                       <- K - discoveries
  neg_mat                               <- matrix(pi[-1] <= pi[1],
                                                  replicates, K, byrow = T)
  pos_mat                               <- !neg_mat
  false_discoveries                     <- Rfast::rowsums(rej_mat*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums((rej_mat == 0)*pos_mat)
  which_discoveries                     <- which(discoveries > 0)
  discoveries[-which_discoveries]       <- 1
  non_discoveries[non_discoveries == 0] <- 1
  c(pi, length(which_discoveries)/replicates, sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sapply(seq_K, function(k) { sum(false_discoveries >= k) })/replicates,
    sapply(seq_K, function(k) { sum(false_non_discoveries >= k) })/replicates,
    sum(false_discoveries)/(K*replicates),
    sum(false_discoveries/discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat[which_discoveries, ]*
                         matrix(pi[-1] <= pi[1], length(which_discoveries), K,
                                byrow = T))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(pi[-1] > pi[1]), 1)*
                                            replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(pi[-1] <= pi[1]), 1)*replicates))
}

trace_ma               <- function(n, K, sigma) {
  K*sigma[1]^2/n[1] + sum(sigma[-1]^2/n[-1])
}
