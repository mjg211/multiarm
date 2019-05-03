covariance_ma       <- function(K, n, sigma) {
  matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
}

determinant_ma      <- function(n, K, sigma) {
  sum(sapply(1:(K + 1), function(k) { n[k]*prod(sigma[-k]^2) }))/prod(n)
}

max_eigenvalue_ma   <- function(n, K, sigma) {
  max(eigen(matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1]))$values)
}

opchar_ma_single    <- function(tau, K, sigma, n, CovZ, u) {
  components       <- outcomes_ma_single(K, u)
  rows_tau         <- nrow(tau)
  P_mat            <- matrix(0, rows_tau, K + 4L)
  EZs              <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                 rows_tau, K, byrow = T)
  for (i in 1:rows_tau) {
    P_mat[i, ]     <- power_fdr_ma_single(EZs[i, ], K, CovZ, u, components)
  }
  opchar           <- cbind(tau, P_mat)
  seq_K            <- 1:K
  colnames(opchar) <- c(paste("tau", seq_K, sep = ""), "Pdis", "Pcon",
                        paste("P", seq_K, sep = ""), "FWER", "FDR")
  opchar           <- tibble::as_tibble(opchar)
  opchar
}

opchar_ma_step      <- function(tau, K, sigma, correction, n, CovZ, uO) {
  rows_tau         <- nrow(tau)
  EZs              <- tau/matrix(sqrt(sigma[1]^2/n[1] + sigma[-1]^2/n[-1]),
                                 rows_tau, K, byrow = T)
  if (rows_tau == 1) {
    P_mat          <- matrix(power_fdr_ma_step(EZs[1, ], K, CovZ, uO),
                             1L, K + 4L, byrow = T)
  } else {
    components     <- outcomes_ma_step(EZs[1, ], K, correction, CovZ, uO)
    P_mat          <- matrix(0L, rows_tau, K + 4L)
    for (i in 1:rows_tau) {
      P_mat[i, ]   <- power_fdr_ma_step(EZs[i, ], K, correction, CovZ, uO,
                                        components)
    }
  }
  opchar           <- cbind(tau, P_mat)
  seq_K            <- 1:K
  colnames(opchar) <- c(paste("tau", seq_K, sep = ""), "Pdis", "Pcon",
                        paste("P", seq_K, sep = ""), "FWER", "FDR")
  opchar           <- tibble::as_tibble(opchar)
  opchar
}

outcomes_ma_single  <- function(K, u) {
  two_to_K               <- 2L^K
  outcomes               <- iterpc::getall(iterpc::iterpc(2L, K, 0:1, T, T))
  ls                     <- rep(list(rep(-Inf, K)), two_to_K)
  us                     <- rep(list(rep(Inf, K)), two_to_K)
  seq_K                  <- 1:K
  for (i in 1:two_to_K) {
    rej                  <- which(outcomes[i, seq_K] == 1L)
    ls[[i]][rej]         <- u
    us[[i]][seq_K[-rej]] <- u
  }
  list(ls = ls, outcomes = outcomes, us = us)
}

outcomes_ma_step    <- function(EZ, K, correction, CovZ, uO, components) {
  if (missing(components)) {
    seq_K                            <- 1:K
    rankings                         <-
      iterpc::getall(iterpc::iterpc(K, K, ordered = T))
    rows_rankings                    <- factorial(K)
    rows_outcomes                    <- rows_rankings*(K + 1L)
    outcomes                         <- matrix(0L, rows_outcomes, 2L*K)
    outcomes[1:rows_rankings, seq_K] <- rankings
    for (k in seq_K[-K]) {
      range                          <-
        (1L + k*rows_rankings):((k + 1L)*rows_rankings)
      outcomes[range, seq_K]         <- rankings
      for (i in range) {
        outcomes[i, K + which(outcomes[i, seq_K] %in% 1:k)]  <- 1L
      }
    }
    range                            <-
      (1L + K*rows_rankings):((K + 1L)*rows_rankings)
    outcomes[range, seq_K]           <- rankings
    outcomes[range, K + seq_K]       <- 1L
    As                               <- ls <- means <- covariances <- list()
    for (i in 1:rows_outcomes) {
      sum_i                          <- sum(outcomes[i, K + seq_K])
      if (correction %in% c("holm", "step_down_dunnett")) {
        As[[i]]                      <- matrix(0L, K - 1 + sum_i +
                                                     as.numeric(sum_i < K), K)
      } else if (correction %in% c("benjamini_hochberg", "hochberg")) {
        As[[i]]                      <-
          matrix(0L, K - 1 + (K - sum_i) + as.numeric(sum_i > 0), K)
      }
      for (k in seq_K[-K]) {
        As[[i]][k, which(outcomes[i, seq_K] == k)]             <- 1L
        As[[i]][k, which(outcomes[i, seq_K] == k + 1)]         <- -1L
      }
      if (correction %in% c("holm", "step_down_dunnett")) {
        if (sum_i > 0) {
          for (k in 1:sum_i) {
            As[[i]][K - 1 + k, which(outcomes[i, seq_K] == k)] <- 1L
          }
        }
        if (sum_i < K) {
          As[[i]][K + sum_i,
                  which(outcomes[i, seq_K] == sum_i + 1)]      <- -1L
          ls[[i]]                    <- c(numeric(K - 1L), uO[seq_len(sum_i)],
                                          -uO[sum_i + 1L])
        } else {
          ls[[i]]                    <- c(numeric(K - 1L), uO)
        }
      } else if (correction %in% c("benjamini_hochberg", "hochberg")) {
        if (sum_i < K) {
          for (k in (sum_i + 1):K) {
            As[[i]][K - 1 + k - sum_i, which(outcomes[i, seq_K] == k)] <- -1L
          }
        }
        if (sum_i > 0) {
          As[[i]][K + (K - sum_i),
                  which(outcomes[i, seq_K] == sum_i)]          <- 1L
          if (sum_i < K) {
            ls[[i]]                  <- c(numeric(K - 1L), -uO[(sum_i + 1L):K],
                                          uO[sum_i])
          } else {
            ls[[i]]                  <- c(numeric(K - 1L), uO[K])
          }
        } else {
          ls[[i]]                    <- c(numeric(K - 1L), -uO)
        }
      }
      means[[i]]                     <- as.numeric(As[[i]]%*%EZ)
      covariances[[i]]               <- As[[i]]%*%CovZ%*%t(As[[i]])
    }
    list(As = As, covariances = covariances, ls = ls, means = means,
         outcomes = outcomes)
  } else {
    means                            <- list()
    for (i in 1:(factorial(K)*(K + 1L))) {
      means[[i]]                     <- as.numeric(components$As[[i]]%*%EZ)
    }
    components$means                 <- means
    components
  }
}

power_all_ma_single <- function(EZ, K, CovZ, u) {
  mvtnorm::pmvnorm(lower = rep(u, K), mean = EZ, sigma = CovZ)[1]
}

power_all_ma_step   <- function(EZ, K, correction, CovZ, uO, components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, components)
  P          <- 0
  for (i in which(Rfast::rowsums(components$outcomes[, K + 1:K]) == K)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

power_any_ma_single <- function(EZ, K, CovZ, u) {
  1 - mvtnorm::pmvnorm(upper = rep(u, K), mean = EZ, sigma = CovZ)[1]
}

power_any_ma_step   <- function(EZ, K, correction, CovZ, uO, components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, components)
  P          <- 0
  for (i in which(Rfast::rowsums(components$outcomes[, K + 1:K]) > 0)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

power_fdr_ma_single <- function(EZ, K, CovZ, u, components) {
  if (missing(components)) {
    components                  <- outcomes_ma_single(K, u)
  }
  two_to_K                      <- 2L^K
  P                             <- numeric(two_to_K)
  for (i in 2:two_to_K) {
    P[i]                        <- mvtnorm::pmvnorm(components$ls[[i]],
                                                    components$us[[i]],
                                                    EZ, sigma = CovZ)[1]
  }
  discoveries                   <- Rfast::rowsums(components$outcomes)
  false_discoveries             <-
    Rfast::rowsums(components$outcomes*matrix(EZ <= 0, two_to_K, K, byrow = T))
  discoveries[discoveries == 0] <- 1
  c(sum(P[-1]), P[two_to_K],
    sapply(1:K, function(k) { sum(P[components$outcomes[, k] > 0]) }),
    sum(P[false_discoveries > 0]), sum(P*false_discoveries/discoveries))
}

power_fdr_ma_step   <- function(EZ, K, correction, CovZ, uO, components) {
  components                    <- outcomes_ma_step(EZ, K, correction, CovZ, uO,
                                                    components)
  seq_K                         <- 1:K
  rows_outcomes                 <- factorial(K)*(K + 1L)
  P                             <- numeric(rows_outcomes)
  rowsums_outcomes              <-
    Rfast::rowsums(components$outcomes[, K + seq_K])
  for (i in which(rowsums_outcomes > 0)) {
    P[i]                        <-
      mvtnorm::pmvnorm(components$ls[[i]], mean  = components$means[[i]],
                       sigma = components$covariances[[i]])[1]
  }
  discoveries                   <- rowsums_outcomes
  false_discoveries             <-
    Rfast::rowsums(components$outcomes[, K + seq_K]*
                     matrix(EZ <= 0, rows_outcomes, K, byrow = T))
  discoveries[discoveries == 0] <- 1
  c(sum(P[rowsums_outcomes > 0]), sum(P[rowsums_outcomes == K]),
    sapply(seq_K, function(k) { sum(P[components$outcomes[, K + k] > 0]) }),
    sum(P[false_discoveries > 0]), sum(P*false_discoveries/discoveries))
}

power_marg_ma_step  <- function(EZ, K, correction, CovZ, uO, power_index,
                                components) {
  components <- outcomes_ma_step(EZ, K, correction, CovZ, uO, components)
  P          <- 0
  for (i in which(components$outcomes[, K + power_index] == 1)) {
    P        <- P + mvtnorm::pmvnorm(components$ls[[i]],
                                     mean  = components$means[[i]],
                                     sigma = components$covariances[[i]])[1]
  }
  P
}

power_ma            <- function(N, K, beta, correction, power, CovZ, u, uO,
                                EZ_div_sqrt_N, power_index, components) {
  if (power == "conjunctive") {
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P <- power_all_ma_single(sqrt(N)*EZ_div_sqrt_N, K, CovZ, u)
    } else {
      P <- power_all_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                             components)
    }
  } else if (power == "disjunctive") {
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P <- power_any_ma_single(sqrt(N)*EZ_div_sqrt_N, K, CovZ, u)
    } else {
      P <- power_any_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                             components)
    }
  } else if (power == "marginal") {
    P   <- power_marg_ma_step(sqrt(N)*EZ_div_sqrt_N, K, correction, CovZ, uO,
                              power_index, components)
  }
  P - (1 - beta)
}

sim_ma_internal     <- function(tau, n, sigma, correction, sigma_z, t_test,
                                pooled, replicates, pi, piO,
                                completed_replicates, total_replicates,
                                summary) {
  summary_i                            <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  K                                    <- length(tau)
  Kp1                                  <- K + 1L
  rej_mat                              <- matrix(0L, replicates, K)
  if (!t_test) {
    denominator                        <- sqrt(sigma_z[1]^2/n[1] +
                                                 sigma_z[-1]^2/n[-1])
    sds                                <- sqrt(sigma^2/n)
  } else {
    seq_Kp1                            <- 1:(K + 1)
    N                                  <- sum(n)
  }
  means                                <- c(0, tau)
  for (i in 1:replicates) {
    if (t_test) {
      X                                <-
        lapply(seq_Kp1, function(k) { stats::rnorm(n[k], means[k], sigma[k]) })
      X_bar                            <-
        sapply(seq_Kp1, function(k) { sum(X[[k]])/n[k] })
      if (pooled) {
        sigma_hat                      <-
          sapply(seq_Kp1,
                 function(k) { sum((X[[k]] - X_bar[k])^2)/(n[k] - 1L) })
      } else {
        X                              <- unlist(X)
        sigma_hat                      <- rep(sum((X - sum(X)/N)^2)/(N - 1),
                                              Kp1)
      }
      pvals                            <-
        stats::pnorm((X_bar[-1] - X_bar[1])/sqrt(sigma_hat[1]^2/n[1] +
                                                   sigma_hat[-1]^2/n[-1]),
                     lower.tail = F)
    } else {
      X_bar                            <- stats::rnorm(Kp1, means, sds)
      pvals                            <-
        stats::pnorm((X_bar[-1] - X_bar[1])/denominator, lower.tail = F)
    }
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      rej_mat[i, ]                     <- (pvals <= pi)
    } else if (correction %in% c("holm", "step_down_dunnett")) {
      order_pvals                      <- order(pvals)
      k                                <- check <- 1
      while (all(k <= K, check == 1)) {
        if (pvals[order_pvals[k]] <= piO[k]) {
          rej_mat[i, order_pvals[k]]   <- rej_mat[i, order_pvals[k]] + 1
          k                            <- k + 1
        } else {
          check                        <- 0
        }
      }
    } else if (correction %in% c("benjamini_hochberg", "hochberg")) {
      order_pvals                      <- order(pvals)
      for (k in K:1) {
        if (pvals[order_pvals[k]] <= piO[k]) {
          rej_mat[i, order_pvals[1:k]] <- rep(1, k)
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
  discoveries                          <- Rfast::rowsums(rej_mat)
  rej_any                              <- sum(discoveries > 0)
  discoveries[discoveries == 0]        <- 1
  c(tau, rej_any/replicates, sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sum(Rfast::rowsums(rej_mat*matrix(tau <= 0, replicates, K,
                                      byrow = T)) > 0)/replicates,
    sum(Rfast::rowsums(rej_mat*matrix(tau <= 0, replicates, K,
                                       byrow = T))/discoveries)/replicates)
}

trace_ma            <- function(n, K, sigma) {
  K*sigma[1]^2/n[1] + sum(sigma[-1]^2/n[-1])
}
