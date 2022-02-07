components_ss_distribution <- function(comp) {
  if (comp$outcome == "norm") {
    VarC             <- comp$sigma[1]^2/comp$n[1]
    VarE             <- comp$sigma[-1]^2/comp$n[-1]
    comp$Var         <- matrix(VarC + VarE, 1)
    sqrt_I           <- sqrt(1/comp$Var[1, ])
    comp$EZ          <- comp$tau*matrix(sqrt_I, comp$nrow_tau, comp$K,
                                        byrow = TRUE)
    Cov_tau          <- matrix(VarC, comp$K, comp$K) + diag(VarE)
    diag_sqrt_I      <- diag(sqrt_I)
    comp$CovZ        <- rep(list(diag_sqrt_I%*%Cov_tau%*%diag_sqrt_I),
                            comp$nrow_tau)
  } else if (comp$outcome %in% c("bern", "pois")) {
    comp$CovZ        <- list()
    comp$Var         <- comp$EZ <- matrix(0, comp$nrow_tau, comp$K)
    if (comp$outcome == "bern") {
      comp$sigma     <- sqrt(comp$pi*(1 - comp$pi))
    } else if (comp$outcome == "pois") {
      comp$sigma     <- sqrt(comp$lambda)
    }
    for (i in 1:comp$nrow_tau) {
      VarC           <- comp$sigma[i, 1]^2/comp$n[1]
      VarE           <- comp$sigma[i, -1]^2/comp$n[-1]
      comp$Var[i, ]  <- VarC + VarE
      sqrt_I         <- sqrt(1/comp$Var[i, ])
      comp$EZ[i, ]   <- comp$tau[i, ]*sqrt_I
      Cov_tau        <- matrix(VarC, comp$K, comp$K) + diag(VarE)
      diag_sqrt_I    <- diag(sqrt_I)
      comp$CovZ[[i]] <- diag_sqrt_I%*%Cov_tau%*%diag_sqrt_I
    }
  }
  comp
}

components_ss_bern         <- function(alpha, beta, correction, delta0, delta1,
                                       integer, K, pi0, power, ratio,
                                       ratio_scenario,
                                       n = c(1, ratio)/(1 + sum(ratio))) {
  list(alpha = alpha, beta = beta, correction = correction, delta0 = delta0,
       delta1 = delta1, integer = integer, K = K, n = n, N = sum(n),
       outcome = "bern", pi0 = pi0, power = power, ratio = ratio,
       ratio_scenario = ratio_scenario, seq_K = 1:K)
}

components_ss_gamma        <- function(comp, i) {
  comp$gamma      <- comp$gammaO <- NA
  if (comp$correction == "benjamini_hochberg") {
    comp$gammaO   <- comp$seq_K*comp$alpha/comp$K
  } else if (comp$correction == "benjamini_yekutieli") {
    comp$gammaO   <- comp$seq_K*comp$alpha/(comp$K*sum(1/comp$seq_K))
  } else if (comp$correction == "bonferroni") {
    comp$gamma    <- comp$alpha/comp$K
  } else if (comp$correction == "dunnett") {
    comp$gamma    <-
      stats::pnorm(mvtnorm::qmvnorm(1 - comp$alpha,
                                    sigma = comp$CovZ[[i]])$quantile,
                   lower.tail = FALSE)
  } else if (comp$correction %in% c("hochberg", "holm_bonferroni")) {
    comp$gammaO   <- comp$alpha/rev(comp$seq_K)
  } else if (comp$correction == "holm_sidak") {
    comp$gammaO   <- 1 - (1 - comp$alpha)^(1/rev(comp$seq_K))
  } else if (comp$correction == "none") {
    comp$gamma    <- comp$alpha
  } else if (comp$correction == "sidak") {
    comp$gamma    <- 1 - (1 - comp$alpha)^(1/comp$K)
  } else if (comp$correction == "step_down_dunnett") {
    Ksq           <- comp$K^2
    if (length(unique(comp$CovZ[[i]][(1:Ksq)[-seq(1, Ksq, comp$K + 1)]])) > 1) {
      stop("The step-down Dunnett correction is only supported for scenarios ",
           "with a shared covariance between the test statistics")
    }
    one_min_alpha <- 1 - comp$alpha
    comp$gammaO   <-
      sapply(comp$seq_K,
             function(k) {
               dim <- comp$K - (k - 1L)
               stats::pnorm(
                 mvtnorm::qmvnorm(one_min_alpha,
                                  sigma =
                                    matrix(comp$CovZ[[i]][2, 1], dim, dim) +
                                    (1 - comp$CovZ[[i]][2, 1])*
                                    diag(dim))$quantile,
                 lower.tail = FALSE) })
  }
  comp$u          <- stats::qnorm(1 - comp$gamma)
  comp$uO         <- stats::qnorm(1 - comp$gammaO)
  comp
}

components_ss_norm         <- function(alpha, beta, correction, delta0, delta1,
                                       integer, K, power, ratio, sigma,
                                       n = c(1, ratio)/(1 + sum(ratio))) {
  list(alpha = alpha, beta = beta, correction = correction, delta0 = delta0,
       delta1 = delta1, integer = integer, K = K, n = n, N = sum(n),
       outcome = "norm", power = power, ratio = ratio, seq_K = 1:K,
       sigma = sigma)
}

components_ss_pois         <- function(alpha, beta, correction, delta0, delta1,
                                       integer, K, lambda0, power, ratio,
                                       ratio_scenario,
                                       n = c(1, ratio)/(1 + sum(ratio))) {
  list(alpha = alpha, beta = beta, correction = correction, delta0 = delta0,
       delta1 = delta1, integer = integer, K = K, lambda0 = lambda0, n = n,
       N = sum(n), outcome = "pois", power = power, ratio = ratio,
       ratio_scenario = ratio_scenario, seq_K = 1:K)
}

components_ss_power_setup  <- function(comp) {
  comp$tau_power                      <- rep(comp$delta1, comp$K)
  comp$power_index                    <- NA
  if (comp$power == "marginal") {
    if (comp$outcome == "norm") {
      Var                             <-
        comp$sigma[1]^2/comp$n[1] + comp$sigma[-1]^2/comp$n[-1]
      if (length(unique(Var)) == 1) {
        comp$power_index              <- 1
      } else if (comp$correction %in% c("holm_bonferroni", "holm_sidak",
                                        "step_down_dunnett")) {
        comp$power_index              <- which(Var == max(Var))[1]
      } else if (comp$correction %in% c("benjamini_hochberg",
                                        "benjamini_yekutieli", "hochberg")) {
        comp$power_index              <- which(Var == min(Var))[1]
      } else if (comp$correction %in%
                 c("bonerroni", "dunnett", "none", "sidak")) {
        comp$power_index              <- which(Var == max(Var))[1]
      }
    } else if (comp$outcome %in% c("bern", "pois")) {
      comp$power_index                <- 1
    }
    comp$tau_power[-comp$power_index] <- comp$delta0
  }
  comp$tau                            <- matrix(comp$tau_power, 1)
  comp$nrow_tau                       <- 1
  if (comp$outcome == "bern") {
    comp$pi_power                     <- comp$pi0 + c(0, comp$tau_power)
    comp$pi                           <- matrix(comp$pi_power, 1)
    comp$nrow_pi                      <- 1
  } else if (comp$outcome == "pois") {
    comp$lambda_power                 <- comp$lambda0 + c(0, comp$tau_power)
    comp$lambda                       <- matrix(comp$lambda_power, 1)
    comp$nrow_lambda                  <- 1
  }
  comp
}

components_ss_single       <- function(comp) {
  comp$outcomes                 <- iterpc::getall(iterpc::iterpc(2, comp$K, 0:1,
                                                                 TRUE, TRUE))
  rej                           <- (comp$outcomes == 1)
  comp$nrow_outcomes            <- 2^comp$K
  comp$discoveries              <- Rfast::rowsums(comp$outcomes)
  comp$inv_outcomes             <- 1 - comp$outcomes
  comp$non_discoveries          <- comp$K - comp$discoveries
  comp                          <- components_ss_gamma(comp, 1)
  for (i in 1:comp$nrow_tau) {
    comp$ls[[i]]                <- matrix(-Inf, comp$nrow_outcomes, comp$K)
    comp$us[[i]]                <- matrix(Inf, comp$nrow_outcomes, comp$K)
    if (all(i > 1, comp$correction == "dunnett", comp$outcome == "bern")) {
      comp                      <- components_ss_gamma(comp, i)
    }
    comp$us[[i]][1, ]           <- comp$ls[[i]][comp$nrow_outcomes, ] <-
      rep(comp$u, comp$K)
    for (j in 2:(comp$nrow_outcomes - 1)) {
      comp$ls[[i]][j, rej[j, ]] <- comp$us[[i]][j, !rej[j, ]]         <- comp$u
    }
  }
  comp
}

components_ss_step         <- function(comp, update = FALSE) {
  if (!update) {
    rankings                                    <-
      iterpc::getall(iterpc::iterpc(comp$K, comp$K, ordered = TRUE))
    nrow_rankings                               <- factorial(comp$K)
    fact                                        <-
      c(comp$seq_K, comp$K + 1)*nrow_rankings
    comp$nrow_outcomes                          <- fact[comp$K + 1]
    comp$outcomes                               <-
      matrix(0L, comp$nrow_outcomes, 2*comp$K)
    comp$outcomes[1:nrow_rankings, comp$seq_K]  <- rankings
    for (k in comp$seq_K[-comp$K]) {
      range                                     <- (1 + fact[k]):fact[k + 1]
      comp$outcomes[range, comp$seq_K]          <- rankings
      for (i in range) {
        comp$outcomes[i, comp$K + which(comp$outcomes[i, comp$seq_K] %in%
                                          1:k)] <- 1L
      }
    }
    range                                       <-
      (1 + fact[comp$K]):fact[comp$K + 1]
    comp$outcomes[range, comp$seq_K]            <- rankings
    comp$outcomes[range, -comp$seq_K]           <- 1L
    comp$discoveries                            <- rep(c(0, comp$seq_K),
                                                       each = nrow_rankings)
    comp$inv_outcomes                           <-
      1 - comp$outcomes[, -comp$seq_K]
    comp$non_discoveries                        <- comp$K - comp$discoveries
    As                                          <- comp$Lambdas <- comp$ls <-
      comp$means   <- tAs <- list()
    for (i in 1:comp$nrow_outcomes) {
      sum_i                                     <-
        sum(comp$outcomes[i, comp$K + comp$seq_K])
      if (comp$correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
        As[[i]]                                 <-
          matrix(0L, comp$K - 1 + sum_i + as.numeric(sum_i < comp$K), comp$K)
      } else if (comp$correction %in% c("benjamini_hochberg",
                                        "benjamini_yekutieli", "hochberg")) {
        As[[i]]                                 <-
          matrix(0L, 2*comp$K - 1 - sum_i + as.numeric(sum_i > 0), comp$K)
      }
      for (k in comp$seq_K[-comp$K]) {
        As[[i]][k, which(comp$outcomes[i, comp$seq_K] == k)]        <- 1L
        As[[i]][k, which(comp$outcomes[i, comp$seq_K] == k + 1)]    <- -1L
      }
      if (comp$correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
        if (sum_i > 0) {
          for (k in 1:sum_i) {
            As[[i]][comp$K - 1 + k,
                    which(comp$outcomes[i, comp$seq_K] == k)]       <- 1L
          }
        }
        if (sum_i < comp$K) {
          As[[i]][comp$K + sum_i,
                  which(comp$outcomes[i, comp$seq_K] == sum_i + 1)] <- -1L
        }
      } else if (comp$correction %in% c("benjamini_hochberg",
                                        "benjamini_yekutieli", "hochberg")) {
        if (sum_i < comp$K) {
          for (k in (sum_i + 1):comp$K) {
            As[[i]][comp$K - 1 + k - sum_i,
                    which(comp$outcomes[i, comp$seq_K] == k)]       <- -1L
          }
        }
        if (sum_i > 0) {
          As[[i]][2*comp$K - sum_i,
                  which(comp$outcomes[i, comp$seq_K] == sum_i)]     <- 1L
        }
      }
      tAs[[i]]                                  <- t(As[[i]])
    }
    comp$As <- As
    comp                                        <- components_ss_gamma(comp, 1)
    for (i in 1:comp$nrow_tau) {
      comp$ls[[i]]                              <- comp$means[[i]]  <-
        comp$Lambdas[[i]] <- list()
      if (all(i > 1, comp$correction == "step_down_dunnett",
              comp$outcome %in% c("bern", "pois"))) {
        comp                                    <- components_ss_gamma(comp, i)
      }
      for (j in 1:comp$nrow_outcomes) {
        sum_j                                   <-
          sum(comp$outcomes[j, comp$K + comp$seq_K])
        if (comp$correction %in% c("holm_bonferroni", "holm_sidak",
                                   "step_down_dunnett")) {
          if (sum_j < comp$K) {
            comp$ls[[i]][[j]]                   <-
              c(numeric(comp$K - 1), comp$uO[seq_len(sum_j)],
                -comp$uO[sum_j + 1])
          } else {
            comp$ls[[i]][[j]]                   <- c(numeric(comp$K - 1),
                                                     comp$uO)
          }
        } else if (comp$correction %in% c("benjamini_hochberg",
                                          "benjamini_yekutieli", "hochberg")) {
          if (sum_j > 0) {
            if (sum_j < comp$K) {
              comp$ls[[i]][[j]]                 <-
                c(numeric(comp$K - 1), -comp$uO[(sum_j + 1):comp$K],
                  comp$uO[sum_j])
            } else {
              comp$ls[[i]][[j]]                 <- c(numeric(comp$K - 1),
                                                     comp$uO[comp$K])
            }
          } else {
            comp$ls[[i]][[j]]                   <- c(numeric(comp$K - 1),
                                                     -comp$uO)
          }
        }
        comp$means[[i]][[j]]                    <-
          as.numeric(As[[j]]%*%comp$EZ[i, ])
        comp$Lambdas[[i]][[j]]                  <-
          As[[j]]%*%comp$CovZ[[i]]%*%tAs[[j]]
      }
    }
  } else {
    for (i in 1:comp$nrow_tau) {
      for (j in 1:comp$nrow_outcomes) {
        comp$means[[i]][[j]]                    <-
          as.numeric(comp$As[[j]]%*%comp$EZ[i, ])
      }
    }
  }
  comp
}

components_ss_update       <- function(comp, tau, pi, lambda) {
  if (!missing(tau)) {
    comp$tau           <- tau
    comp$nrow_tau      <- comp$K + 2
    if (comp$outcome == "bern") {
      comp$pi          <- pi
      comp$nrow_pi     <- comp$nrow_tau
    } else if (comp$outcome == "pois") {
      comp$lambda      <- lambda
      comp$nrow_lambda <- comp$nrow_tau
    }
  }
  comp                 <- components_ss_distribution(comp)
  if (comp$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
    comp               <- components_ss_single(comp)
  } else {
    comp               <- components_ss_step(comp)
  }
  comp
}

covariance_ss              <- function(K, n, sigma, standardise = FALSE) {
  if (standardise) {
    CovTau      <- matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
    diag_sqrt_I <- diag(1/sqrt(diag(CovTau)))
    diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  } else {
    matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1])
  }
}

determinant_ss             <- function(n, K, sigma) {
  sum(sapply(1:(K + 1), function(k) { n[k]*prod(sigma[-k]^2) }))/prod(n)
}

integer_ss                 <- function(comp) {
  if (comp$integer) {
    comp$n <- ceiling(comp$n)
    comp$N <- sum(comp$n)
    comp   <- components_ss_update(comp)
  }
  comp
}

max_eigenvalue_ss          <- function(n, K, sigma) {
  max(eigen(matrix(sigma[1]^2/n[1], K, K) + diag(sigma[-1]^2/n[-1]))$values)
}

opchar_ss_internal         <- function(comp) {
  opchar           <- matrix(0, comp$nrow_tau, 3*comp$K + 8)
  if (comp$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
    for (i in 1:comp$nrow_tau) {
      opchar[i, ]  <- opchar_ss_single(i, comp)
    }
  } else {
    for (i in 1:comp$nrow_tau) {
      opchar[i, ]  <- opchar_ss_step(i, comp)
    }
  }
  if (comp$outcome == "norm") {
    opchar         <- cbind(comp$tau, opchar)
    fact           <- paste0("tau", comp$seq_K)
  } else if (comp$outcome == "bern") {
    opchar         <- cbind(comp$pi, opchar)
    fact           <- paste0("pi", c(0, comp$seq_K))
  } else if (comp$outcome == "pois") {
    opchar         <- cbind(comp$lambda, opchar)
    fact           <- paste0("lambda", c(0, comp$seq_K))
  }
  colnames(opchar) <- c(fact, "Pdis", "Pcon", paste0("P", comp$seq_K),
                        paste0("FWERI", comp$seq_K),
                        paste0("FWERII", comp$seq_K), "PHER", "FDR", "pFDR",
                        "FNDR", "Sens", "Spec")
  comp$opchar      <- tibble::as_tibble(opchar, .name_repair = "minimal")
  comp
}

opchar_ss_single           <- function(i, comp) {
  P                                               <- numeric(comp$nrow_outcomes)
  for (j in 1:comp$nrow_outcomes) {
    P[j]                                          <-
      mvtnorm::pmvnorm(comp$ls[[i]][j, ], comp$us[[i]][j, ], comp$EZ[i, ],
                       sigma = comp$CovZ[[i]])[1]
  }
  P[is.nan(P)] <- 0
  P            <- P/sum(P)
  neg_mat                                         <-
    matrix(comp$EZ[i, ] <= 0, comp$nrow_outcomes, comp$K, byrow = TRUE)
  pos_mat                                         <- !neg_mat
  false_discoveries                               <-
    Rfast::rowsums(comp$outcomes*neg_mat)
  false_non_discoveries                           <-
    Rfast::rowsums(comp$inv_outcomes*pos_mat)
  which_discoveries                               <- which(comp$discoveries > 0)
  comp$discoveries[-which_discoveries]            <- 1
  comp$non_discoveries[comp$non_discoveries == 0] <- 1
  pFDR                                            <-
    sum(P[which_discoveries]*false_discoveries[which_discoveries]/
          comp$discoveries[which_discoveries])/sum(P[which_discoveries])
  if (is.nan(pFDR)) {
    pFDR                                          <- 0
  }
  c(sum(P[-1]), P[comp$nrow_outcomes],
    sapply(comp$seq_K, function(k) { sum(P[comp$outcomes[, k] > 0]) }),
    sapply(comp$seq_K, function(k) { sum(P[false_discoveries >= k]) }),
    sapply(comp$seq_K, function(k) { sum(P[false_non_discoveries >= k]) }),
    sum(P*false_discoveries)/comp$K,
    sum(P*false_discoveries/comp$discoveries),
    pFDR,
    sum(P*false_non_discoveries/comp$non_discoveries),
    sum(P*Rfast::rowsums(comp$outcomes*pos_mat))/max(sum(comp$EZ[i, ] > 0), 1),
    sum(P*Rfast::rowsums(comp$inv_outcomes*neg_mat))/
      max(sum(comp$EZ[i, ] <= 0), 1))
}

opchar_ss_step             <- function(i, comp) {
  P                                               <- numeric(comp$nrow_outcomes)
  for (j in 1:comp$nrow_outcomes) {
    P[j]                                          <-
      mvtnorm::pmvnorm(comp$ls[[i]][[j]], mean  = comp$means[[i]][[j]],
                       sigma = comp$Lambdas[[i]][[j]])[1]
  }
  P[is.nan(P)] <- 0
  P            <- P/sum(P)
  neg_mat                                         <-
    matrix(comp$EZ[i, ] <= 0, comp$nrow_outcomes, comp$K, byrow = TRUE)
  pos_mat                                         <- !neg_mat
  false_discoveries                               <-
    Rfast::rowsums(comp$outcomes[, -comp$seq_K]*neg_mat)
  false_non_discoveries                           <-
    Rfast::rowsums(comp$inv_outcomes*pos_mat)
  which_discoveries                               <- which(comp$discoveries > 0)
  comp$discoveries[-which_discoveries]            <- 1
  comp$non_discoveries[comp$non_discoveries == 0] <- 1
  pFDR                                            <-
    sum(P[which_discoveries]*false_discoveries[which_discoveries]/
          comp$discoveries[which_discoveries])/sum(P[which_discoveries])
  if (is.nan(pFDR)) {
    pFDR                                          <- 0
  }
  c(sum(P[which_discoveries]), sum(P[comp$discoveries == comp$K]),
    sapply(comp$seq_K, function(k) { sum(P[comp$outcomes[, comp$K + k] > 0]) }),
    sapply(comp$seq_K, function(k) { sum(P[false_discoveries >= k]) }),
    sapply(comp$seq_K, function(k) { sum(P[false_non_discoveries >= k]) }),
    sum(P*false_discoveries)/comp$K,
    sum(P*false_discoveries/comp$discoveries),
    pFDR,
    sum(P*false_non_discoveries/comp$non_discoveries),
    sum(P*Rfast::rowsums(comp$outcomes[, -comp$seq_K]*pos_mat))/
      max(sum(comp$EZ[i, ] > 0), 1),
    sum(P*Rfast::rowsums(comp$inv_outcomes*neg_mat))/
      max(sum(comp$EZ[i, ] <= 0), 1))
}

power_ss_single_all        <- function(comp) {
  mvtnorm::pmvnorm(lower = rep(comp$u, comp$K), mean = comp$EZ[1, ],
                   sigma = comp$CovZ[[1]])[1]
}

power_ss_single_any        <- function(comp) {
  1 - mvtnorm::pmvnorm(upper = rep(comp$u, comp$K), mean = comp$EZ[1, ],
                       sigma = comp$CovZ[[1]])[1]
}

power_ss_step_all          <- function(comp) {
  P   <- 0
  for (i in which(comp$discoveries == comp$K)) {
    P <- P + mvtnorm::pmvnorm(comp$ls[[1]][[i]], mean = comp$means[[1]][[i]],
                              sigma = comp$Lambdas[[1]][[i]])[1]
  }
  P
}

power_ss_step_any          <- function(comp) {
  P   <- 0
  for (i in which(comp$discoveries > 0)) {
    P <- P + mvtnorm::pmvnorm(comp$ls[[1]][[i]], mean = comp$means[[1]][[i]],
                              sigma = comp$Lambdas[[1]][[i]])[1]
  }
  P
}

power_ss_step_marg         <- function(comp) {
  P   <- 0
  for (i in which(comp$outcomes[, comp$K + comp$power_index] == 1)) {
    P <- P + mvtnorm::pmvnorm(comp$ls[[1]][[i]], mean = comp$means[[1]][[i]],
                              sigma = comp$Lambdas[[1]][[i]])[1]
  }
  P
}

root_power_ss              <- function(N, comp) {
  comp$N <- N
  comp$n <- comp$N*c(1, comp$ratio)/(1 + sum(comp$ratio))
  comp   <- components_ss_distribution(comp)
  if (!(comp$correction %in% c("bonferroni", "dunnett", "none", "sidak"))) {
    comp <- components_ss_step(comp, TRUE)
  }
  if (comp$power == "conjunctive") {
    if (comp$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P  <- power_ss_single_all(comp)
    } else {
      P  <- power_ss_step_all(comp)
    }
  } else if (comp$power == "disjunctive") {
    if (comp$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      P  <- power_ss_single_any(comp)
    } else {
      P  <- power_ss_step_any(comp)
    }
  } else if (comp$power == "marginal") {
    P    <- power_ss_step_marg(comp)
  }
  P - (1 - comp$beta)
}

sample_size_ss             <- function(comp) {
  if (all(comp$power == "marginal",
          comp$correction %in% c("bonferroni", "dunnett", "none", "sidak"))) {
    comp$N <- ((comp$u + stats::qnorm(1 - comp$beta))*
                 sqrt(comp$Var[1, comp$power_index])/comp$delta1)^2
  } else {
    Nmax   <- 3*((stats::qnorm(1 - comp$alpha/comp$K) +
                    stats::qnorm(1 - comp$beta))*
                   sqrt(max(comp$Var[1, ]))/comp$delta1)^2
    comp$N <- stats::uniroot(f = root_power_ss, interval = c(1e-16, Nmax),
                             comp = comp)$root
  }
  comp$n   <- comp$N*comp$n
  components_ss_update(comp)
}

sim_ss_bern_internal       <- function(pi, alpha, completed_replicates,
                                       correction, gamma, gammaO, n, replicates,
                                       summary, total_replicates) {
  summary_i                             <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  K                                     <- length(pi) - 1
  Kp1                                   <- K + 1
  rej_mat                               <- matrix(0L, replicates, K)
  seq_K                                 <- 1:K
  seq_Kp1                               <- c(seq_K, Kp1)
  one_min_alpha                         <- 1 - alpha
  for (i in 1:replicates) {
    pi_hat                              <- stats::rbinom(Kp1, n, pi)/n
    if (correction %in% c("dunnett", "step_down_dunnett")) {
      sigma                             <- sqrt(pi_hat*(1 - pi_hat))
      pvals                             <-
        stats::pnorm((pi_hat[-1] - pi_hat[1])/
                       sqrt(sigma[-1]^2/n[-1] + sigma[1]^2/n[1]),
                     lower.tail = FALSE)
    } else {
      pvals                             <-
        stats::pnorm((pi_hat[-1] - pi_hat[1])/
                       sqrt(pi_hat[-1]*(1 - pi_hat[-1])/n[-1] +
                              pi_hat[1]*(1 - pi_hat[1])/n[1]),
                     lower.tail = FALSE)
    }
    if (sum(is.na(pvals)) > 0) {
      pvals[which(is.na(pvals))]        <- 0.5
    }
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      if (correction == "dunnett") {
        gamma                           <-
          stats::pnorm(mvtnorm::qmvnorm(1 - alpha,
                                        sigma = covariance_ss(K, n, sigma,
                                                              TRUE))$quantile,
                       lower.tail = FALSE)
      }
      rej_mat[i, ]                      <- (pvals <= gamma)
    } else if (correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
      if (correction == "step_down_dunnett") {
        corr                            <- covariance_ss(K, n, sigma,
                                                         TRUE)[2, 1]
        gammaO                          <-
          sapply(seq_K,
                 function(k) {
                   dim                  <- Kp1 - k
                   stats::pnorm(
                     mvtnorm::qmvnorm(one_min_alpha,
                                      sigma = matrix(corr, dim, dim) +
                                                (1 - corr)*diag(dim))$quantile,
                     lower.tail = FALSE) })
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
      message("..approximately ",
              10*which(summary_i == (completed_replicates + i)),
              "% through the required simulations..")
    }
  }
  discoveries                           <- Rfast::rowsums(rej_mat)
  non_discoveries                       <- K - discoveries
  neg_mat                               <- matrix(pi[-1] <= pi[1],
                                                  replicates, K, byrow = TRUE)
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
                                byrow = TRUE))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(pi[-1] > pi[1]), 1)*
                                            replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(pi[-1] <= pi[1]), 1)*replicates))
}

sim_ss_norm_internal       <- function(tau, completed_replicates, correction,
                                       gamma, gammaO, n, pooled, replicates,
                                       sigma, sigma_z, summary, t_test,
                                       total_replicates) {
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
                     lower.tail = FALSE)
    } else {
      X_bar                             <- stats::rnorm(Kp1, means, sds)
      pvals                             <-
        stats::pnorm((X_bar[-1] - X_bar[1])/denominator, lower.tail = FALSE)
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
      message("..approximately ",
              10*which(summary_i == (completed_replicates + i)),
              "% through the required simulations..")
    }
  }
  discoveries                           <- Rfast::rowsums(rej_mat)
  non_discoveries                       <- K - discoveries
  neg_mat                               <- matrix(tau <= 0, replicates, K,
                                                  byrow = TRUE)
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
                                byrow = TRUE))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(tau > 0), 1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(tau <= 0), 1)*replicates))
}

sim_ss_pois_internal       <- function(lambda, alpha, completed_replicates,
                                       correction, gamma, gammaO, n, replicates,
                                       summary, total_replicates) {
  summary_i                             <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  K                                     <- length(pi) - 1
  Kp1                                   <- K + 1
  rej_mat                               <- matrix(0L, replicates, K)
  seq_K                                 <- 1:K
  seq_Kp1                               <- c(seq_K, Kp1)
  one_min_alpha                         <- 1 - alpha
  for (i in 1:replicates) {
    lambda_hat                          <- stats::rpois(Kp1, n*lambda)/n
    if (correction %in% c("dunnett", "step_down_dunnett")) {
      sigma                             <- sqrt(lambda_hat)
      pvals                             <-
        stats::pnorm((lambda_hat[-1] - lambda_hat[1])/
                       sqrt(sigma[-1]^2/n[-1] + sigma[1]^2/n[1]),
                     lower.tail = FALSE)
    } else {
      pvals                             <-
        stats::pnorm((lambda_hat[-1] - lambda_hat[1])/
                       sqrt(lambda_hat[-1]/n[-1] + lambda_hat[1]/n[1]),
                     lower.tail = FALSE)
    }
    if (sum(is.na(pvals)) > 0) {
      pvals[which(is.na(pvals))]        <- 0.5
    }
    if (correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      if (correction == "dunnett") {
        gamma                           <-
          stats::pnorm(mvtnorm::qmvnorm(1 - alpha,
                                        sigma = covariance_ss(K, n, sigma,
                                                              TRUE))$quantile,
                       lower.tail = FALSE)
      }
      rej_mat[i, ]                      <- (pvals <= gamma)
    } else if (correction %in% c("holm_bonferroni", "holm_sidak",
                                 "step_down_dunnett")) {
      if (correction == "step_down_dunnett") {
        corr                            <- covariance_ss(K, n, sigma,
                                                         TRUE)[2, 1]
        gammaO                          <-
          sapply(seq_K,
                 function(k) {
                   dim                  <- Kp1 - k
                   stats::pnorm(
                     mvtnorm::qmvnorm(one_min_alpha,
                                      sigma = matrix(corr, dim, dim) +
                                        (1 - corr)*diag(dim))$quantile,
                     lower.tail = FALSE) })
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
      message("..approximately ",
              10*which(summary_i == (completed_replicates + i)),
              "% through the required simulations..")
    }
  }
  discoveries                           <- Rfast::rowsums(rej_mat)
  non_discoveries                       <- K - discoveries
  neg_mat                               <- matrix(lambda[-1] <= lambda[1],
                                                  replicates, K, byrow = TRUE)
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
                         matrix(lambda[-1] <= lambda[1],
                                length(which_discoveries), K, byrow = TRUE))/
          discoveries[which_discoveries])/length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(lambda[-1] > lambda[1]), 1)*
                                            replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(lambda[-1] <= lambda[1]), 1)*replicates))
}
