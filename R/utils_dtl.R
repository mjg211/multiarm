components_dtl_all                 <- function(comp) {
  comp                                          <-
    components_dtl_covariances_sqrt_Is(comp)
  Lambda_null <- Lambda       <- matrix(0, comp$JK, comp$JK)
  As                                            <- diag_sqrt_Is <- Lambdas <-
    lowers       <- means   <-
    uppers       <- list()
  for (j1 in comp$seq_J) {
    diag_sqrt_Is[[j1]]                          <-
      diag(comp$sqrt_I[[1]][, comp$seq_js[[j1]]])
    fact                                        <-
      diag_sqrt_Is[[j1]]%*%comp$Cov_taus[[1]][[j1]]
    Lambda[comp$seq_js[[j1]],
           comp$seq_js[[j1]]]                   <- fact%*%diag_sqrt_Is[[j1]]
    for (j2 in seq_len(j1 - 1)) {
      Lambda[comp$seq_js[[j1]],
             comp$seq_js[[j2]]]                 <-
        Lambda[comp$seq_js[[j2]],
               comp$seq_js[[j1]]]               <- fact%*%diag_sqrt_Is[[j2]]
    }
  }
  ordering                                      <-
    iterpc::getall(iterpc::iterpc(n = comp$K, r = comp$K,
                                  labels  = comp$seq_K, ordered = TRUE))
  nrow_ordering                                 <- nrow(ordering)
  nrow_outcomes                                 <-
    nrow_ordering*(comp$KvJ + 1)
  outcomes                                      <- matrix(0L, nrow_outcomes,
                                                          2*comp$K)
  for (i in 1:(comp$KvJ + 1)) {
    fact                                        <- (i - 1)*nrow_ordering
    outcomes[(1 + fact):(fact + nrow_ordering), comp$seq_K]                   <-
      ordering
    for (j in 1:nrow_ordering) {
      outcomes[fact + j, comp$K + order(ordering[j, ], decreasing = TRUE)]   <-
        c(integer(comp$Kp1 - i), rep(1L, i - 1))
    }
  }
  conditions                                    <-
    sum(comp$Kv) - comp$J + 1 + as.numeric(comp$KvJ > 1)*(comp$KvJ - 1)
  lowers_i                                      <- numeric(conditions)
  uppers_i                                      <- rep(Inf, conditions)
  means_1                                       <-
    rep(comp$tau[1, ], comp$J)*comp$sqrt_Is[[1]][1, ]
  for (i in 1:nrow_outcomes) {
    A                                           <- matrix(0L, conditions, comp$JK)
    counter                                     <- 1L
    for (j in comp$seq_J[-comp$J]) {
      dropped                                   <- comp$Kv[j]:(comp$Kv[j + 1] + 1)
      len_dropped                               <- length(dropped)
      fact                                      <- comp$Kv[1]*(j - 1)
      if (len_dropped > 1) {
        for (cond in 1:(comp$Kv[j] - comp$Kv[j + 1] - 1)) {
          A[counter,
            fact + which(outcomes[i, comp$seq_K] == dropped[cond + 1])]       <- 1L
          A[counter,
            fact + which(outcomes[i, comp$seq_K] == dropped[cond])]           <- -1L
          counter                               <- counter + 1L
        }
      }
      continue                                  <- comp$Kv[j + 1]:1
      for (cond in rev(continue)) {
        A[counter,
          fact + which(outcomes[i, comp$seq_K] == continue[cond])]            <- 1L
        A[counter,
          fact + which(outcomes[i, comp$seq_K] == dropped[len_dropped])]      <- -1L
        counter                                 <- counter + 1L
      }
    }
    fact                                        <- comp$K*(comp$J - 1)
    if (comp$KvJ > 1) {
      condition                                 <- matrix(0L, comp$KvJ - 1, comp$JK)
      for (cond in 1:(comp$KvJ - 1)) {
        A[counter, fact + which(outcomes[i, comp$seq_K] == comp$KvJ - cond)]     <- 1L
        A[counter, fact + which(outcomes[i, comp$seq_K] == comp$KvJ - cond + 1)] <- -1L
        counter                                 <- counter + 1L
      }
    }
    for (k in 1:comp$KvJ) {
      A[counter, fact + which(outcomes[i, comp$seq_K] == k)]                  <- 1L
      counter                                   <- counter + 1L
    }
    As[[i]]                                     <- A
    Lambdas[[i]]                                <- A%*%Lambda%*%t(A)
    means[[i]]                                  <- as.numeric(A%*%means_1)
    lowers[[i]]                                 <- lowers_i
    uppers[[i]]                                 <- uppers_i
    for (k in 1:comp$KvJ) {
      fact                                      <- conditions - comp$KvJ + k
      if (outcomes[i, comp$K + which(outcomes[i, comp$seq_K] == k)] == 1L) {
        lowers[[i]][fact]                       <- comp$e
      } else {
        lowers[[i]][fact]                       <- -Inf
        uppers[[i]][fact]                       <- comp$e
      }
    }
  }
  mod_discoveries                               <- discoveries     <-
    Rfast::rowsums(outcomes[, comp$K + comp$seq_K, drop = FALSE])
  mod_non_discoveries                           <- non_discoveries <-
    comp$K - discoveries
  disjunctive                                   <- which(discoveries > 0)
  mod_discoveries[-disjunctive]                 <- 1L
  mod_non_discoveries[mod_non_discoveries == 0] <- 1L
  comp$As                  <- As
  comp$conjunctive         <- which(discoveries == comp$K)
  comp$discoveries         <- discoveries
  comp$disjunctive         <- disjunctive
  comp$inv_psi             <- (outcomes[, comp$K + comp$seq_K] == 0)
  comp$Lambda_null         <- Lambda_null
  comp$Lambdas             <- Lambdas
  comp$lowers              <- lowers
  comp$means               <- comp$means
  comp$mod_discoveries     <- mod_discoveries
  comp$mod_non_discoveries <- mod_non_discoveries
  comp$non_discoveries     <- non_discoveries
  comp$nrow_outcomes       <- nrow_outcomes
  comp$outcomes            <- cbind(outcomes, integer(nrow_outcomes))
  comp$uppers              <- uppers
  comp
}

components_dtl_all_update          <- function(comp, i) {
  if (comp$outcome %in% c("bern", "norm")) {
    Lambda                            <- comp$Lambda_null
    diag_sqrt_Is                      <- list()
    for (j1 in comp$seq_J) {
      diag_sqrt_Is[[j1]]              <-
        diag(comp$sqrt_Is[[i]][, comp$seq_js[[j1]]])
      fact                            <-
        diag_sqrt_Is[[j1]]%*%comp$Cov_taus[[i]][[j1]]
      Lambda[comp$seq_js[[j1]],
             comp$seq_js[[j1]]]       <- fact%*%diag_sqrt_Is[[j1]]
      for (j2 in seq_len(j1 - 1)) {
        Lambda[comp$seq_js[[j1]],
               comp$seq_js[[j2]]]     <-
          Lambda[comp$seq_js[[j2]],
                 comp$seq_js[[j1]]]   <- fact%*%diag_sqrt_Is[[j2]]
      }
    }
    means_i                           <-
      rep(comp$tau[i, ], comp$J)*comp$sqrt_Is[[i]][1, ]
    for (i in 1:comp$nrow_outcomes) {
      comp$Lambdas[[i]]               <- comp$As[[i]]%*%Lambda%*%t(comp$As[[i]])
      comp$means[[i]]                 <- as.numeric(comp$As[[i]]%*%means_i)
    }
  } else {
    means_i                           <-
      rep(comp$tau[i, ], comp$J)*comp$sqrt_Is[[i]][1, ]
    for (i in 1:comp$nrow_outcomes) {
      comp$means[[i]]                 <- as.numeric(comp$As[[i]]%*%means_i)
    }
  }
  comp
}

components_dtl_covariances_sqrt_Is <- function(comp) {
  Cov_taus                               <- diag_sqrt_Is <- sqrt_Is <- list()
  sqrt_Is_null                           <- matrix(0, 1, comp$JK)
  if (comp$type == "variable") {
    NC                                   <-
      comp$n_factor*comp$spacing/comp$spacing[1]
  } else if (comp$type == "fixed") {
    fact                                 <-
      (comp$spacing - c(0, comp$spacing[-comp$J]))/comp$spacing[1]
    NC                                   <-
      cumsum(comp$n_factor*fact/(comp$r*comp$Kv + 1))
  }
  NE                                     <- comp$r*NC
  if (comp$outcome == "norm") {
    Cov_taus[[1]]                        <- list()
    sqrt_Is[[1]]                         <- sqrt_Is_null
    for (j in comp$seq_J) {
      VarC                               <- comp$sigma[1, 1]^2/NC[j]
      VarE                               <- comp$sigma[1, -1]^2/NE[j]
      sqrt_Is[[1]][, comp$seq_js[[j]]]   <- sqrt(1/(VarC + VarE))
      diag_sqrt_Is[[j]]                  <-
        diag(sqrt_Is[[1]][, comp$seq_js[[j]]])
      Cov_taus[[1]][[j]]                 <- matrix(VarC, comp$K, comp$K) +
        diag(VarE)
    }
    if (nrow(comp$sigma) > 1) {
      for (i in 1:nrow(comp$sigma)) {
        Cov_taus[[i]]                    <- Cov_taus[[1]]
        sqrt_Is[[i]]                     <- sqrt_Is[[1]]
      }
    }
  } else if (comp$outcome %in% c("bern", "pois")) {
    for (i in 1:nrow(comp$sigma)) {
      Cov_taus[[i]]                      <- list()
      sqrt_Is[[i]]                       <- sqrt_Is_null
      for (j in comp$seq_J) {
        VarC                             <- comp$sigma[i, 1]^2/NC[j]
        VarE                             <- comp$sigma[i, -1]^2/NE[j]
        sqrt_Is[[i]][, comp$seq_js[[j]]] <- sqrt(1/(VarC + VarE))
        diag_sqrt_Is[[j]]                <-
          diag(sqrt_Is[[i]][, comp$seq_js[[j]]])
        Cov_taus[[i]][[j]]               <- matrix(VarC, comp$K, comp$K) +
          diag(VarE)
      }
    }
  }
  comp$Cov_taus                          <- Cov_taus
  comp$sqrt_Is                           <- sqrt_Is
  comp
}

components_dtl_hg_lfc              <- function(comp) {
  comp$tau_power                                 <-
    c(rep(comp$delta1, comp$c), rep(comp$delta0, comp$K - comp$c))
  comp$tau                                       <- rbind(rep(0, comp$K),
                                                          comp$tau_power)
  comp$nrow_tau                                  <- 2
  if (comp$outcome == "bern") {
    comp$pi                                      <- comp$pi0 + cbind(0,
                                                                     comp$tau)
    comp$nrow_pi                                 <- 2
    comp$sigma                                   <- sqrt(comp$pi*(1 - comp$pi))
  } else if (comp$outcome == "norm") {
    comp$pi                                      <- comp$nrow_pi <- NA
    if (length(comp$sigma) == 1) {
      comp$sigma                                 <- matrix(comp$sigma, 2,
                                                           comp$Kp1)
    } else {
      comp$sigma                                 <-
        cbind(comp$sigma[1], matrix(comp$sigma[2], 2, comp$K))
    }
  } else if (comp$outcome == "pois") {
    comp$lambda                                  <-
      comp$lambda0 + cbind(0, comp$tau)
    comp$nrow_lambda                             <- 2
    comp$sigma                                   <- sqrt(comp$lambda)
  }
  comp                                         <-
    components_dtl_covariances_sqrt_Is(comp)
  Lambda_0                                     <- Lambda_1       <-
    matrix(0, comp$JK, comp$JK)
  diag_sqrt_Is_0                               <- diag_sqrt_Is_1 <- list()
  for (j1 in comp$seq_J) {
    diag_sqrt_Is_0[[j1]]                       <-
      diag(comp$sqrt_Is[[1]][, comp$seq_js[[j1]]])
    fact_0                                     <-
      diag_sqrt_Is_0[[j1]]%*%comp$Cov_taus[[1]][[j1]]
    Lambda_0[comp$seq_js[[j1]],
             comp$seq_js[[j1]]]                <-
      fact_0%*%diag_sqrt_Is_0[[j1]]
    diag_sqrt_Is_1[[j1]]                       <-
      diag(comp$sqrt_Is[[2]][, comp$seq_js[[j1]]])
    fact_1                                     <-
      diag_sqrt_Is_1[[j1]]%*%comp$Cov_taus[[2]][[j1]]
    Lambda_1[comp$seq_js[[j1]],
             comp$seq_js[[j1]]]                <-
      fact_1%*%diag_sqrt_Is_1[[j1]]
    for (j2 in seq_len(j1 - 1)) {
      Lambda_0[comp$seq_js[[j1]],
               comp$seq_js[[j2]]]              <-
        Lambda_0[comp$seq_js[[j2]],
                 comp$seq_js[[j1]]]            <-
        fact_0%*%diag_sqrt_Is_0[[j2]]
      Lambda_1[comp$seq_js[[j1]],
               comp$seq_js[[j2]]]              <-
        Lambda_1[comp$seq_js[[j2]],
                 comp$seq_js[[j1]]]            <-
        fact_1%*%diag_sqrt_Is_1[[j2]]
    }
  }
  rej                           <- matrix(0L, comp$KvJ, comp$KvJ)
  rej[lower.tri(rej, diag = TRUE)] <- 1L
  outcomes                      <- cbind(matrix(comp$seq_K, comp$KvJ, comp$K,
                                                byrow = TRUE),
                                         rej, matrix(0L, comp$KvJ, 2))
  conditions                    <- sum(comp$Kv[1:comp$Jm1]) - comp$Jm1 + comp$KvJ +
    as.numeric(comp$KvJ > 1)*(comp$KvJ - 1)
  A                             <- matrix(0L, conditions, comp$JK)
  counter                       <- 1
  for (j in 1:comp$Jm1) {
    dropped                     <- comp$Kv[j]:(comp$Kv[j + 1] + 1)
    if (length(dropped) > 1) {
      for (cond in 1:(comp$Kv[j] - comp$Kv[j + 1] - 1)) {
        A[counter,
          comp$K*(j - 1) +
            which(outcomes[1, comp$seq_K] == dropped[cond + 1])]            <- 1L
        A[counter,
          comp$K*(j - 1) + which(outcomes[1, comp$seq_K] == dropped[cond])]  <- -1L
        counter                 <- counter + 1
      }
    }
    continue                    <- comp$Kv[j + 1]:1
    for (cond in 1:comp$Kv[j + 1]) {
      A[counter,
        comp$K*(j - 1) + which(outcomes[1, comp$seq_K] == continue[cond])]   <- 1L
      A[counter,
        comp$K*(j - 1) +
          which(outcomes[1, comp$seq_K] == dropped[length(dropped)])]       <- -1L
      counter                   <- counter + 1
    }
  }
  if (comp$KvJ > 1) {
    condition                   <- matrix(0L, comp$KvJ - 1, comp$JK)
    for (cond in 1:(comp$KvJ - 1)) {
      A[counter,
        comp$K*(comp$J - 1) + which(outcomes[1, comp$seq_K] == comp$KvJ - cond)]     <- 1L
      A[counter,
        comp$K*(comp$J - 1) + which(outcomes[1, comp$seq_K] == comp$KvJ - cond + 1)] <- -1L
      counter                   <- counter + 1
    }
  }
  for (k in 1:comp$KvJ) {
    A[counter, (comp$J - 1)*comp$K + which(outcomes[1, comp$seq_K] == k)]         <- 1L
    counter                     <- counter + 1
  }
  means_HG                      <- numeric(conditions)
  means_LFC                     <-
    as.numeric(A%*%(rep(comp$tau_power, comp$J)*
                      comp$sqrt_Is[[2]][1, ]))
  Lambda_HG                     <- A%*%Lambda_0%*%t(A)
  Lambda_LFC                    <- A%*%Lambda_1%*%t(A)
  lowers_i                      <- numeric(conditions)
  uppers_i                      <- rep(Inf, conditions)
  lowers                        <- uppers <- list()
  for (i in 1:comp$KvJ) {
    lowers[[i]]                 <- lowers_i
    uppers[[i]]                 <- uppers_i
  }
  comp$Lambda_HG <- Lambda_HG
  comp$Lambda_LFC <- Lambda_LFC
  comp$means_HG <- means_HG
  comp$means_LFC <- means_LFC
  comp$nrow_Lambda_HG <- nrow(Lambda_HG)
  comp$outcomes <- outcomes
  comp$lowers <- lowers
  comp$uppers <- uppers
  comp
}

components_dtl_init                <- function(alpha, beta, delta0, delta1,
                                               integer, Kv, power, ratio,
                                               spacing, summary, type, sigma,
                                               pi0, lambda0, n_factor = 1,
                                               e = NULL) {
  J             <- length(Kv)
  K             <- Kv[1]
  seq_J         <- 1:J
  seq_js        <- list()
  for (j in seq_J) {
    seq_js[[j]] <- (1 + (j - 1)*K):(j*K)
  }
  if (power == "marginal") {
    c           <- 1L
  } else if (power == "disjunctive") {
    c           <- K
  }
  if (!missing(sigma)) {
    outcome     <- "norm"
    pi0         <- lambda0 <- NA
  } else if (!missing(pi0)) {
    outcome     <- "bern"
    sigma       <- lambda0 <- NA
  } else if (!missing(lambda0)) {
    outcome     <- "pois"
    sigma       <- pi0     <- NA
  }
  list(alpha = alpha, beta = beta, c = c, delta0 = delta0, delta1 = delta1,
       e = e, integer = integer, J = J, JK = J*K, Jm1 = J - 1, K = K,
       Kp1 = K + 1, Kv = Kv, KvJ = Kv[J], lambda0 = lambda0,
       n_factor = n_factor, outcome = outcome, pi0 = pi0, power = power,
       r = ratio, seq_J = seq_J, seq_js = seq_js, seq_K = 1:K,
       seq_KvJ = 1:Kv[J], sigma = sigma, spacing = spacing, summary = summary,
       twoKp1 = 2*K + 1, type = type)
}

components_dtl_update              <- function(comp, tau, pi, lambda) {
  if (comp$outcome == "norm") {
    comp$tau         <- tau
    comp$nrow_tau    <- nrow(comp$tau)
    if (is.matrix(comp$sigma)) {
      comp$sigma     <- matrix(comp$sigma[1, ], comp$nrow_tau, comp$Kp1,
                               byrow = TRUE)
    } else {
      if (length(comp$sigma) == 1) {
        comp$sigma   <- matrix(comp$sigma, 2, comp$Kp1)
      } else {
        comp$sigma   <- cbind(comp$sigma[1], matrix(comp$sigma[2], 2, comp$K))
      }
      comp$sigma     <- matrix(comp$sigma[1, ], comp$nrow_tau, comp$Kp1,
                               byrow = TRUE)
    }
  } else if (comp$outcome == "bern") {
    comp$pi          <- pi
    comp$tau         <- pi[, -1] - pi[, 1]
    comp$nrow_pi     <- comp$nrow_tau <- nrow(comp$pi)
    comp$sigma       <- sqrt(comp$pi*(1 - comp$pi))
  } else if (comp$outcome == "pois") {
    comp$lambda      <- lambda
    comp$tau         <- lambda[, -1] - lambda[, 1]
    comp$nrow_lambda <- comp$nrow_tau <- nrow(comp$lambda)
    comp$sigma       <- sqrt(comp$lambda)
  }
  components_dtl_all(comp)
}

components_dtl_update_bounds       <- function(e, comp) {
  for (i in comp$seq_KvJ) {
    for (k in comp$seq_KvJ) {
      if (comp$outcomes[i, comp$K + k] == 1L) {
        comp$lowers[[i]][comp$nrow_Lambda_HG - comp$KvJ + k] <- e
      } else {
        comp$lowers[[i]][comp$nrow_Lambda_HG - comp$KvJ + k] <- -Inf
        comp$uppers[[i]][comp$nrow_Lambda_HG - comp$KvJ + k] <- e
      }
    }
  }
  comp
}

integer_dtl                        <- function(comp) {
  if (comp$integer) {
    if (comp$type == "variable") {
      comp$n_factor   <- ceiling(comp$n_factor)
      while (comp$n_factor*comp$r%%1 != 0) {
        comp$n_factor <- comp$n_factor + 1
      }
    } else {
      comp$n_factor   <- ceiling(comp$n_factor)
      while (any(comp$n_factor%%(1 + comp$r*comp$seq_K) != 0)) {
        comp$n_factor <- comp$n_factor + 1
      }
    }
  }
  comp
}

calculate_mode                     <- function(x) {
  uniq_x <- unique(na.omit(x))
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

opchar_dtl_internal                <- function(comp) {
  opchar           <- matrix(0, comp$nrow_tau, 3*comp$K + 8)
  for (i in 1:comp$nrow_tau) {
    opchar[i, ]    <- opchar_dtl_internal_2(comp, i)
  }
  if (comp$type == "fixed") {
    ess            <- mess <- moss <- maxN <-
      comp$n_factor*comp$spacing[comp$J]/comp$spacing[1]
  } else {
    ess            <- comp$n_factor*(1 + comp$K*comp$r)
    for (j in comp$seq_J[-1]) {
      ess          <- ess + comp$n_factor*(1 + comp$Kv[j]*comp$r)*
        (comp$spacing[j] - comp$spacing[j - 1])/comp$spacing[1]
    }
    mess           <- moss <- maxN <- ess
  }
  opchar           <- cbind(opchar, ess, 0, mess, moss, maxN)
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
                        "FNDR", "Sens", "Spec", "ESS", "SDSS", "MeSS", "MoSS",
                        "maxN")
  comp$opchar      <- tibble::as_tibble(opchar, .name_repair = "minimal")
  comp
}

opchar_dtl_internal_2              <- function(comp, i) {
  comp                            <- components_dtl_all_update(comp, i)
  neg_mat                         <-
    matrix(comp$tau[i, ] <= 0, comp$nrow_outcomes, comp$K, byrow = TRUE)
  tau                             <- rep(comp$tau[i, ], comp$J)
  for (i in 1:comp$nrow_outcomes) {
    comp$outcomes[i, comp$twoKp1] <-
      mvtnorm::pmvnorm(comp$lowers[[i]], comp$uppers[[i]],
                       comp$means[[i]], sigma = comp$Lambdas[[i]])[1]
  }
  comp$outcomes[is.nan(comp$outcomes[, comp$twoKp1]), comp$twoKp1] <- 0
  comp$outcomes[, comp$twoKp1]    <-
    comp$outcomes[, comp$twoKp1]/sum(comp$outcomes[, comp$twoKp1])
  pos_mat                         <- !neg_mat
  false_discoveries               <-
    Rfast::rowsums(comp$outcomes[, comp$seq_K + comp$K]*neg_mat)
  false_non_discoveries           <-
    Rfast::rowsums(comp$inv_psi*pos_mat)
  pFDR                            <-
    sum(comp$outcomes[comp$disjunctive, comp$twoKp1]*
          false_discoveries[comp$disjunctive]/
          comp$discoveries[comp$disjunctive])/
    sum(comp$outcomes[comp$disjunctive, comp$twoKp1])
  if (is.nan(pFDR)) {
    pFDR                          <- 0
  }
  c(sum(comp$outcomes[comp$disjunctive, comp$twoKp1]),
    sum(comp$outcomes[comp$conjunctive, comp$twoKp1]),
    sapply(comp$seq_K,
           function(k) { sum(comp$outcomes[comp$outcomes[, comp$K + k] == 1,
                                           comp$twoKp1]) }),
    sapply(comp$seq_K,
           function(k) { sum(comp$outcomes[false_discoveries >= k,
                                           comp$twoKp1]) }),
    sapply(comp$seq_K,
           function(k) { sum(comp$outcomes[false_non_discoveries >= k,
                                           comp$twoKp1]) }),
    sum(comp$outcomes[, comp$twoKp1]*false_discoveries)/comp$K,
    sum(comp$outcomes[, comp$twoKp1]*false_discoveries/comp$mod_discoveries),
    pFDR,
    sum(comp$outcomes[, comp$twoKp1]*
          false_non_discoveries/comp$mod_non_discoveries),
    sum(comp$outcomes[, comp$twoKp1]*
          Rfast::rowsums(comp$outcomes[, comp$K + comp$seq_K]*pos_mat))/
      max(sum(tau[comp$seq_K] > 0), 1),
    sum(comp$outcomes[, comp$twoKp1]*Rfast::rowsums(comp$inv_psi*neg_mat))/
      max(sum(tau[comp$seq_K] <= 0), 1))
}

root_bounds_dtl                    <- function(e, comp) {
  comp <- components_dtl_update_bounds(e, comp)
  for (i in comp$seq_KvJ) {
    comp$outcomes[i, comp$K + comp$KvJ + 1L] <-
      mvtnorm::pmvnorm(comp$lowers[[i]], comp$uppers[[i]], comp$means_HG,
                       sigma = comp$Lambda_HG)[1]
  }
  factorial(comp$K)*sum(comp$outcomes[, comp$K + comp$KvJ + 1L]) - comp$alpha
}

root_ss_dtl                        <- function(ss, comp) {
  for (i in 1:comp$KvJ) {
    comp$outcomes[i, comp$K + comp$KvJ + 2L] <-
      mvtnorm::pmvnorm(comp$lowers[[i]], comp$uppers[[i]], sqrt(ss)*comp$means_LFC,
                       sigma = comp$Lambda_LFC)[1]
  }
  if (comp$power == "marginal") {
    factorial(comp$K - 1)*sum(comp$outcomes[, comp$K + comp$KvJ + 2L]) -
      (1 - comp$beta)
  } else {
    factorial(comp$K)*sum(comp$outcomes[, comp$K + comp$KvJ + 2L]) -
      (1 - comp$beta)
  }
}

sim_dtl_bern_internal              <- function(pi, completed_replicates, n, e,
                                               Kv, ratio, type, spacing,
                                               replicates, summary,
                                               total_replicates) {
  summary_i                   <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  J                           <- length(Kv)
  seq_J                       <- 1:J
  K                           <- Kv[1]
  seq_K                       <- 1:K
  Kp1                         <- K + 1L
  if (type == "variable") {
    N                         <- matrix(c(n, rep(n*ratio, K)), replicates, Kp1)
    nj_base                   <- c(n, rep(n*ratio, K))
    maxN                      <- n*(1 + ratio*K)
    for (j in seq_J[-1]) {
      maxN                    <- maxN + n*(1 + ratio*Kv[j])*
        (spacing[j] - spacing[j - 1])/spacing[1]
    }
  } else {
    N                         <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                        replicates, K + 1L)
    maxN                      <- n/spacing[1]
  }
  rej_mat                     <- matrix(0L, replicates, K)
  numeric_Kp1                 <- numeric(Kp1)
  for (i in 1:replicates) {
    present                   <- rep(TRUE, Kp1)
    pi_hat                    <- numeric_Kp1
    nj                        <- N[i, ]
    for (j in seq_J) {
      pi_hat_iterate          <- numeric_Kp1
      pi_hat_iterate[present] <- stats::rbinom(sum(present), nj[present],
                                               pi[present])
      pi_hat                  <-
        ((N[i, ] - nj)*pi_hat + pi_hat_iterate)/N[i, ]
      sigma2                  <- pi_hat*(1 - pi_hat)
      Z_j                     <-
        (pi_hat[-1] - pi_hat[1])/sqrt(sigma2[1]/N[i, 1] + sigma2[-1]/N[i, -1])
      if (j < J) {
        order_Z_j             <- order(Z_j, decreasing = TRUE)
        found                 <- 0L
        present_new           <- c(TRUE, rep(FALSE, K))
        counter               <- 1L
        while (found < Kv[j + 1]) {
          if (present[order_Z_j[counter] + 1]) {
            present_new[order_Z_j[counter] + 1] <- TRUE
            found             <- found + 1L
          }
          counter             <- counter + 1L
        }
        present               <- present_new
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        nj                    <- nj*(spacing[j + 1] - spacing[j])/spacing[1]
        N[i, ]                <- N[i, ] + nj
      } else {
        for (k in which(present[-1])) {
          if (Z_j[k] > e) {
            rej_mat[i, k]       <- 1L
          }
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
  neg_mat                               <- matrix(pi[-1] <= pi[1], replicates,
                                                  K, byrow = TRUE)
  pos_mat                               <- !neg_mat
  false_discoveries                     <- Rfast::rowsums(rej_mat*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums((rej_mat == 0)*pos_mat)
  which_discoveries                     <- which(discoveries > 0)
  discoveries[-which_discoveries]       <- 1
  non_discoveries[non_discoveries == 0] <- 1
  rowsums_N                             <- Rfast::rowsums(N)
  c(pi, length(which_discoveries)/replicates,
    sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sapply(1:K, function(k) { sum(false_discoveries >= k) })/replicates,
    sapply(1:K, function(k) { sum(false_non_discoveries >= k) })/replicates,
    sum(false_discoveries)/(K*replicates),
    sum(false_discoveries/discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat[which_discoveries, ]*
                         matrix(pi[-1] <= pi[1], length(which_discoveries), K,
                                byrow = TRUE))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/
      (max(sum(pi[-1] > pi[1]), 1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(pi[-1] <= pi[1]), 1)*replicates),
    sum(rowsums_N)/replicates, stats::sd(rowsums_N),
    stats::quantile(rowsums_N, 0.5), calculate_mode(rowsums_N), maxN)
}

sim_dtl_norm_internal              <- function(tau, completed_replicates, n, e,
                                               Kv, sigma, ratio, type, spacing,
                                               replicates, summary,
                                               total_replicates) {
  summary_i                   <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  tau                         <- c(0, tau)
  J                           <- length(Kv)
  seq_J                       <- 1:J
  K                           <- Kv[1]
  seq_K                       <- 1:K
  Kp1                         <- K + 1L
  if (type == "variable") {
    N                         <- matrix(c(n, rep(n*ratio, K)), replicates, Kp1)
    nj_base                   <- c(n, rep(n*ratio, K))
    maxN                      <- n*(1 + ratio*K)
    for (j in seq_J[-1]) {
      maxN                    <- maxN + n*(1 + ratio*Kv[j])*
        (spacing[j] - spacing[j - 1])/spacing[1]
    }
  } else {
    N                         <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                        replicates, K + 1L)
    maxN                      <- n/spacing[1]
  }
  rej_mat                     <- matrix(0L, replicates, K)
  numeric_Kp1                 <- numeric(Kp1)
  for (i in 1:replicates) {
    present                   <- rep(TRUE, Kp1)
    mu_hat                    <- numeric_Kp1
    nj                        <- N[i, ]
    for (j in seq_J) {
      mu_hat_iterate          <- numeric_Kp1
      mu_hat_iterate[present] <- stats::rnorm(sum(present), tau[present],
                                              sigma[present]/sqrt(nj[present]))
      mu_hat                  <-
        ((N[i, ] - nj)*mu_hat + nj*mu_hat_iterate)/N[i, ]
      Z_j                     <-
        (mu_hat[-1] - mu_hat[1])/sqrt(sigma[1]^2/N[i, 1] + sigma[-1]^2/N[i, -1])
      if (j < J) {
        order_Z_j             <- order(Z_j, decreasing = TRUE)
        found                 <- 0L
        present_new           <- c(TRUE, rep(FALSE, K))
        counter               <- 1L
        while (found < Kv[j + 1]) {
          if (present[order_Z_j[counter] + 1]) {
            present_new[order_Z_j[counter] + 1] <- TRUE
            found             <- found + 1L
          }
          counter             <- counter + 1L
        }
        present               <- present_new
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        nj                    <- nj*(spacing[j + 1] - spacing[j])/spacing[1]
        N[i, ]                <- N[i, ] + nj
      } else {
        for (k in which(present[-1])) {
          if (Z_j[k] > e) {
            rej_mat[i, k]       <- 1L
          }
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
  neg_mat                               <- matrix(tau[-1] <= 0, replicates, K,
                                                  byrow = TRUE)
  pos_mat                               <- !neg_mat
  false_discoveries                     <- Rfast::rowsums(rej_mat*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums((rej_mat == 0)*pos_mat)
  which_discoveries                     <- which(discoveries > 0)
  discoveries[-which_discoveries]       <- 1
  non_discoveries[non_discoveries == 0] <- 1
  rowsums_N                             <- Rfast::rowsums(N)
  c(tau[-1], length(which_discoveries)/replicates,
    sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sapply(1:K, function(k) { sum(false_discoveries >= k) })/replicates,
    sapply(1:K, function(k) { sum(false_non_discoveries >= k) })/replicates,
    sum(false_discoveries)/(K*replicates),
    sum(false_discoveries/discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat[which_discoveries, ]*
                         matrix(tau[-1] <= 0, length(which_discoveries), K,
                                byrow = TRUE))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(tau[-1] > 0), 1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(tau[-1] <= 0), 1)*replicates),
    sum(rowsums_N)/replicates, stats::sd(rowsums_N),
    stats::quantile(rowsums_N, 0.5), calculate_mode(rowsums_N), maxN)
}

sim_dtl_pois_internal              <- function(lambda, completed_replicates, n,
                                               e, Kv, ratio, type, spacing,
                                               replicates, summary,
                                               total_replicates) {
  summary_i                       <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  J                               <- length(Kv)
  seq_J                           <- 1:J
  K                               <- Kv[1]
  seq_K                           <- 1:K
  Kp1                             <- K + 1L
  if (type == "variable") {
    N                         <- matrix(c(n, rep(n*ratio, K)), replicates, Kp1)
    nj_base                   <- c(n, rep(n*ratio, K))
    maxN                      <- n*(1 + ratio*K)
    for (j in seq_J[-1]) {
      maxN                    <- maxN + n*(1 + ratio*Kv[j])*
        (spacing[j] - spacing[j - 1])/spacing[1]
    }
  } else {
    N                         <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                        replicates, K + 1L)
    maxN                      <- n/spacing[1]
  }
  rej_mat                         <- matrix(0L, replicates, K)
  numeric_Kp1                     <- numeric(Kp1)
  for (i in 1:replicates) {
    present                       <- rep(TRUE, Kp1)
    lambda_hat                    <- numeric_Kp1
    nj                            <- N[i, ]
    for (j in seq_J) {
      lambda_hat_iterate          <- numeric_Kp1
      lambda_hat_iterate[present] <- stats::rpois(sum(present),
                                                  nj[present]*lambda[present])
      lambda_hat                  <-
        ((N[i, ] - nj)*lambda_hat + lambda_hat_iterate)/N[i, ]
      sigma2                      <- lambda_hat
      Z_j                         <-
        (lambda_hat[-1] - lambda_hat[1])/
        sqrt(sigma2[1]/N[i, 1] + sigma2[-1]/N[i, -1])
      if (j < J) {
        order_Z_j             <- order(Z_j, decreasing = TRUE)
        found                 <- 0L
        present_new           <- c(TRUE, rep(FALSE, K))
        counter               <- 1L
        while (found < Kv[j + 1]) {
          if (present[order_Z_j[counter] + 1]) {
            present_new[order_Z_j[counter] + 1] <- TRUE
            found             <- found + 1L
          }
          counter             <- counter + 1L
        }
        present               <- present_new
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        nj                    <- nj*(spacing[j + 1] - spacing[j])/spacing[1]
        N[i, ]                <- N[i, ] + nj
      } else {
        for (k in which(present[-1])) {
          if (Z_j[k] > e) {
            rej_mat[i, k]       <- 1L
          }
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
  neg_mat                               <- matrix(lambda[-1] <= lambda[1], replicates,
                                                  K, byrow = TRUE)
  pos_mat                               <- !neg_mat
  false_discoveries                     <- Rfast::rowsums(rej_mat*neg_mat)
  false_non_discoveries                 <-
    Rfast::rowsums((rej_mat == 0)*pos_mat)
  which_discoveries                     <- which(discoveries > 0)
  discoveries[-which_discoveries]       <- 1
  non_discoveries[non_discoveries == 0] <- 1
  rowsums_N                             <- Rfast::rowsums(N)
  c(lambda, length(which_discoveries)/replicates,
    sum(discoveries == K)/replicates,
    Rfast::colsums(rej_mat)/replicates,
    sapply(1:K, function(k) { sum(false_discoveries >= k) })/replicates,
    sapply(1:K, function(k) { sum(false_non_discoveries >= k) })/replicates,
    sum(false_discoveries)/(K*replicates),
    sum(false_discoveries/discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat[which_discoveries, ]*
                         matrix(lambda[-1] <= lambda[1], length(which_discoveries), K,
                                byrow = TRUE))/discoveries[which_discoveries])/
      length(which_discoveries),
    sum(false_non_discoveries/non_discoveries)/replicates,
    sum(Rfast::rowsums(rej_mat*pos_mat))/
      (max(sum(lambda[-1] > lambda[1]), 1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(lambda[-1] <= lambda[1]), 1)*replicates),
    sum(rowsums_N)/replicates, stats::sd(rowsums_N),
    stats::quantile(rowsums_N, 0.5), calculate_mode(rowsums_N), maxN)
}
