fdr_ma_single  <- function(tau, K, u, n, sigma, st_Lambda) {
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

fdr_ma_step    <- function(tau, K, correction, uO, n, sigma, st_Lambda) {
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

fwer_ma_single <- function(tau, K, u, n, sigma, st_Lambda) {
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

fwer_ma_step   <- function(tau, K, correction, uO, n, sigma, st_Lambda) {
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
