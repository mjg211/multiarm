bounds_mams            <- function(C, J, fshape, eshape, ffix, efix) {
  if (eshape == "obf") {
    e    <- C*sqrt(J/(1:J))
  } else if (eshape == "pocock") {
    e    <- rep(C, J)
  } else if (eshape == "fixed") {
    e    <- c(rep(efix, J - 1), C)
  } else if (eshape=="triangular") {
    e    <- C*(1 + (1:J)/J)/sqrt(1:J)
  }
  if (fshape == "obf") {
    f    <- c(-C*sqrt(J/(1:(J - 1))), e[J])
  } else if (fshape == "pocock") {
    f    <- c(rep(-C, J - 1), e[J])
  } else if (fshape == "fixed") {
    f    <- c(rep(ffix, J - 1), e[J])
  } else if (fshape == "triangular") {
    if (eshape == "triangular") {
      f  <- -C*(1 - 3*(1:J)/J)/sqrt(1:J)
    } else {
      f  <- -C*(1 - 3*(1:J)/J)/sqrt(1:J)/(-1*(1 - 3)/sqrt(J))
    }
  }
  bounds <- c(f, e, -Inf, Inf)
  list(bounds = bounds, e = e, f = f)
}

components_all_dtl     <- function(Kv, r, sigma, e) {
  J                              <- length(Kv)
  seq_J                          <- 1:J
  K                              <- Kv[1]
  seq_K                          <- 1:K
  Lambda                         <- matrix(0, Kv[1]*J, Kv[1]*J)
  r_factor                       <- r/(r + 1)
  diag_block                     <-
    matrix(r_factor, Kv[1], Kv[1]) + diag(1 - r_factor, Kv[1], Kv[1])
  for (j in seq_J) {
    row_col                      <- (1 + (j - 1)*Kv[1]):(j*Kv[1])
    Lambda[row_col, row_col]     <- diag_block
  }
  if (J > 1) {
    for (j1 in seq_J[-1]) {
      for (j2 in 1:(j1 - 1)) {
        Cov_Zj1Zj2               <-
          matrix(r_factor*sqrt(j2/j1), Kv[1], Kv[1]) +
          diag(sqrt(j2/j1) - r_factor*sqrt(j2/j1), Kv[1], Kv[1])
        j1_seq                   <- (1 + (j1 - 1)*Kv[1]):(j1*Kv[1])
        j2_seq                   <- (1 + (j2 - 1)*Kv[1]):(j2*Kv[1])
        Lambda[j2_seq, j1_seq]   <- Cov_Zj1Zj2
        Lambda[j1_seq, j2_seq]   <- Cov_Zj1Zj2
      }
    }
  }
  ordering                      <-
    iterpc::getall(iterpc::iterpc(n       = Kv[1],
                                  r       = Kv[1],
                                  labels  = 1:Kv[1],
                                  ordered = T))
  outcomes                      <- matrix(0L, nrow(ordering)*(Kv[J] + 1), 2*Kv[1])
  for (i in 1:(Kv[J] + 1)) {
    outcomes[(1 + (i - 1)*nrow(ordering)):(i*nrow(ordering)), 1:Kv[1]] <-
      ordering
    for (j in 1:nrow(ordering)) {
      outcomes[(i - 1)*nrow(ordering) + j, Kv[1] + order(ordering[j, ],
                                                         decreasing = T)] <-
        c(rep(0, Kv[1] - i + 1), rep(1, i - 1))
    }
  }
  conditions                    <- sum(Kv[1:(J - 1)]) - (J - 1) + Kv[J] +
    as.numeric(Kv[J] > 1)*(Kv[J] - 1)
  As                            <- Lambdas <- lowers <- uppers <- list()
  lowers_i                      <- numeric(conditions)
  uppers_i                      <- rep(Inf, conditions)
  for (i in 1:nrow(outcomes)) {
    A                             <- matrix(0L, conditions, J*Kv[1])
    counter                       <- 1
    for (j in 1:(J - 1)) {
      dropped                     <- Kv[j]:(Kv[j + 1] + 1)
      if (length(dropped) > 1) {
        for (cond in 1:(Kv[j] - Kv[j + 1] - 1)) {
          A[counter,
            Kv[1]*(j - 1) +
              which(outcomes[i, 1:Kv[1]] == dropped[cond + 1])]            <- 1L
          A[counter,
            Kv[1]*(j - 1) + which(outcomes[i, 1:Kv[1]] == dropped[cond])]  <- -1L
          counter                 <- counter + 1
        }
      }
      continue                    <- Kv[j + 1]:1
      for (cond in 1:Kv[j + 1]) {
        A[counter,
          Kv[1]*(j - 1) + which(outcomes[i, 1:Kv[1]] == continue[cond])]   <- 1L
        A[counter,
          Kv[1]*(j - 1) +
            which(outcomes[i, 1:Kv[1]] == dropped[length(dropped)])]       <- -1L
        counter                   <- counter + 1
      }
    }
    if (Kv[J] > 1) {
      condition                   <- matrix(0L, Kv[J] - 1, J*Kv[1])
      for (cond in 1:(Kv[J] - 1)) {
        A[counter,
          Kv[1]*(J - 1) + which(outcomes[i, 1:Kv[1]] == Kv[J] - cond)]     <- 1L
        A[counter,
          Kv[1]*(J - 1) + which(outcomes[i, 1:Kv[1]] == Kv[J] - cond + 1)] <- -1L
        counter                   <- counter + 1
      }
    }
    for (k in 1:Kv[J]) {
      A[counter, (J - 1)*Kv[1] + which(outcomes[i, 1:Kv[1]] == k)]         <- 1L
      counter                     <- counter + 1
    }
    As[[i]]                       <- A
    Lambdas[[i]]                  <- A%*%Lambda%*%t(A)
    lowers[[i]]                   <- lowers_i
    uppers[[i]]                   <- uppers_i
    for (i in 1:Kv[J]) {
      for (k in 1:Kv[J]) {
        if (outcomes[i, Kv[1] + k] == 1L) {
          lowers[[i]][nrow(As[[i]]) - Kv[J] + k] <- e
        } else {
          lowers[[i]][nrow(As[[i]]) - Kv[J] + k] <- -Inf
          uppers[[i]][nrow(As[[i]]) - Kv[J] + k] <- e
        }
      }
    }
  }
  mod_discoveries                            <- discoveries <-
    Rfast::rowsums(outcomes[, seq_K + K])
  mod_non_discoveries                        <- non_discoveries <-
    K - discoveries
  disjunctive                                <- which(discoveries > 0)
  mod_discoveries[-disjunctive]                 <- 1
  mod_non_discoveries[mod_non_discoveries == 0] <- 1
  list(conjunctive         = which(discoveries == K),
       discoveries         = discoveries,
       disjunctive         = disjunctive,
       inv_psi             = (outcomes[, K + seq_K] == 0),
       J                   = J,
       K                   = K,
       Kv = Kv,
       As                  = As,
       Lambdas             = Lambdas,
       lowers              = lowers,
       mod_discoveries     = mod_discoveries,
       mod_non_discoveries = mod_non_discoveries,
       non_discoveries     = non_discoveries,
       nrow_outcomes      = nrow(outcomes),
       outcomes           = cbind(outcomes, numeric(nrow(outcomes))),
       seq_K              = seq_K,
       twoKp1 = 2*K + 1,
       uppers             = uppers)
}

components_all_mams    <- function(K, J, a, b, c, d, r, sigma, type, n_factor) {
  seq_K                                      <- 1:K
  seq_J                                      <- 1:J
  perms_d                                    <-
    iterpc::getall(iterpc::iterpc(2*J, K, ordered = T, replace = T))
  num_perms_d                                <- nrow(perms_d)
  ceil_perms_obj                             <- ceiling(perms_d/2)
  outcomes                                   <-
    cbind(1*(perms_d%%2 == 0),
          ceil_perms_obj,
          apply(ceil_perms_obj, 1, max) +
            (type == "variable")*Rfast::rowsums(ceil_perms_obj),
          numeric(num_perms_d))
  colnames(outcomes)                         <-
    c(paste0("psi_", seq_K),
      paste0("omega_", seq_K),
      ifelse(type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|tau)")
  retain                                     <- !logical(num_perms_d)
  seq_KpK                                    <- seq_K + K
  for (i in 1:num_perms_d) {
    for (j in seq_J) {
      le                                     <-
        which(outcomes[i, seq_KpK] <= j)
      g                                      <-
        which(outcomes[i, seq_KpK] > j)
      if (all(c(length(le), length(g)) > 0)) {
        if (sum(outcomes[i, le]) >= d) {
          retain[i]                          <- F
        }
      }
    }
  }
  outcomes                                   <- outcomes[retain, ]
  if (type == "variable") {
    Lambda                                   <- matrix(0, K*J, K*J)
    r_factor                                 <- r/(r + 1)
    diag_block                               <-
      matrix(r_factor, K, K) + diag(1 - r_factor, K, K)
    for (j in seq_J) {
      row_col                                <- (1 + (j - 1)*K):(j*K)
      Lambda[row_col, row_col]               <- diag_block
    }
    if (J > 1) {
      for (j1 in seq_J[-1]) {
        for (j2 in 1:(j1 - 1)) {
          Cov_Zj1Zj2                         <-
            matrix(r_factor*sqrt(j2/j1), K, K) +
            diag(sqrt(j2/j1) - r_factor*sqrt(j2/j1), K, K)
          j1_seq                             <- (1 + (j1 - 1)*K):(j1*K)
          j2_seq                             <- (1 + (j2 - 1)*K):(j2*K)
          Lambda[j2_seq, j1_seq]             <- Cov_Zj1Zj2
          Lambda[j1_seq, j2_seq]             <- Cov_Zj1Zj2
        }
      }
    }
    sqrt_I                                   <-
      sqrt(n_factor*rep((J/(sigma[1]^2 + sigma[2]^2/r))*seq_J/J, each = K))
  } else {
    if (J == 2) {
      stage_K                                <- cbind(rep(K, K), 1:K)
    } else {
      stage_K                                <-
        iterpc::getall(iterpc::iterpc(K, J - 1, 1:K, F, T))
      stage_K                                <-
        cbind(K, t(apply(stage_K, 1, rev)))
    }
    Lambda_null                              <- matrix(0, K*J, K*J)
    Lambda_ks                                <- diag_sqrt_Is    <- Cov_taus <-
      sqrt_I_ks       <- list()
    for (k in 1:nrow(stage_K)) {
      Lambda                                 <- Lambda_null
      NC                                     <-
        n_factor*cumsum(1/(r*stage_K[k, ] + 1))
      NE                                     <-
        n_factor*cumsum(r/(r*stage_K[k, ] + 1))
      for (j1 in seq_J) {
        j1_seq                               <- (1 + (j1 - 1)*K):(j1*K)
        diag_sqrt_Is[[j1]]                   <-
          diag(rep(sqrt(1/(sigma[1]^2/NC[j1] + sigma[2]^2/NE[j1])), K))
        Cov_taus[[j1]]                       <-
          diag(rep(sigma[2]^2/NE[j1], K)) + matrix(sigma[1]^2/NC[j1], K, K)
        Lambda[j1_seq, j1_seq]               <-
          diag_sqrt_Is[[j1]]%*%Cov_taus[[j1]]%*%diag_sqrt_Is[[j1]]
        for (j2 in seq_len(j1 - 1)) {
          j2_seq                             <- (1 + (j2 - 1)*K):(j2*K)
          Lambda[j1_seq, j2_seq]             <-
            diag_sqrt_Is[[j1]]%*%Cov_taus[[j1]]%*%diag_sqrt_Is[[j2]]
          Lambda[j2_seq, j1_seq]             <- t(Lambda[j1_seq, j2_seq])
        }
      }
      sqrt_I_ks[[k]]                         <-
        rep(sqrt(1/(sigma[1]^2/NC + sigma[2]^2/NE)), each = K)
      Lambda_ks[[k]]                         <- Lambda
    }
  }
  tau_indices                                <- sqrt_Is   <- l_indices <-
    u_indices <- Lambdas   <- list()
  for (i in 1:nrow(outcomes)) {
    relevant_indices                         <- NULL
    psi                                      <- outcomes[i, seq_K]
    omega                                    <- outcomes[i, seq_KpK]
    l_i                                      <- u_i <- numeric(sum(omega))
    for (k in seq_K) {
      seq_k                                  <- seq(k, by = K,
                                                    length.out = omega[k])
      relevant_indices                       <- c(relevant_indices, seq_k)
      l_i_k                                  <- u_i_k <- numeric(omega[k])
      for (j in 1:omega[k]) {
        if (omega[k] > j) {
          l_i_k[j]                           <- j
        } else if (all(psi[k] == 0, omega[k] == j)) {
          l_i_k[j]                           <- 2*J + 1
        } else if (all(psi[k] == 1, omega[k] == j)) {
          l_i_k[j]                           <- J + j
        }
        if (any(all(psi[k] == 0, omega[k] == j, max(omega) > j),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    psi[omega == j] == 0),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    sum(psi == 1) < d))) {
          u_i_k[j]                           <- j
        } else if (all(psi[k] == 1, omega[k] == j)) {
          u_i_k[j]                           <- 2*J + 2
        } else if (any(omega[k] > j,
                       all(psi[k] == 0, omega[k] == j, max(omega) == j,
                           sum(psi == 1) >= d))) {
          u_i_k[j]                           <- J + j
        }
      }
      l_i[seq_k]                             <- l_i_k
      u_i[seq_k]                             <- u_i_k
    }
    sort_relevant_indices                    <- sort(relevant_indices)
    tau_indices[[i]]                         <- sort_relevant_indices
    if (type == "variable") {
      sqrt_Is[[i]]                           <- sqrt_I[sort_relevant_indices]
      Lambdas[[i]]                           <- Lambda[sort_relevant_indices,
                                                       sort_relevant_indices]
    } else {
      stage_k                                <-
        sapply(seq_J, function(j) {max(1, sum(omega >= j))})
      which_k                                <-
        which(sapply(1:nrow(stage_K),
                     function(k) {sum(stage_K[k, ] == stage_k)}) == J)
      sqrt_Is[[i]]                           <-
        sqrt_I_ks[[which_k]][sort_relevant_indices]
      Lambdas[[i]]                           <-
        Lambda_ks[[which_k]][sort_relevant_indices, sort_relevant_indices]
    }
    l_indices[[i]]                           <- l_i[sort_relevant_indices]
    u_indices[[i]]                           <- u_i[sort_relevant_indices]
  }
  mod_discoveries                            <- discoveries <-
    Rfast::rowsums(outcomes[, seq_K])
  mod_non_discoveries                        <- non_discoveries <-
    K - discoveries
  disjunctive                                <- which(discoveries > 0)
  mod_discoveries[-disjunctive]                 <- 1
  mod_non_discoveries[mod_non_discoveries == 0] <- 1
  list(conjunctive         = which(discoveries == K),
       discoveries         = discoveries,
       disjunctive         = disjunctive,
       inv_psi             = (outcomes[, seq_K] == 0),
       J                   = J,
       K                   = K,
       Lambdas             = Lambdas,
       l_indices           = l_indices,
       max_N_factor        = ifelse(type == "variable", J*(1 + r*K), J),
       mod_discoveries     = mod_discoveries,
       mod_non_discoveries = mod_non_discoveries,
       n_factor            = n_factor,
       non_discoveries     = non_discoveries,
       nrow_outcomes      = nrow(outcomes),
       outcomes           = outcomes,
       seq_K              = seq_K,
       sqrt_Is            = sqrt_Is,
       tau_indices        = tau_indices,
       twoKp2             = 2*K + 2,
       u_indices          = u_indices)
}

components_HG_LFC_mams <- function(K, J, a, b, c, d, r, sigma, delta1, delta0,
                                   type, w) {
  seq_K                                      <- 1:K
  seq_J                                      <- 1:J
  perms_K_HG                                 <-
    iterpc::getall(iterpc::iterpc(2*J, K, replace = T))
  perms_d_HG                                 <- perms_K_HG
  num_perms_d_HG                             <- nrow(perms_d_HG)
  deg_d_HG_a_fwer                            <- deg_d_HG <- prob_col <-
    numeric(num_perms_d_HG)
  for (i in 1:num_perms_d_HG) {
    deg_d_HG[i]                              <-
      nrow(iterpc::getall(iterpc::iterpc(table(perms_d_HG[i, ]),
                                         ordered = T)))
  }
  perms_d_HG_a_fwer_indices                  <-
    (Rfast::rowsums(perms_d_HG%%2 == 0) >= a)
  deg_d_HG_a_fwer[perms_d_HG_a_fwer_indices] <-
    deg_d_HG[perms_d_HG_a_fwer_indices]
  ceil_perms_obj                             <- ceiling(perms_d_HG/2)
  thetas_d_HG_a_fwer                         <-
    cbind(1*(perms_d_HG%%2 == 0),
          ceil_perms_obj,
          deg_d_HG,
          deg_d_HG_a_fwer,
          apply(ceil_perms_obj, 1, max) +
            (type == "variable")*Rfast::rowsums(ceil_perms_obj),
          prob_col)
  colnames(thetas_d_HG_a_fwer)               <-
    c(paste0("psi_", seq_K),
      paste0("omega_", seq_K),
      "deg_{HG}(psi,omega)",
      "deg_{HG,a-FWER}(psi,omega)",
      ifelse(type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|HG)")
  retain                                     <- !logical(num_perms_d_HG)
  seq_KpK                                    <- seq_K + K
  for (i in 1:num_perms_d_HG) {
    for (j in seq_J) {
      le                                     <-
        which(thetas_d_HG_a_fwer[i, seq_KpK] <= j)
      g                                      <-
        which(thetas_d_HG_a_fwer[i, seq_KpK] > j)
      if (all(c(length(le), length(g)) > 0)) {
        if (sum(thetas_d_HG_a_fwer[i, le]) >= d) {
          retain[i]                          <- F
        }
      }
    }
  }
  thetas_d_HG_a_fwer                        <- thetas_d_HG_a_fwer[retain, ]
  if (!missing(w)) {
    if (w[1] == 0) {
      thetas_d_HG_a_fwer                    <-
        thetas_d_HG_a_fwer[(thetas_d_HG_a_fwer[, 2*K + 2] > 0), ]
    }
  }
  perms_c_delta1                            <-
    iterpc::getall(iterpc::iterpc(2*J, c, replace = T))
  num_perms_c_delta1                        <- nrow(perms_c_delta1)
  deg_c_delta1                              <- numeric(num_perms_c_delta1)
  for (i in 1:num_perms_c_delta1) {
    deg_c_delta1[i]                         <-
      nrow(iterpc::getall(iterpc::iterpc(table(perms_c_delta1[i, ]),
                                         ordered = T)))
  }
  perms_c_delta1_b_power_indices            <-
    (Rfast::rowsums(perms_c_delta1%%2 == 0) >= b)
  deg_c_delta1_b_power                      <- numeric(num_perms_c_delta1)
  deg_c_delta1_b_power[perms_c_delta1_b_power_indices] <-
    deg_c_delta1[perms_c_delta1_b_power_indices]
  if (c < K) {
    perms_Kmin_c_delta0                     <-
      iterpc::getall(iterpc::iterpc(2*J, K - c, replace = T))
    num_perms_Kmin_c_delta0                 <- nrow(perms_Kmin_c_delta0)
    deg_Kmin_c_delta0                       <-
      numeric(num_perms_Kmin_c_delta0)
    for (i in 1:num_perms_Kmin_c_delta0) {
      deg_Kmin_c_delta0[i]                  <-
        nrow(iterpc::getall(iterpc::iterpc(table(perms_Kmin_c_delta0[i, ]),
                                           ordered = T)))
    }
    perms_K_d_bc_LFC                         <-
      matrix(0L, num_perms_c_delta1*num_perms_Kmin_c_delta0, K)
    for (i in 1:num_perms_c_delta1) {
      range                                  <-
        (1 + (i - 1)*num_perms_Kmin_c_delta0):(i*num_perms_Kmin_c_delta0)
      i_matrix                               <-
        matrix(perms_c_delta1[i, ], num_perms_Kmin_c_delta0, c, byrow = T)
      perms_K_d_bc_LFC[range, ]              <- cbind(i_matrix,
                                                      perms_Kmin_c_delta0)
    }
    deg_K_d_bc_LFC                           <-
      rep(deg_c_delta1, each = num_perms_Kmin_c_delta0)*
      rep(deg_Kmin_c_delta0, num_perms_c_delta1)
    deg_K_d_bc_LFC_power                     <-
      rep(deg_c_delta1_b_power, each = num_perms_Kmin_c_delta0)*
      rep(deg_Kmin_c_delta0, num_perms_c_delta1)
    ceil_perms_obj                           <- ceiling(perms_K_d_bc_LFC/2)
    thetas_bc_LFC_power                      <-
      cbind(1*(perms_K_d_bc_LFC%%2 == 0),
            ceil_perms_obj,
            deg_K_d_bc_LFC,
            deg_K_d_bc_LFC_power,
            apply(ceil_perms_obj, 1, max) +
              (type == "variable")*Rfast::rowsums(ceil_perms_obj),
            numeric(num_perms_c_delta1*num_perms_Kmin_c_delta0))
  } else {
    ceil_perms_obj                           <- ceiling(perms_c_delta1/2)
    thetas_bc_LFC_power                      <-
      cbind(1*(perms_c_delta1%%2 == 0),
            ceil_perms_obj,
            deg_c_delta1,
            deg_c_delta1_b_power,
            apply(ceil_perms_obj, 1, max) +
              (type == "variable")*Rfast::rowsums(ceil_perms_obj),
            numeric(num_perms_c_delta1))
  }
  colnames(thetas_bc_LFC_power)              <-
    c(paste0("psi_", seq_K),
      paste0("omega_", seq_K),
      "deg_{LFC}(psi,omega)",
      "deg_{LFC,bc-power}(psi,omega)",
      ifelse(type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|LFC)")
  retain                                     <-
    !logical(nrow(thetas_bc_LFC_power))
  for (i in 1:nrow(thetas_bc_LFC_power)) {
    for (j in seq_J) {
      le                                     <-
        which(thetas_bc_LFC_power[i, seq_KpK] <= j)
      g                                      <-
        which(thetas_bc_LFC_power[i, seq_KpK] > j)
      if (all(c(length(le), length(g)) > 0)) {
        if (sum(thetas_bc_LFC_power[i, le]) >= d) {
          retain[i]                          <- F
        }
      }
    }
  }
  thetas_bc_LFC_power                        <- thetas_bc_LFC_power[retain, ]
  if (!missing(w)) {
    if (w[2] == 0) {
      thetas_bc_LFC_power                    <-
        thetas_bc_LFC_power[(thetas_bc_LFC_power[, 2*K + 2] > 0), ]
    }
  }
  if (type == "variable") {
    Lambda                                   <- matrix(0, K*J, K*J)
    r_factor                                 <- r/(r + 1)
    diag_block                               <-
      matrix(r_factor, K, K) + diag(1 - r_factor, K, K)
    for (j in seq_J) {
      row_col                                <- (1 + (j - 1)*K):(j*K)
      Lambda[row_col, row_col]               <- diag_block
    }
    if (J > 1) {
      for (j1 in seq_J[-1]) {
        for (j2 in 1:(j1 - 1)) {
          Cov_Zj1Zj2                         <-
            matrix(r_factor*sqrt(j2/j1), K, K) +
            diag(sqrt(j2/j1) - r_factor*sqrt(j2/j1), K, K)
          j1_seq                             <- (1 + (j1 - 1)*K):(j1*K)
          j2_seq                             <- (1 + (j2 - 1)*K):(j2*K)
          Lambda[j2_seq, j1_seq]             <- Cov_Zj1Zj2
          Lambda[j1_seq, j2_seq]             <- Cov_Zj1Zj2
        }
      }
    }
    sqrt_I_div_par                           <-
      sqrt(rep((J/(sigma[1]^2 + sigma[2]^2/r))*seq_J/J, each = K))
  } else {
    if (J == 2) {
      stage_K                                <- cbind(rep(K, K), 1:K)
    } else {
      stage_K                                <-
        iterpc::getall(iterpc::iterpc(K, J - 1, 1:K, F, T))
      stage_K                                <-
        cbind(rep(K, nrow(stage_K)), t(apply(stage_K, 1, rev)))
    }
    Lambda_null                              <- matrix(0, K*J, K*J)
    Lambdas                                  <- diag_sqrt_Is    <- Cov_taus <-
      sqrt_I_div_pars <- list()
    for (k in 1:nrow(stage_K)) {
      Lambda                                 <- Lambda_null
      NC                                     <- cumsum(1/(r*stage_K[k, ] + 1))
      NE                                     <- cumsum(r/(r*stage_K[k, ] + 1))
      for (j1 in seq_J) {
        j1_seq                               <- (1 + (j1 - 1)*K):(j1*K)
        diag_sqrt_Is[[j1]]                   <-
          diag(rep(sqrt(1/(sigma[1]^2/NC[j1] + sigma[2]^2/NE[j1])), K))
        Cov_taus[[j1]]                       <-
          diag(rep(sigma[2]^2/NE[j1], K)) + matrix(sigma[1]^2/NC[j1], K, K)
        Lambda[j1_seq, j1_seq]               <-
          diag_sqrt_Is[[j1]]%*%Cov_taus[[j1]]%*%diag_sqrt_Is[[j1]]
        for (j2 in seq_len(j1 - 1)) {
          j2_seq                             <- (1 + (j2 - 1)*K):(j2*K)
          Lambda[j1_seq, j2_seq]             <-
            diag_sqrt_Is[[j1]]%*%Cov_taus[[j1]]%*%diag_sqrt_Is[[j2]]
          Lambda[j2_seq, j1_seq]             <- t(Lambda[j1_seq, j2_seq])
        }
      }
      sqrt_I_div_pars[[k]]                   <-
        rep(sqrt(1/(sigma[1]^2/NC + sigma[2]^2/NE)), each = K)
      Lambdas[[k]]                           <- Lambda
    }
  }
  means_HG                                   <- l_indices_HG <- u_indices_HG <-
    Lambdas_HG <- list()
  for (i in 1:nrow(thetas_d_HG_a_fwer)) {
    relevant_indices                         <- NULL
    psi                                      <- thetas_d_HG_a_fwer[i, seq_K]
    omega                                    <- thetas_d_HG_a_fwer[i, seq_KpK]
    l_i                                      <- u_i <- numeric(sum(omega))
    for (k in seq_K) {
      seq_k                                  <- seq(k, by = K,
                                                    length.out = omega[k])
      relevant_indices                       <- c(relevant_indices, seq_k)
      l_i_k                                  <- u_i_k <- numeric(omega[k])
      for (j in 1:omega[k]) {
        if (omega[k] > j) {
          l_i_k[j]                           <- j
        } else if (all(psi[k] == 0, omega[k] == j)) {
          l_i_k[j]                           <- 2*J + 1
        } else if (all(psi[k] == 1, omega[k] == j)) {
          l_i_k[j]                           <- J + j
        }
        if (any(all(psi[k] == 0, omega[k] == j, max(omega) > j),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    psi[omega == j] == 0),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    sum(psi == 1) < d))) {
          u_i_k[j]                           <- j
        } else if (all(psi[k] == 1, omega[k] == j)) {
          u_i_k[j]                           <- 2*J + 2
        } else if (any(omega[k] > j,
                       all(psi[k] == 0, omega[k] == j, max(omega) == j,
                           sum(psi == 1) >= d))) {
          u_i_k[j]                           <- J + j
        }
      }
      l_i[seq_k]                             <- l_i_k
      u_i[seq_k]                             <- u_i_k
    }
    sort_relevant_indices                    <- sort(relevant_indices)
    means_HG[[i]]                            <-
      numeric(length(relevant_indices))
    if (type == "variable") {
      means_HG[[i]]                          <-
        numeric(length(relevant_indices))
      Lambdas_HG[[i]]                        <- Lambda[sort_relevant_indices,
                                                       sort_relevant_indices]
    } else {
      stage_k                                <-
        sapply(seq_J, function(j) {max(1, sum(omega >= j))})
      Lambdas_HG[[i]]                        <-
        Lambdas[[which(
          sapply(1:nrow(stage_K),
                 function(k) {sum(stage_K[k, ] ==
                                    stage_k)}) == J)]][sort_relevant_indices,
                                                       sort_relevant_indices]
    }
    l_indices_HG[[i]]                        <- l_i[sort_relevant_indices]
    u_indices_HG[[i]]                        <- u_i[sort_relevant_indices]
  }
  means_div_par_LFC                          <- l_indices_LFC <-
    u_indices_LFC <- Lambdas_LFC <-
    list()
  delta_LFC                                  <- rep(c(rep(delta1, c),
                                                      rep(delta0, K - c)), J)
  for (i in 1:nrow(thetas_bc_LFC_power)) {
    relevant_indices                         <- NULL
    psi                                      <- thetas_bc_LFC_power[i, seq_K]
    omega                                    <- thetas_bc_LFC_power[i, seq_KpK]
    l_i                                      <- u_i <- numeric(sum(omega))
    for (k in seq_K) {
      seq_k                                  <- seq(k, by = K,
                                                    length.out = omega[k])
      relevant_indices                       <- c(relevant_indices, seq_k)
      l_i_k                                  <- u_i_k <- numeric(omega[k])
      for (j in 1:omega[k]) {
        if (omega[k] > j) {
          l_i_k[j]                           <- j
        } else if (all(psi[k] == 0, omega[k] == j)) {
          l_i_k[j]                           <- 2*J + 1
        } else if (all(psi[k] == 1, omega[k] == j)) {
          l_i_k[j]                           <- J + j
        }
        if (any(all(psi[k] == 0, omega[k] == j, max(omega) > j),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    psi[omega == j] == 0),
                all(psi[k] == 0, omega[k] == j, max(omega) == j,
                    sum(psi == 1) < d))) {
          u_i_k[j]                           <- j
        } else if (all(psi[k] == 1, omega[k] == j)) {
          u_i_k[j]                           <- 2*J + 2
        } else if (any(omega[k] > j,
                       all(psi[k] == 0, omega[k] == j, max(omega) == j,
                           sum(psi == 1) >= d))) {
          u_i_k[j]                           <- J + j
        }
      }
      l_i[seq_k]                             <- l_i_k
      u_i[seq_k]                             <- u_i_k
    }
    sort_relevant_indices                    <- sort(relevant_indices)
    if (type == "variable") {
      means_div_par_LFC[[i]]                 <-
        delta_LFC[sort_relevant_indices]*sqrt_I_div_par[sort_relevant_indices]
      Lambdas_LFC[[i]]                       <- Lambda[sort_relevant_indices,
                                                       sort_relevant_indices]
    } else {
      stage_k                                <-
        sapply(seq_J, function(j) {max(1, sum(omega >= j))})
      which_k                                <-
        which(sapply(1:nrow(stage_K),
                     function(k) {sum(stage_K[k, ] == stage_k)}) == J)
      means_div_par_LFC[[i]]                 <-
        delta_LFC[sort_relevant_indices]*
        sqrt_I_div_pars[[which_k]][sort_relevant_indices]
      Lambdas_LFC[[i]]                       <-
        Lambdas[[which_k]][sort_relevant_indices, sort_relevant_indices]
    }
    l_indices_LFC[[i]]                       <- l_i[sort_relevant_indices]
    u_indices_LFC[[i]]                       <- u_i[sort_relevant_indices]
  }
  list(Jp1                      = J + 1,
       l_indices_HG             = l_indices_HG,
       l_indices_LFC            = l_indices_LFC,
       Lambdas_HG               = Lambdas_HG,
       Lambdas_LFC              = Lambdas_LFC,
       max_N_factor             = ifelse(type == "variable", J*(1 + r*K), J),
       means_HG                 = means_HG,
       means_div_par_LFC        = means_div_par_LFC,
       nrow_thetas_bc_LFC_power = nrow(thetas_bc_LFC_power),
       nrow_thetas_d_HG_a_fwer  = nrow(thetas_d_HG_a_fwer),
       pars_seq1                = seq_J + 1,
       pars_seq2                = seq_J[-1],
       pars_seq3                = (J + 2):(2*J),
       thetas_bc_LFC_power      = thetas_bc_LFC_power,
       thetas_d_HG_a_fwer       = thetas_d_HG_a_fwer,
       twoKp1                   = 2*K + 1,
       twoKp2                   = 2*K + 2,
       twoKp3                   = 2*K + 3,
       twoKp4                   = 2*K + 4,
       u_indices_HG             = u_indices_HG,
       u_indices_LFC            = u_indices_LFC)
}

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

obj_fn_gen_mams        <- function(pars, alpha, beta, r, w, N_fixed,
                                   max_N_factor, components) {
  bounds                                      <-
    c(pars[components$pars_seq1],
      pars[components$pars_seq2] + pars[components$pars_seq3],
      pars[components$Jp1],
      -Inf, Inf)
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
                       sqrt(pars[1])*components$means_div_par_LFC[[i]],
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
  sum(w*pars[1]*c(ESS_HG, ESS_LFC, max_N_factor)) +
    N_fixed*(as.numeric(a_fwer > alpha)*(a_fwer - alpha)/alpha +
               as.numeric(bc_typeII > beta)*(bc_typeII - beta)/beta)
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

opchar_dtl_internal    <- function(tau, Kv, r, sigma, e, nj0) {
  K                                <- Kv[1]
  components                       <- components_all_dtl(Kv, r, sigma, e)
  rows_tau                         <- nrow(tau)
  if (rows_tau == 1) {
    output                         <- opchar_dtl_internal_2(tau[1, ], r,
                                                            sigma, nj0, components)
    opchar                         <-
      matrix(c(tau, output), 1, byrow = T)
    pmf_N                          <- NULL
  } else {
    opchar                         <- cbind(tau, matrix(0, rows_tau, 3*K + 12))
    pmf_N                          <- NULL
    for (i in 1:rows_tau) {
      output                       <- opchar_dtl_internal_2(tau[i, ], r, sigma,
                                                            nj0, components)
      opchar[i, -components$seq_K] <- output
    }
  }
  colnames(opchar)                 <-
    c(paste0("tau", components$seq_K), "Pdis", "Pcon",
      paste0("P", components$seq_K), paste0("FWERI", components$seq_K),
      paste0("FWERII", components$seq_K), "PHER", "FDR", "pFDR", "FNDR", "Sens",
      "Spec", "ESS", "SDSS", "MSS", "maxN")
  list(opchar = tibble::as_tibble(opchar), pmf_N = pmf_N)
}

opchar_dtl_internal_2  <- function(tau, r, sigma, nj0, components) {
  neg_mat                                     <-
    matrix(tau <= 0, components$nrow_outcomes, components$K, byrow = T)
  tau                                         <- rep(tau, components$J)
  for (i in 1:components$nrow_outcomes) {
    components$outcomes[i, components$twoKp1] <-
      mvtnorm::pmvnorm(
        components$lowers[[i]], components$uppers[[i]],
        as.numeric(components$As[[i]]%*%(
          tau*sqrt(rep(((1:components$J)/(sigma[1]^2 + sigma[2]^2/r)),
                               each = components$Kv[1])))),
        sigma = components$Lambdas[[i]])[1]
  }
  pos_mat                                     <- !neg_mat
  false_discoveries                           <-
    Rfast::rowsums(components$outcomes[,
                                       components$K + components$seq_K]*neg_mat)
  false_non_discoveries                       <-
    Rfast::rowsums(components$inv_psi*pos_mat)
  core                                        <-
    c(sum(components$outcomes[components$disjunctive, components$twoKp1]),
      sum(components$outcomes[components$conjunctive, components$twoKp1]),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[components$outcomes[, components$K + k] == 1,
                                                   components$twoKp1]) }),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[false_discoveries >= k,
                                                   components$twoKp1]) }),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[false_non_discoveries >= k,
                                                   components$twoKp1]) }),
      sum(components$outcomes[, components$twoKp1]*false_discoveries)/
        components$K,
      sum(components$outcomes[, components$twoKp1]*false_discoveries/
            components$mod_discoveries),
      sum(components$outcomes[components$disjunctive, components$twoKp1]*
            false_discoveries[components$disjunctive]/
            components$discoveries[components$disjunctive])/
        sum(components$outcomes[components$disjunctive, components$twoKp1]),
      sum(components$outcomes[, components$twoKp1]*
            false_non_discoveries/components$mod_non_discoveries),
      sum(components$outcomes[, components$twoKp1]*
            Rfast::rowsums(components$outcomes[, components$seq_K]*pos_mat))/
        max(sum(tau[1:components$K] > 0), 1),
      sum(components$outcomes[, components$twoKp1]*
            Rfast::rowsums(components$inv_psi*neg_mat))/
        max(sum(tau[1:components$K] <= 0), 1))
  ess           <- nj0*(1 + r)*components$J
  vss           <- 0
  opchar        <- c(core, ess, sqrt(vss))
  poss_n        <- ess
  MSS   <- ess
  c(opchar, MSS, MSS)
}

opchar_mams_internal   <- function(tau, K, J, a, b, c, d, e, f, r, sigma, type,
                                   n_factor) {
  components                       <- components_all_mams(K, J, a, b, c, d, r,
                                                          sigma, type, n_factor)
  bounds                           <- c(f, e, -Inf, Inf)
  combs                            <-
    iterpc::getall(iterpc::iterpc(n       = components$K,
                                  r       = 2,
                                  labels  = 1:components$K,
                                  replace = T))
  rows_tau                         <- nrow(tau)
  if (rows_tau == 1) {
    output                         <- opchar_mams_internal_2(tau[1, ], bounds,
                                                             components)
    opchar                         <-
      matrix(c(tau, output$opchar), 1, byrow = T)
    pmf_N                          <- output$pmf_N
  } else {
    opchar                         <- cbind(tau, matrix(0, rows_tau,
                                                        3*K + 12 + nrow(combs)))
    pmf_N                          <- list()
    for (i in 1:rows_tau) {
      output                       <- opchar_mams_internal_2(tau[i, ], bounds,
                                                             components)
      opchar[i, -components$seq_K] <- output$opchar
      pmf_N[[i]]                   <- output$pmf_N
    }
  }
  powers_names                     <- character(nrow(combs))
  for (i in 1:nrow(combs)) {
    powers_names[i]                <- paste0(combs[i, ], collapse = "")
  }
  colnames(opchar)                 <-
    c(paste0("tau", components$seq_K), "Pdis", "Pcon",
      paste0("P", components$seq_K), paste0("FWERI", components$seq_K),
      paste0("FWERII", components$seq_K), "PHER", "FDR", "pFDR", "FNDR", "Sens",
      "Spec", paste0("P", powers_names), "ESS", "SDSS", "MSS", "maxN")
  list(opchar = tibble::as_tibble(opchar), pmf_N = pmf_N)
}

opchar_mams_internal_2 <- function(tau, bounds, components) {
  neg_mat                                     <-
    matrix(tau <= 0, components$nrow_outcomes, components$K, byrow = T)
  tau                                         <- rep(tau, components$J)
  for (i in 1:components$nrow_outcomes) {
    components$outcomes[i, components$twoKp2] <-
      mvtnorm::pmvnorm(bounds[components$l_indices[[i]]],
                       bounds[components$u_indices[[i]]],
                       tau[components$tau_indices[[i]]]*components$sqrt_I[[i]],
                       sigma = components$Lambdas[[i]])[1]
  }
  pos_mat                                     <- !neg_mat
  false_discoveries                           <-
    Rfast::rowsums(components$outcomes[, components$seq_K]*neg_mat)
  false_non_discoveries                       <-
    Rfast::rowsums(components$inv_psi*pos_mat)
  core                                        <-
    c(sum(components$outcomes[components$disjunctive, components$twoKp2]),
      sum(components$outcomes[components$conjunctive, components$twoKp2]),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[components$outcomes[, k] == 1,
                                                   components$twoKp2]) }),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[false_discoveries >= k,
                                                   components$twoKp2]) }),
      sapply(components$seq_K,
             function(k) { sum(components$outcomes[false_non_discoveries >= k,
                                                   components$twoKp2]) }),
      sum(components$outcomes[, components$twoKp2]*false_discoveries)/
        components$K,
      sum(components$outcomes[, components$twoKp2]*false_discoveries/
            components$mod_discoveries),
      sum(components$outcomes[components$disjunctive, components$twoKp2]*
            false_discoveries[components$disjunctive]/
            components$discoveries[components$disjunctive])/
        sum(components$outcomes[components$disjunctive, components$twoKp2]),
      sum(components$outcomes[, components$twoKp2]*
            false_non_discoveries/components$mod_non_discoveries),
      sum(components$outcomes[, components$twoKp2]*
            Rfast::rowsums(components$outcomes[, components$seq_K]*pos_mat))/
        max(sum(tau[1:components$K] > 0), 1),
      sum(components$outcomes[, components$twoKp2]*
            Rfast::rowsums(components$inv_psi*neg_mat))/
        max(sum(tau[1:components$K] <= 0), 1))
  ess           <- sum(components$outcomes[, components$twoKp2]*
                         components$outcomes[, components$twoKp2 - 1]*
                         components$n_factor)
  vss           <- max(sum(components$outcomes[, components$twoKp2]*
                             (components$outcomes[, components$twoKp2 - 1]*
                                components$n_factor)^2) - ess^2, 0)
  combs         <- iterpc::getall(iterpc::iterpc(n       = components$K,
                                                 r       = 2,
                                                 labels  = 1:components$K,
                                                 replace = T))
  powers        <- numeric(nrow(combs))
  for (i in 1:nrow(combs)) {
    powers[i]   <-
      sum(components$outcomes[which(Rfast::rowsums(
                                      components$outcomes[, 1:combs[i, 2],
                                                          drop = F]) >=
                                                              combs[i, 1]),
                              components$twoKp2])
  }
  opchar        <- c(core, powers, ess, sqrt(vss))
  poss_n        <- sort(unique(components$outcomes[, components$twoKp2 - 1]))
  pmf_N         <- matrix(0, nrow = length(poss_n), ncol = 2)
  for (i in 1:length(poss_n)) {
    pmf_N[i, ]  <- c(poss_n[i]*components$n_factor,
                     sum(components$outcomes[which(components$outcomes[, components$twoKp2 - 1] == poss_n[i]),
                                             components$twoKp2]))
  }
  cum_S <- cumsum(pmf_N[, 2])
  MSS   <- ifelse(any(cum_S == 0.5),
                  0.5*(pmf_N[which(cum_S == 0.5), 1] +
                         pmf_N[which(cum_S == 0.5) + 1, 1]),
                  pmf_N[which(cum_S > 0.5)[1], 1])
  colnames(pmf_N) <- c("N", "P")
  list(opchar = c(opchar, MSS, pmf_N[nrow(pmf_N), 1]), pmf_N = tibble::as_tibble(pmf_N))
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
      mvtnorm::pmvnorm(components$ls[[i]],
                       mean  = components$means[[i]],
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

root_bound_gen_ma      <- function(pars, K, alpha, components) {
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

root_bounds_dtl        <- function(e, Kv, alpha, J, outcomes, lowers, uppers,
                                   means_HG, Lambda) {
  for (i in 1:Kv[J]) {
    for (k in 1:Kv[J]) {
      if (outcomes[i, Kv[1] + k] == 1L) {
        lowers[[i]][nrow(Lambda) - Kv[J] + k] <- e
      } else {
        lowers[[i]][nrow(Lambda) - Kv[J] + k] <- -Inf
        uppers[[i]][nrow(Lambda) - Kv[J] + k] <- e
      }
    }
    outcomes[i, Kv[1] + Kv[J] + 1L]           <-
      mvtnorm::pmvnorm(lowers[[i]], uppers[[i]], means_HG, sigma = Lambda)[1]
  }
  factorial(Kv[1])*sum(outcomes[, Kv[1] + Kv[J] + 1L]) - alpha
}

root_bounds_mams       <- function(C, J, type, alpha, fshape, eshape, ffix,
                                   efix, components) {
  bounds <- bounds_mams(C, J, fshape, eshape, ffix, efix)$bounds
  fwer   <- 0
  for (i in which(components$thetas_d_HG_a_fwer[, components$twoKp2] > 0)) {
    fwer <- fwer + components$thetas_d_HG_a_fwer[i, components$twoKp2]*
                     mvtnorm::pmvnorm(bounds[components$l_indices_HG[[i]]],
                                      bounds[components$u_indices_HG[[i]]],
                                      components$means_HG[[i]],
                                      sigma = components$Lambdas_HG[[i]])[1]
  }
  fwer - alpha
}

root_ss_dtl            <- function(ss, Kv, beta, J, outcomes, lowers, uppers,
                                   means_LFC, Lambda) {
  for (i in 1:Kv[J]) {
    outcomes[i, Kv[1] + Kv[J] + 2L] <-
      mvtnorm::pmvnorm(lowers[[i]], uppers[[i]], sqrt(ss)*means_LFC,
                       sigma = Lambda)[1]
  }
  factorial(Kv[1] - 1)*sum(outcomes[, Kv[1] + Kv[J] + 2L]) - (1 - beta)
}

root_ss_gen_ma         <- function(pars, K, beta, components, e) {
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

root_ss_mams           <- function(ss, J, type, beta, components, bounds) {
  power   <- 0
  for (i in which(components$thetas_bc_LFC_power[, components$twoKp2] > 0)) {
    power <- power + components$thetas_bc_LFC_power[i, components$twoKp2]*
                       mvtnorm::pmvnorm(
                         bounds[components$l_indices_LFC[[i]]],
                         bounds[components$u_indices_LFC[[i]]],
                         sqrt(ss)*components$means_div_par_LFC[[i]],
                         sigma = components$Lambdas_LFC[[i]])[1]
  }
  power - (1 - beta)
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
