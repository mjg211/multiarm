bounds_gs                          <- function(C, comp) {
  if (comp$eshape == "obf") {
    e   <- C*sqrt(comp$J/comp$seq_J)
  } else if (comp$eshape == "pocock") {
    e   <- rep(C, comp$J)
  } else if (comp$eshape == "fixed") {
    e   <- c(rep(comp$efix, comp$Jm1), C)
  } else if (comp$eshape == "triangular") {
    e   <- C*(1 + comp$seq_J/comp$J)/sqrt(comp$seq_J)
  }
  if (comp$fshape == "obf") {
    f   <- c(-C*sqrt(comp$J/comp$seq_J[-comp$J]), e[comp$J])
  } else if (comp$fshape == "pocock") {
    f   <- c(rep(-C, comp$Jm1), e[comp$J])
  } else if (comp$fshape == "fixed") {
    f   <- c(rep(comp$ffix, comp$Jm1), e[comp$J])
  } else if (comp$fshape == "triangular") {
    if (comp$eshape == "triangular") {
      f <- -C*(1 - 3*comp$seq_J/comp$J)/sqrt(comp$seq_J)
    } else {
      f <- -0.5*C*(1 - 3*comp$seq_J/comp$J)*sqrt(comp$J/comp$seq_J)
    }
  }
  comp$bounds <- c(f, e, -Inf, Inf)
  comp$e      <- e
  comp$f      <- f
  comp
}

components_gs_all                  <- function(comp) {
  comp                                          <-
    components_gs_covariances_sqrt_Is(comp)
  perms_d                                       <-
    iterpc::getall(iterpc::iterpc(comp$twoJ, comp$K, ordered = TRUE,
                                  replace = TRUE))
  num_perms_d                                   <- nrow(perms_d)
  ceil_perms_obj                                <- ceiling(perms_d/2)
  outcomes                                      <-
    cbind(1*(perms_d%%2 == 0), ceil_perms_obj,
          comp$spacing[apply(ceil_perms_obj, 1, max)] +
            (comp$type == "variable")*
            Rfast::rowsums(matrix(comp$spacing[ceil_perms_obj], ncol = comp$K,
                                  byrow = FALSE)),
          numeric(num_perms_d))
  colnames(outcomes)                            <-
    c(paste0("psi_", comp$seq_K), paste0("omega_", comp$seq_K),
      ifelse(comp$type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|tau)")
  retain                                        <- !logical(num_perms_d)
  for (i in 1:num_perms_d) {
    for (j in comp$seq_J) {
      le                                        <-
        which(outcomes[i, comp$seq_KpK] <= j)
      if (all(length(le) > 0,
              length(which(outcomes[i, comp$seq_KpK] > j)) > 0)) {
        if (sum(outcomes[i, le]) >= comp$d) {
          retain[i]                             <- FALSE
        }
      }
    }
  }
  outcomes                                      <- outcomes[retain, ]
  nrow_outcomes                                 <- nrow(outcomes)
  Lambda_null                                   <- matrix(0, comp$JK, comp$JK)
  if (comp$type == "variable") {
    nrow_stage_K                                <- stage_K <- NULL
    Lambda                                      <- Lambda_null
    diag_sqrt_Is                                <- seq_js  <- list()
    for (j1 in comp$seq_J) {
      diag_sqrt_Is[[j1]]                        <-
        diag(comp$sqrt_Is[[1]][, comp$seq_js[[j1]]])
      fact                                      <-
        diag_sqrt_Is[[j1]]%*%comp$Cov_taus[[1]][[j1]]
      Lambda[comp$seq_js[[j1]],
             comp$seq_js[[j1]]]                 <- fact%*%diag_sqrt_Is[[j1]]
      for (j2 in seq_len(j1 - 1)) {
        Lambda[comp$seq_js[[j1]],
               comp$seq_js[[j2]]]               <-
          Lambda[comp$seq_js[[j2]],
                 comp$seq_js[[j1]]]             <- fact%*%diag_sqrt_Is[[j2]]
      }
    }
  } else {
    diag_sqrt_Is                                <- Lambda_ks <- seq_js <- list()
    numeric_JK                                  <- numeric(comp$JK)
    for (k in comp$seq_nrow_stage_K) {
      Lambda                                    <- Lambda_null
      for (j1 in comp$seq_J) {
        diag_sqrt_Is[[j1]]                      <-
          diag(comp$sqrt_Is[[1]][k, comp$seq_js[[j1]]])
        fact                                    <-
          diag_sqrt_Is[[j1]]%*%comp$Cov_taus[[1]][[k]][[j1]]
        Lambda[comp$seq_js[[j1]],
               comp$seq_js[[j1]]]               <- fact%*%diag_sqrt_Is[[j1]]
        for (j2 in seq_len(j1 - 1)) {
          Lambda[comp$seq_js[[j1]],
                 comp$seq_js[[j2]]]             <-
            Lambda[comp$seq_js[[j2]],
                   comp$seq_js[[j1]]]           <- fact%*%diag_sqrt_Is[[j2]]
        }
      }
      Lambda_ks[[k]]                            <- Lambda
    }
  }
  l_indices                                     <- Lambdas      <-
                                                   mean_factors <-
                                                   tau_indices  <- u_indices <-
                                                   list()
  seq_nrow_outcomes                             <- 1:nrow_outcomes
  for (i in seq_nrow_outcomes) {
    relevant_indices                            <- NULL
    psi                                         <-
      as.logical(outcomes[i, comp$seq_K])
    sum_psi                                     <- sum(psi)
    omega                                       <- outcomes[i, comp$seq_KpK]
    max_omega                                   <- max(omega)
    l_i                                         <- u_i   <-
      comp$numerics[[sum(omega)]]
    for (k in comp$seq_K) {
      seq_k                                     <- comp$seq_ks[[k]][[omega[k]]]
      relevant_indices                          <- c(relevant_indices, seq_k)
      l_i_k                                     <- u_i_k <-
        comp$numerics[[omega[k]]]
      for (j in comp$seqs[[omega[k]]]) {
        if (omega[k] > j) {
          l_i_k[j]                              <- j
        } else if (all(!psi[k], omega[k] == j)) {
          l_i_k[j]                              <- comp$twoJp1
        } else if (all(psi[k], omega[k] == j)) {
          l_i_k[j]                              <- comp$seq_JpJ[j]
        }
        if (any(all(!psi[k], omega[k] == j, max_omega > j),
                all(!psi[k], omega[k] == j, max_omega == j,
                    psi[omega == j] == 0),
                all(!psi[k], omega[k] == j, max_omega == j,
                    sum_psi < comp$d))) {
          u_i_k[j]                              <- j
        } else if (all(psi[k], omega[k] == j)) {
          u_i_k[j]                              <- comp$twoJp2
        } else if (any(omega[k] > j,
                       all(!psi[k], omega[k] == j, max_omega == j,
                           sum_psi >= comp$d))) {
          u_i_k[j]                              <- comp$seq_JpJ[j]
        }
      }
      l_i[seq_k]                                <- l_i_k
      u_i[seq_k]                                <- u_i_k
    }
    tau_indices[[i]]                            <- sort(relevant_indices)
    if (comp$type == "variable") {
      mean_factors[[i]]                         <-
        comp$sqrt_Is[[1]][, tau_indices[[i]]]
      Lambdas[[i]]                              <- Lambda[tau_indices[[i]],
                                                          tau_indices[[i]]]
    } else {
      stage_k                                   <-
        sapply(comp$seq_J, function(j) {max(1, sum(omega >= j))})
      which_k                                   <-
        which(sapply(comp$seq_nrow_stage_K,
                     function(k) {sum(comp$stage_K[k, ] == stage_k)}) == comp$J)
      mean_factors[[i]]                         <-
        comp$sqrt_Is[[1]][which_k, tau_indices[[i]]]
      Lambdas[[i]]                              <-
        Lambda_ks[[which_k]][tau_indices[[i]], tau_indices[[i]]]
    }
    l_indices[[i]]                              <- l_i[tau_indices[[i]]]
    u_indices[[i]]                              <- u_i[tau_indices[[i]]]
  }
  mod_discoveries                               <- discoveries     <-
    Rfast::rowsums(outcomes[, comp$seq_K])
  mod_non_discoveries                           <- non_discoveries <-
    comp$K - discoveries
  disjunctive                                   <- which(discoveries > 0)
  mod_discoveries[-disjunctive]                 <- 1L
  mod_non_discoveries[mod_non_discoveries == 0] <- 1L
  comp$conjunctive                              <- which(discoveries == comp$K)
  comp$discoveries                              <- discoveries
  comp$disjunctive                              <- disjunctive
  comp$inv_psi                                  <- (outcomes[, comp$seq_K] == 0)
  comp$Lambda_null                              <- Lambda_null
  comp$Lambdas                                  <- Lambdas
  comp$l_indices                                <- l_indices
  comp$mean_factors                             <- mean_factors
  comp$mod_discoveries                          <- mod_discoveries
  comp$mod_non_discoveries                      <- mod_non_discoveries
  comp$non_discoveries                          <- non_discoveries
  comp$nrow_outcomes                            <- nrow_outcomes
  comp$outcomes                                 <- outcomes
  comp$seq_nrow_outcomes                        <- seq_nrow_outcomes
  comp$tau_indices                              <- tau_indices
  comp$u_indices                                <- u_indices
  comp
}

components_gs_all_update_bern_pois <- function(comp, i) {
  if (comp$type == "variable") {
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
  } else {
    Lambda_null                       <- comp$Lambda_null
    diag_sqrt_Is                      <- Lambda_ks <- list()
    for (k in comp$seq_nrow_stage_K) {
      Lambda                          <- Lambda_null
      for (j1 in comp$seq_J) {
        diag_sqrt_Is[[j1]]            <-
          diag(comp$sqrt_Is[[i]][k, comp$seq_js[[j1]]])
        fact                          <-
          diag_sqrt_Is[[j1]]%*%comp$Cov_taus[[i]][[k]][[j1]]
        Lambda[comp$seq_js[[j1]],
               comp$seq_js[[j1]]]     <- fact%*%diag_sqrt_Is[[j1]]
        for (j2 in seq_len(j1 - 1)) {
          Lambda[comp$seq_js[[j1]],
                 comp$seq_js[[j2]]]   <-
            Lambda[comp$seq_js[[j2]],
                   comp$seq_js[[j1]]] <- fact%*%diag_sqrt_Is[[j2]]
        }
      }
      Lambda_ks[[k]]                  <- Lambda
    }
  }
  for (l in comp$seq_nrow_outcomes) {
    if (comp$type == "variable") {
      comp$mean_factors[[l]]          <-
        comp$sqrt_Is[[i]][, comp$tau_indices[[l]]]
      comp$Lambdas[[l]]               <-
        Lambda[comp$tau_indices[[l]], comp$tau_indices[[l]]]
    } else {
      omega                           <- comp$outcomes[l, comp$seq_KpK]
      stage_k                         <-
        sapply(comp$seq_J, function(j) {max(1, sum(omega >= j))})
      which_k                         <-
        which(sapply(comp$seq_nrow_stage_K,
                     function(k) {sum(comp$stage_K[k, ] == stage_k)}) ==
                comp$J)
      comp$mean_factors[[l]]          <-
        comp$sqrt_Is[[i]][which_k, comp$tau_indices[[l]]]
      comp$Lambdas[[l]]               <-
        Lambda_ks[[which_k]][comp$tau_indices[[l]],
                             comp$tau_indices[[l]]]
    }
  }
  comp
}

components_gs_covariances_sqrt_Is  <- function(comp) {
  Cov_taus                                    <- diag_sqrt_Is <- sqrt_Is <-
    list()
  if (comp$outcome == "norm") {
    if (comp$type == "variable") {
      NC                                      <- comp$n_factor*comp$spacing
      NE                                      <- comp$r*NC
      sqrt_Is_null                            <- matrix(0, 1, comp$JK)
      Cov_taus[[1]]                           <- list()
      sqrt_Is[[1]]                            <- sqrt_Is_null
      for (j in comp$seq_J) {
        VarC                                  <- comp$sigma[1, 1]^2/NC[j]
        VarE                                  <- comp$sigma[1, -1]^2/NE[j]
        sqrt_Is[[1]][, comp$seq_js[[j]]]      <- sqrt(1/(VarC + VarE))
        diag_sqrt_Is[[j]]                     <-
          diag(sqrt_Is[[1]][, comp$seq_js[[j]]])
        Cov_taus[[1]][[j]]                    <- matrix(VarC, comp$K, comp$K) +
          diag(VarE)
      }
    } else {
      NC                                      <- matrix(0, comp$nrow_stage_K,
                                                        comp$J)
      for (k in 1:comp$nrow_stage_K) {
        NC[k, 1]                              <-
          comp$n_factor/(comp$r*comp$K + 1)
        for (j in comp$seq_J[-1]) {
          NC[k, j]                            <- NC[k, j - 1] +
            comp$n_factor*(comp$spacing[j] - comp$spacing[j - 1])/
            (comp$r*comp$stage_K[k, j] + 1)
        }
      }
      NE                                      <- comp$r*NC
      sqrt_Is_null                            <- matrix(0, comp$nrow_stage_K,
                                                        comp$JK)
      Cov_taus[[1]]                           <- list()
      sqrt_Is[[1]]                            <- sqrt_Is_null
      for (k in 1:comp$nrow_stage_K) {
        Cov_taus[[1]][[k]]                    <- list()
        for (j in comp$seq_J) {
          VarC                              <- comp$sigma[1, 1]^2/NC[k, j]
          VarE                              <- comp$sigma[1, -1]^2/NE[k, j]
          sqrt_Is[[1]][k, comp$seq_js[[j]]] <- sqrt(1/(VarC + VarE))
          diag_sqrt_Is[[j]]                 <-
            diag(sqrt_Is[[1]][k, comp$seq_js[[j]]])
          Cov_taus[[1]][[k]][[j]]           <- matrix(VarC, comp$K, comp$K) +
            diag(VarE)
        }
      }
    }
    if (nrow(comp$sigma) > 1) {
      for (i in 1:nrow(comp$sigma)) {
        Cov_taus[[i]]                       <- Cov_taus[[1]]
        sqrt_Is[[i]]                        <- sqrt_Is[[1]]
      }
    }
  } else if (comp$outcome %in% c("bern", "pois")) {
    if (comp$type == "variable") {
      NC                                      <- comp$n_factor*comp$spacing
      NE                                      <- comp$r*NC
      sqrt_Is_null                            <- matrix(0, 1, comp$JK)
      for (i in 1:nrow(comp$sigma)) {
        Cov_taus[[i]]                         <- list()
        sqrt_Is[[i]]                          <- sqrt_Is_null
        for (j in comp$seq_J) {
          VarC                                <- comp$sigma[i, 1]^2/NC[j]
          VarE                                <- comp$sigma[i, -1]^2/NE[j]
          sqrt_Is[[i]][, comp$seq_js[[j]]]    <- sqrt(1/(VarC + VarE))
          diag_sqrt_Is[[j]]                   <-
            diag(sqrt_Is[[i]][, comp$seq_js[[j]]])
          Cov_taus[[i]][[j]]                  <- matrix(VarC, comp$K, comp$K) +
            diag(VarE)
        }
      }
    } else {
      NC                                      <- matrix(0, comp$nrow_stage_K,
                                                        comp$J)
      for (k in 1:comp$nrow_stage_K) {
        NC[k, 1]                              <-
          comp$n_factor/(comp$r*comp$K + 1)
        for (j in comp$seq_J[-1]) {
          NC[k, j]                            <- NC[k, j - 1] +
            comp$n_factor*(comp$spacing[j] - comp$spacing[j - 1])/
            (comp$r*comp$stage_K[k, j] + 1)
        }
      }
      NE                                      <- comp$r*NC
      sqrt_Is_null                            <- matrix(0, comp$nrow_stage_K,
                                                        comp$JK)
      for (i in 1:nrow(comp$sigma)) {
        Cov_taus[[i]]                         <- list()
        sqrt_Is[[i]]                          <- sqrt_Is_null
        for (k in 1:comp$nrow_stage_K) {
          Cov_taus[[i]][[k]]                  <- list()
          for (j in comp$seq_J) {
            VarC                              <- comp$sigma[i, 1]^2/NC[k, j]
            VarE                              <- comp$sigma[i, -1]^2/NE[k, j]
            sqrt_Is[[i]][k, comp$seq_js[[j]]] <- sqrt(1/(VarC + VarE))
            diag_sqrt_Is[[j]]                 <-
              diag(sqrt_Is[[i]][k, comp$seq_js[[j]]])
            Cov_taus[[i]][[k]][[j]]           <- matrix(VarC, comp$K, comp$K) +
              diag(VarE)
          }
        }
      }
    }
  }
  comp$Cov_taus                               <- Cov_taus
  comp$sqrt_Is                                <- sqrt_Is
  comp
}

components_gs_hg_lfc               <- function(comp) {
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
  comp                                           <-
    components_gs_covariances_sqrt_Is(comp)
  perms_d_HG                                     <- perms_K_HG <-
    iterpc::getall(iterpc::iterpc(2*comp$J, comp$K, replace = TRUE))
  num_perms_d_HG                                 <- nrow(perms_d_HG)
  seq_num_perms_d_HG                             <- 1:num_perms_d_HG
  deg_d_HG_a_fwer                                <- deg_d_HG   <- prob_col <-
    integer(num_perms_d_HG)
  for (i in seq_num_perms_d_HG) {
    deg_d_HG[i]                                  <-
      nrow(iterpc::getall(iterpc::iterpc(table(perms_d_HG[i, ]),
                                         ordered = TRUE)))
  }
  fact                                           <- (perms_d_HG%%2 == 0)
  perms_d_HG_a_fwer_indices                      <-
    (Rfast::rowsums(fact) >= comp$a)
  deg_d_HG_a_fwer[perms_d_HG_a_fwer_indices]     <-
    deg_d_HG[perms_d_HG_a_fwer_indices]
  ceil_perms_obj                                 <- ceiling(perms_d_HG/2)
  thetas_d_HG_a_fwer                             <-
    cbind(1*fact, ceil_perms_obj, deg_d_HG, deg_d_HG_a_fwer,
          comp$spacing[apply(ceil_perms_obj, 1, max)] +
            (comp$type == "variable")*
            Rfast::rowsums(matrix(comp$spacing[ceil_perms_obj], ncol = comp$K,
                                  byrow = FALSE)), prob_col)
  colnames(thetas_d_HG_a_fwer)                   <-
    c(paste0("psi_", comp$seq_K), paste0("omega_", comp$seq_K),
      "deg_{HG}(psi,omega)", "deg_{HG,a-FWER}(psi,omega)",
      ifelse(comp$type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|HG)")
  retain                                         <- !logical(num_perms_d_HG)
  for (i in seq_num_perms_d_HG) {
    for (j in comp$seq_J) {
      le                                         <-
        which(thetas_d_HG_a_fwer[i, comp$seq_KpK] <= j)
      if (all(length(le) > 0,
              length(which(thetas_d_HG_a_fwer[i, comp$seq_KpK] > j)) > 0)) {
        if (sum(thetas_d_HG_a_fwer[i, le]) >= comp$d) {
          retain[i]                              <- FALSE
        }
      }
    }
  }
  thetas_d_HG_a_fwer                             <- thetas_d_HG_a_fwer[retain, ]
  if (!is.na(comp$w)) {
    if (comp$w[1] == 0) {
      thetas_d_HG_a_fwer                         <-
        thetas_d_HG_a_fwer[(thetas_d_HG_a_fwer[, comp$twoKp2] > 0), ]
    }
  }
  nrow_thetas_d_HG_a_fwer                        <- nrow(thetas_d_HG_a_fwer)
  perms_c_delta1                                 <-
    iterpc::getall(iterpc::iterpc(comp$twoJ, comp$c, replace = TRUE))
  num_perms_c_delta1                             <- nrow(perms_c_delta1)
  deg_c_delta1                                   <- integer(num_perms_c_delta1)
  for (i in 1:num_perms_c_delta1) {
    deg_c_delta1[i]                              <-
      nrow(iterpc::getall(iterpc::iterpc(table(perms_c_delta1[i, ]),
                                         ordered = TRUE)))
  }
  perms_c_delta1_b_power_indices                 <-
    (Rfast::rowsums(perms_c_delta1%%2 == 0) >= comp$b)
  deg_c_delta1_b_power                           <- integer(num_perms_c_delta1)
  deg_c_delta1_b_power[perms_c_delta1_b_power_indices] <-
    deg_c_delta1[perms_c_delta1_b_power_indices]
  if (comp$c < comp$K) {
    perms_Kmin_c_delta0                          <-
      iterpc::getall(iterpc::iterpc(comp$twoJ, comp$K - comp$c, replace = TRUE))
    num_perms_Kmin_c_delta0                      <- nrow(perms_Kmin_c_delta0)
    deg_Kmin_c_delta0                            <-
      integer(num_perms_Kmin_c_delta0)
    for (i in 1:num_perms_Kmin_c_delta0) {
      deg_Kmin_c_delta0[i]                       <-
        nrow(iterpc::getall(iterpc::iterpc(table(perms_Kmin_c_delta0[i, ]),
                                           ordered = TRUE)))
    }
    perms_K_d_bc_LFC                             <-
      matrix(0L, num_perms_c_delta1*num_perms_Kmin_c_delta0, comp$K)
    for (i in 1:num_perms_c_delta1) {
      range                                      <-
        (1 + (i - 1)*num_perms_Kmin_c_delta0):(i*num_perms_Kmin_c_delta0)
      i_matrix                                   <-
        matrix(perms_c_delta1[i, ], num_perms_Kmin_c_delta0, comp$c,
               byrow = TRUE)
      perms_K_d_bc_LFC[range, ]                  <- cbind(i_matrix,
                                                          perms_Kmin_c_delta0)
    }
    deg_K_d_bc_LFC                               <-
      rep(deg_c_delta1, each = num_perms_Kmin_c_delta0)*
      rep(deg_Kmin_c_delta0, num_perms_c_delta1)
    deg_K_d_bc_LFC_power                         <-
      rep(deg_c_delta1_b_power, each = num_perms_Kmin_c_delta0)*
      rep(deg_Kmin_c_delta0, num_perms_c_delta1)
    ceil_perms_obj                               <- ceiling(perms_K_d_bc_LFC/2)
    thetas_bc_LFC_power                          <-
      cbind(1*(perms_K_d_bc_LFC%%2 == 0), ceil_perms_obj, deg_K_d_bc_LFC,
            deg_K_d_bc_LFC_power,
            comp$spacing[apply(ceil_perms_obj, 1, max)] +
              (comp$type == "variable")*
              Rfast::rowsums(matrix(comp$spacing[ceil_perms_obj], ncol = comp$K,
                                    byrow = FALSE)),
            numeric(num_perms_c_delta1*num_perms_Kmin_c_delta0))
  } else {
    ceil_perms_obj                               <- ceiling(perms_c_delta1/2)
    thetas_bc_LFC_power                          <-
      cbind(1*(perms_c_delta1%%2 == 0), ceil_perms_obj, deg_c_delta1,
            deg_c_delta1_b_power,
            comp$spacing[apply(ceil_perms_obj, 1, max)] +
              (comp$type == "variable")*
              Rfast::rowsums(matrix(comp$spacing[ceil_perms_obj], ncol = comp$K,
                                    byrow = FALSE)),
            numeric(num_perms_c_delta1))
  }
  colnames(thetas_bc_LFC_power)                  <-
    c(paste0("psi_", comp$seq_K), paste0("omega_", comp$seq_K),
      "deg_{LFC}(psi,omega)", "deg_{LFC,bc-power}(psi,omega)",
      ifelse(comp$type == "variable", "N(omega)/n", "N(omega)/n_tilde"),
      "P(psi,omega|LFC)")
  num_thetas_bc_LFC_power                        <- nrow(thetas_bc_LFC_power)
  retain                                         <-
    !logical(num_thetas_bc_LFC_power)
  for (i in 1:num_thetas_bc_LFC_power) {
    for (j in comp$seq_J) {
      le                                         <-
        which(thetas_bc_LFC_power[i, comp$seq_KpK] <= j)
      if (all(length(le) > 0,
              length(which(thetas_bc_LFC_power[i, comp$seq_KpK] > j)) > 0)) {
        if (sum(thetas_bc_LFC_power[i, le]) >= comp$d) {
          retain[i]                              <- FALSE
        }
      }
    }
  }
  thetas_bc_LFC_power                            <-
    thetas_bc_LFC_power[retain, ]
  if (!is.na(comp$w)) {
    if (comp$w[2] == 0) {
      thetas_bc_LFC_power                        <-
        thetas_bc_LFC_power[(thetas_bc_LFC_power[, comp$twoKp2] > 0), ]
    }
  }
  nrow_thetas_bc_LFC_power                       <- nrow(thetas_bc_LFC_power)
  if (comp$type == "variable") {
    Lambda_0                                     <- Lambda_1       <-
      matrix(0, comp$JK, comp$JK)
    diag_sqrt_Is_0                               <- diag_sqrt_Is_1 <- seq_js <-
      list()
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
  } else {
    Lambda_null                                  <- matrix(0, comp$JK, comp$JK)
    diag_sqrt_Is_0                               <- diag_sqrt_Is_1 <-
                                                    Lambdas_0      <-
                                                    Lambdas_1      <- list()
    for (k in 1:comp$nrow_stage_K) {
      Lambda_0                                   <- Lambda_1       <-
                                                    Lambda_null
      for (j1 in comp$seq_J) {
        diag_sqrt_Is_0[[j1]]                     <-
          diag(comp$sqrt_Is[[1]][k, comp$seq_js[[j1]]])
        fact_0                                   <-
          diag_sqrt_Is_0[[j1]]%*%comp$Cov_taus[[1]][[k]][[j1]]
        Lambda_0[comp$seq_js[[j1]],
                 comp$seq_js[[j1]]]              <-
          fact_0%*%diag_sqrt_Is_0[[j1]]
        diag_sqrt_Is_1[[j1]]                     <-
          diag(comp$sqrt_Is[[2]][k, comp$seq_js[[j1]]])
        fact_1                                   <-
          diag_sqrt_Is_1[[j1]]%*%comp$Cov_taus[[2]][[k]][[j1]]
        Lambda_1[comp$seq_js[[j1]],
                 comp$seq_js[[j1]]]              <-
          fact_1%*%diag_sqrt_Is_1[[j1]]
        for (j2 in seq_len(j1 - 1)) {
          Lambda_0[comp$seq_js[[j1]],
                   comp$seq_js[[j2]]]            <-
            Lambda_0[comp$seq_js[[j2]],
                     comp$seq_js[[j1]]]          <-
            fact_0%*%diag_sqrt_Is_0[[j2]]
          Lambda_1[comp$seq_js[[j1]],
                   comp$seq_js[[j2]]]            <-
            Lambda_1[comp$seq_js[[j2]],
                     comp$seq_js[[j1]]]          <-
            fact_1%*%diag_sqrt_Is_1[[j2]]
        }
      }
      Lambdas_0[[k]]                             <- Lambda_0
      Lambdas_1[[k]]                             <- Lambda_1
    }
  }
  means_HG                                       <- l_indices_HG <-
                                                    Lambdas_HG   <-
                                                    u_indices_HG <- list()
  for (i in 1:nrow_thetas_d_HG_a_fwer) {
    relevant_indices                             <- NULL
    psi                                          <-
      as.logical(thetas_d_HG_a_fwer[i, comp$seq_K])
    sum_psi                                      <- sum(psi)
    omega                                        <-
      thetas_d_HG_a_fwer[i, comp$seq_KpK]
    max_omega                                    <- max(omega)
    l_i                                          <- u_i   <-
      comp$numerics[[sum(omega)]]
    for (k in comp$seq_K) {
      seq_k                                      <- comp$seq_ks[[k]][[omega[k]]]
      relevant_indices                           <- c(relevant_indices, seq_k)
      l_i_k                                      <- u_i_k <-
        comp$numerics[[omega[k]]]
      for (j in comp$seqs[[omega[k]]]) {
        if (omega[k] > j) {
          l_i_k[j]                               <- j
        } else if (all(!psi[k], omega[k] == j)) {
          l_i_k[j]                               <- comp$twoJp1
        } else if (all(psi[k], omega[k] == j)) {
          l_i_k[j]                               <- comp$seq_JpJ[j]
        }
        if (any(all(!psi[k], omega[k] == j, max_omega > j),
                all(!psi[k], omega[k] == j, max_omega == j,
                    psi[omega == j] == 0),
                all(!psi[k], omega[k] == j, max_omega == j,
                    sum_psi < comp$d))) {
          u_i_k[j]                               <- j
        } else if (all(psi[k] == 1, omega[k] == j)) {
          u_i_k[j]                               <- comp$twoJp2
        } else if (any(omega[k] > j,
                       all(!psi[k], omega[k] == j, max_omega == j,
                           sum_psi >= comp$d))) {
          u_i_k[j]                               <- comp$seq_JpJ[j]
        }
      }
      l_i[seq_k]                                 <- l_i_k
      u_i[seq_k]                                 <- u_i_k
    }
    sort_relevant_indices                        <- sort(relevant_indices)
    means_HG[[i]]                                <-
      comp$numerics[[length(relevant_indices)]]
    if (comp$type == "variable") {
      Lambdas_HG[[i]]                            <-
        Lambda_0[sort_relevant_indices, sort_relevant_indices]
    } else {
      stage_k                                    <-
        sapply(comp$seq_J, function(j) {max(1, sum(omega >= j))})
      Lambdas_HG[[i]]                            <-
        Lambdas_0[[which(
          sapply(comp$seq_nrow_stage_K,
                 function(k) {sum(comp$stage_K[k, ] ==
                                    stage_k)}) ==
            comp$J)]][sort_relevant_indices, sort_relevant_indices]
    }
    l_indices_HG[[i]]                            <- l_i[sort_relevant_indices]
    u_indices_HG[[i]]                            <- u_i[sort_relevant_indices]
  }
  means_factors_LFC                              <- l_indices_LFC <-
                                                    Lambdas_LFC   <-
                                                    u_indices_LFC <- list()
  delta_LFC                                      <- rep(comp$tau_power, comp$J)
  for (i in 1:nrow_thetas_bc_LFC_power) {
    relevant_indices                             <- NULL
    psi                                          <-
      as.logical(thetas_bc_LFC_power[i, comp$seq_K])
    sum_psi                                      <- sum(psi)
    omega                                        <-
      thetas_bc_LFC_power[i, comp$seq_KpK]
    max_omega                                    <- max(omega)
    l_i                                          <- u_i <-
      comp$numerics[[sum(omega)]]
    for (k in comp$seq_K) {
      seq_k                                      <- comp$seq_ks[[k]][[omega[k]]]
      relevant_indices                           <- c(relevant_indices, seq_k)
      l_i_k                                      <- u_i_k <-
        comp$numerics[[omega[k]]]
      for (j in comp$seqs[[omega[k]]]) {
        if (omega[k] > j) {
          l_i_k[j]                               <- j
        } else if (all(!psi[k], omega[k] == j)) {
          l_i_k[j]                               <- comp$twoJp1
        } else if (all(psi[k], omega[k] == j)) {
          l_i_k[j]                               <- comp$seq_JpJ[j]
        }
        if (any(all(!psi[k], omega[k] == j, max_omega > j),
                all(!psi[k], omega[k] == j, max_omega == j,
                    psi[omega == j] == 0),
                all(!psi[k], omega[k] == j, max_omega == j,
                    sum_psi < comp$d))) {
          u_i_k[j]                               <- j
        } else if (all(psi[k], omega[k] == j)) {
          u_i_k[j]                               <- comp$twoJp2
        } else if (any(omega[k] > j,
                       all(!psi[k], omega[k] == j, max_omega == j,
                           sum_psi >= comp$d))) {
          u_i_k[j]                               <- comp$seq_JpJ[j]
        }
      }
      l_i[seq_k]                                 <- l_i_k
      u_i[seq_k]                                 <- u_i_k
    }
    sort_relevant_indices                        <- sort(relevant_indices)
    if (comp$type == "variable") {
      means_factors_LFC[[i]]                     <-
        delta_LFC[sort_relevant_indices]*
        comp$sqrt_Is[[2]][, sort_relevant_indices]
      Lambdas_LFC[[i]]                           <-
        Lambda_1[sort_relevant_indices, sort_relevant_indices]
    } else {
      stage_k                                    <-
        sapply(comp$seq_J, function(j) {max(1, sum(omega >= j))})
      which_k                                    <-
        which(sapply(comp$seq_nrow_stage_K,
                     function(k) {sum(comp$stage_K[k, ] == stage_k)}) == comp$J)
      means_factors_LFC[[i]]                     <-
        delta_LFC[sort_relevant_indices]*
        comp$sqrt_Is[[2]][which_k, sort_relevant_indices]
      Lambdas_LFC[[i]]                           <-
        Lambdas_1[[which_k]][sort_relevant_indices, sort_relevant_indices]
    }
    l_indices_LFC[[i]]                           <- l_i[sort_relevant_indices]
    u_indices_LFC[[i]]                           <- u_i[sort_relevant_indices]
  }
  comp$l_indices_HG                              <- l_indices_HG
  comp$l_indices_LFC                             <- l_indices_LFC
  comp$Lambdas_HG                                <- Lambdas_HG
  comp$Lambdas_LFC                               <- Lambdas_LFC
  comp$means_HG                                  <- means_HG
  comp$means_factors_LFC                         <- means_factors_LFC
  comp$nrow_thetas_bc_LFC_power                  <- nrow_thetas_bc_LFC_power
  comp$nrow_thetas_d_HG_a_fwer                   <- nrow_thetas_d_HG_a_fwer
  comp$pars_seq1                                 <- comp$seq_J + 1
  comp$pars_seq2                                 <- comp$seq_J[-1]
  comp$pars_seq3                                 <- (comp$J + 2):(2*comp$J)
  comp$thetas_bc_LFC_power                       <- thetas_bc_LFC_power
  comp$thetas_d_HG_a_fwer                        <- thetas_d_HG_a_fwer
  comp$u_indices_HG                              <- u_indices_HG
  comp$u_indices_LFC                             <- u_indices_LFC
  comp
}

components_gs_init                 <- function(alpha, beta, delta0, delta1,
                                               efix, eshape, ffix, fshape,
                                               integer, J, K, power, ratio,
                                               stopping, summary, type, sigma,
                                               pi0, lambda0, n_factor = 1,
                                               f = NULL, e = NULL) {
  a                    <- 1L
  if (power == "marginal") {
    b                  <- c <- 1L
  } else if (power == "conjunctive") {
    b                  <- c <- K
  } else if (power == "disjunctive") {
    b                  <- 1L
    c                  <- K
  }
  if (stopping == "simultaneous") {
    d                  <- 1L
  } else if (stopping == "separate") {
    d                  <- K
  }
  Kp1                  <- K + 1
  Kp2                  <- K + 2
  Jm1                  <- J - 1
  JK                   <- J*K
  numerics             <- seqs <- seq_js <- seq_ks <- list()
  seq_J                <- 1:J
  seq_K                <- 1:K
  for (j in seq_J) {
    seq_js[[j]]        <- (1 + (j - 1)*K):(j*K)
    seqs[[j]]          <- 1:j
  }
  for (j in 1:JK) {
    numerics[[j]]      <- numeric(j)
  }
  for (k in seq_K) {
    seq_ks[[k]]        <- list()
    for (j in seq_J) {
      seq_ks[[k]][[j]] <- seq(k, by = K, length.out = j)
    }
  }
  if (type == "fixed") {
    if (J == 2) {
      stage_K          <- cbind(rep(K, K), seq_K)
    } else {
      stage_K          <- iterpc::getall(iterpc::iterpc(K, Jm1, seq_K, FALSE,
                                                        TRUE))
      stage_K          <- cbind(K, t(apply(stage_K, 1, rev)))
    }
    nrow_stage_K       <- nrow(stage_K)
    seq_nrow_stage_K   <- 1:nrow_stage_K
  } else {
    stage_K            <- nrow_stage_K <- seq_nrow_stage_K <- NULL
  }
  if (!missing(sigma)) {
    outcome            <- "norm"
    pi0                <- lambda0 <- NA
  } else if (!missing(pi0)) {
    outcome            <- "bern"
    sigma              <- lambda0 <- NA
  } else if (!missing(lambda0)) {
    outcome            <- "pois"
    sigma              <- pi0 <- NA
  }
  if (all(!is.null(f), !is.null(e))) {
    bounds             <- c(f, e, -Inf, Inf)
  } else {
    bounds             <- NULL
  }
  spacing              <- seq_J
  list(a = a, alpha = alpha, b = b, beta = beta, bounds = bounds, c = c, d = d,
       delta0 = delta0, delta1 = delta1, e = e, efix = efix, eshape = eshape,
       f = f, ffix = ffix, fshape = fshape, integer = integer, J = J,
       Jm1 = J - 1, Jp1 = J + 1, JK = JK, K = K, Kp1 = K + 1, Kp2 = K + 2,
       lambda0 = lambda0,
       max_N_factor = ifelse(type == "variable", J*(1 + ratio*K), J),
       n_factor = n_factor, nrow_stage_K = nrow_stage_K, numerics = numerics,
       outcome = outcome, pi0 = pi0, power = power, r = ratio, seqs = seqs,
       seq_J = seq_J, seq_JpJ = J + seq_J, seq_js = seq_js, seq_K = seq_K,
       seq_ks = seq_ks, seq_KpK = 1:K + K, seq_nrow_stage_K = seq_nrow_stage_K,
       sigma = sigma, spacing = spacing, stage_K = stage_K, stopping = stopping,
       summary = summary, twoJ = 2*J, twoJp1 = 2*J + 1, twoJp2 = 2*J + 2,
       twoKp1 = 2*K + 1, twoKp2 = 2*K + 2, twoKp3 = 2*K + 3, twoKp4 = 2*K + 4,
       type = type, w = NA)
}

components_gs_update               <- function(comp, tau, pi, lambda) {
  if (comp$outcome == "norm") {
    comp$tau      <- tau
    comp$nrow_tau <- nrow(comp$tau)
    if (is.matrix(comp$sigma)) {
      comp$sigma  <- matrix(comp$sigma[1, ], comp$nrow_tau, comp$Kp1,
                            byrow = TRUE)
    } else {
      if (length(comp$sigma) == 1) {
        comp$sigma <- matrix(comp$sigma, 2, comp$Kp1)
      } else {
        comp$sigma <- cbind(comp$sigma[1], matrix(comp$sigma[2], 2, comp$K))
      }
      comp$sigma  <- matrix(comp$sigma[1, ], comp$nrow_tau, comp$Kp1,
                            byrow = TRUE)
    }
  } else if (comp$outcome == "bern") {
    comp$pi       <- pi
    comp$tau      <- pi[, -1] - pi[, 1]
    comp$nrow_pi  <- comp$nrow_tau <- nrow(comp$pi)
    comp$sigma    <- sqrt(comp$pi*(1 - comp$pi))
  } else if (comp$outcome == "pois") {
    comp$lambda   <- lambda
    comp$tau      <- lambda[, -1] - lambda[, 1]
    comp$nrow_lambda <- comp$nrow_tau <- nrow(comp$lambda)
    comp$sigma    <- sqrt(comp$lambda)
  }
  components_gs_all(comp)
}

integer_gs                         <- function(comp) {
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

opchar_gs_internal                 <- function(comp) {
  #comp$combs          <- iterpc::getall(iterpc::iterpc(n       = comp$K,
  #                                                     r       = 2,
  #                                                     labels  = comp$seq_K,
  #                                                     replace = TRUE))
  #comp$nrow_combs     <- nrow(comp$combs)
  #comp$seq_nrow_combs <- 1:comp$nrow_combs
  opchar              <- matrix(0, comp$nrow_tau,
                                3*comp$K + 13) #+ comp$nrow_combs)
  comp$poss_n         <-
    comp$n_factor*sort(unique(comp$outcomes[, comp$twoKp1]))
  comp$len_poss_n     <- length(comp$poss_n)
  pmf_N               <- cbind(rep(comp$poss_n, comp$nrow_tau), 0)
  summary_message     <- ceiling(seq(0.1, 1, 0.1)*comp$nrow_tau)
  for (i in 1:comp$nrow_tau) {
    output            <- opchar_gs_internal_2(i, comp)
    opchar[i, ]       <- output$opchar
    range             <- (1 + (i - 1)*comp$len_poss_n):(i*comp$len_poss_n)
    pmf_N[range, 2]   <- output$pmf_N
    if (all(i %in% summary_message, comp$summary)) {
      which_match     <- which(summary_message == i)
      message("..", 10*which_match[length(which_match)], "% complete..")
    }
  }
  #powers_names        <- character(comp$nrow_combs)
  #for (i in 1:comp$nrow_combs) {
  #  powers_names[i]   <- paste0(comp$combs[i, ], collapse = "")
  #}
  if (comp$outcome == "norm") {
    opchar            <- cbind(comp$tau, opchar)
    pmf_N             <- cbind(matrix(rep(as.vector(comp$tau),
                                          each = comp$len_poss_n),
                                      comp$nrow_tau*comp$len_poss_n, comp$K),
                               pmf_N)
    fact              <- paste0("tau", comp$seq_K)
  } else if (comp$outcome == "bern") {
    opchar            <- cbind(comp$pi, opchar)
    pmf_N             <- cbind(matrix(rep(as.vector(comp$pi),
                                          each = comp$len_poss_n),
                                      comp$nrow_tau*comp$len_poss_n,
                                      comp$Kp1), pmf_N)
    fact              <- paste0("pi", c(0, comp$seq_K))
  } else if (comp$outcome == "pois") {
    opchar            <- cbind(comp$lambda, opchar)
    pmf_N             <- cbind(matrix(rep(as.vector(comp$lambda),
                                          each = comp$len_poss_n),
                                      comp$nrow_tau*comp$len_poss_n,
                                      comp$Kp1), pmf_N)
    fact              <- paste0("lambda", c(0, comp$seq_K))
  }
  colnames(pmf_N)     <- c(fact, "n", "Prob")
  colnames(opchar)    <- c(fact, "Pdis", "Pcon", paste0("P", comp$seq_K),
                           paste0("FWERI", comp$seq_K),
                           paste0("FWERII", comp$seq_K), "PHER", "FDR", "pFDR",
                           "FNDR", "Sens", "Spec",# paste0("P", powers_names),
                           "ESS", "SDSS", "MeSS", "MoSS", "maxN")
  comp$opchar         <- tibble::as_tibble(opchar, .name_repair = "minimal")
  comp$pmf_N          <- tibble::as_tibble(pmf_N, .name_repair = "minimal")
  comp
}

opchar_gs_internal_2               <- function(i, comp) {
  if (comp$outcome %in% c("bern", "pois")) {
    comp                          <- components_gs_all_update_bern_pois(comp, i)
  }
  neg_mat                         <-
    matrix(comp$tau[i, ] <= 0, comp$nrow_outcomes, comp$K, byrow = TRUE)
  tau                             <- rep(comp$tau[i, ], comp$J)
  for (i in comp$seq_nrow_outcomes) {
    comp$outcomes[i, comp$twoKp2] <-
      mvtnorm::pmvnorm(comp$bounds[comp$l_indices[[i]]],
                       comp$bounds[comp$u_indices[[i]]],
                       tau[comp$tau_indices[[i]]]*comp$mean_factors[[i]],
                       sigma = comp$Lambdas[[i]])[1]
  }
  comp$outcomes[is.nan(comp$outcomes[, comp$twoKp2]), comp$twoKp2] <- 0
  comp$outcomes[, comp$twoKp2]    <-
    comp$outcomes[, comp$twoKp2]/sum(comp$outcomes[, comp$twoKp2])
  pos_mat                         <- !neg_mat
  false_discoveries               <-
    Rfast::rowsums(comp$outcomes[, comp$seq_K]*neg_mat)
  false_non_discoveries           <-
    Rfast::rowsums(comp$inv_psi*pos_mat)
  pFDR                            <-
    sum(comp$outcomes[comp$disjunctive, comp$twoKp2]*
          false_discoveries[comp$disjunctive]/
          comp$discoveries[comp$disjunctive])/
    sum(comp$outcomes[comp$disjunctive, comp$twoKp2])
  if (is.nan(pFDR)) {
    pFDR                                          <- 0
  }
  core                            <-
    c(sum(comp$outcomes[comp$disjunctive, comp$twoKp2]),
      sum(comp$outcomes[comp$conjunctive, comp$twoKp2]),
      sapply(comp$seq_K,
             function(k) { sum(comp$outcomes[comp$outcomes[, k] == 1,
                                             comp$twoKp2]) }),
      sapply(comp$seq_K,
             function(k) { sum(comp$outcomes[false_discoveries >= k,
                                             comp$twoKp2]) }),
      sapply(comp$seq_K,
             function(k) { sum(comp$outcomes[false_non_discoveries >= k,
                                             comp$twoKp2]) }),
      sum(comp$outcomes[, comp$twoKp2]*false_discoveries)/comp$K,
      sum(comp$outcomes[, comp$twoKp2]*false_discoveries/comp$mod_discoveries),
      pFDR,
      sum(comp$outcomes[, comp$twoKp2]*
            false_non_discoveries/comp$mod_non_discoveries),
      sum(comp$outcomes[, comp$twoKp2]*
            Rfast::rowsums(comp$outcomes[, comp$seq_K]*pos_mat))/
        max(sum(tau[comp$seq_K] > 0), 1),
      sum(comp$outcomes[, comp$twoKp2]*Rfast::rowsums(comp$inv_psi*neg_mat))/
        max(sum(tau[comp$seq_K] <= 0), 1))
  ess                             <-
    sum(comp$outcomes[, comp$twoKp2]*comp$outcomes[, comp$twoKp1]*comp$n_factor)
  vss                             <-
    max(sum(comp$outcomes[, comp$twoKp2]*(comp$outcomes[, comp$twoKp1]*
                                            comp$n_factor)^2) - ess^2, 0)
  #powers                          <- numeric(comp$nrow_combs)
  #for (i in comp$seq_nrow_combs) {
  #  powers[i]                     <-
  #    sum(comp$outcomes[which(Rfast::rowsums(
  #      comp$outcomes[, 1:comp$combs[i, 2], drop = FALSE]) >= comp$combs[i, 1]),
  #      comp$twoKp2])
  #}
  pmf_N                           <- numeric(comp$len_poss_n)
  for (i in 1:comp$len_poss_n) {
    pmf_N[i]                      <-
      sum(comp$outcomes[which(abs(comp$outcomes[, comp$twoKp1] -
                                    comp$poss_n[i]/comp$n_factor) < 1e-6),
                        comp$twoKp2])
  }
  cum_S                           <- cumsum(pmf_N)
  MeSS                            <-
    ifelse(any(cum_S == 0.5), 0.5*(comp$poss_n[which(cum_S == 0.5)] +
                                     comp$poss_n[which(cum_S == 0.5) + 1]),
           comp$poss_n[which(cum_S > 0.5)[1]])
  MoSS                            <-
    mean(comp$poss_n[which(pmf_N == max(pmf_N))])
  list(opchar = c(core, ess, sqrt(vss), MeSS, MoSS,
                  comp$poss_n[comp$len_poss_n]),
       pmf_N  = pmf_N)
}

root_bounds_gs                     <- function(C, comp) {
  comp     <- bounds_gs(C, comp)
  fwer     <- 0
  for (i in which(comp$thetas_d_HG_a_fwer[, comp$twoKp2] > 0)) {
    int    <- mvtnorm::pmvnorm(comp$bounds[comp$l_indices_HG[[i]]],
                               comp$bounds[comp$u_indices_HG[[i]]],
                               comp$means_HG[[i]],
                               sigma = comp$Lambdas_HG[[i]])[1]
    if (!is.nan(int)) {
      fwer <- fwer + comp$thetas_d_HG_a_fwer[i, comp$twoKp2]*
        mvtnorm::pmvnorm(comp$bounds[comp$l_indices_HG[[i]]],
                         comp$bounds[comp$u_indices_HG[[i]]],
                         comp$means_HG[[i]],
                         sigma = comp$Lambdas_HG[[i]])[1]
    }
  }
  fwer - comp$alpha
}

root_ss_gs                         <- function(ss, comp) {
  power   <- 0
  for (i in which(comp$thetas_bc_LFC_power[, comp$twoKp2] > 0)) {
    power <- power + comp$thetas_bc_LFC_power[i, comp$twoKp2]*
      mvtnorm::pmvnorm(comp$bounds[comp$l_indices_LFC[[i]]],
                       comp$bounds[comp$u_indices_LFC[[i]]],
                       sqrt(ss)*comp$means_factors_LFC[[i]],
                       sigma = comp$Lambdas_LFC[[i]])[1]
  }
  power - (1 - comp$beta)
}

sim_gs_bern_internal               <- function(pi, completed_replicates, n, e,
                                               f, ratio, stopping, type,
                                               replicates, K, J, summary,
                                               total_replicates) {
  summary_i                   <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  seq_J                       <- 1:J
  seq_K                       <- 1:K
  Kp1                         <- K + 1L
  if (type == "variable") {
    N                         <- matrix(c(n, rep(n*ratio, K)), replicates, Kp1)
    nj_base                   <- c(n, rep(n*ratio, K))
    maxN                      <- J*n*(1 + ratio*K)
  } else {
    N                         <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                        replicates, K + 1L)
    maxN                      <- n*J
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
      for (k in which(present[-1])) {
        if (Z_j[k] > e[j]) {
          rej_mat[i, k]       <- 1L
          present[k + 1]      <- FALSE
        } else if (Z_j[k] <= f[j]) {
          present[k + 1]      <- FALSE
        }
      }
      if (all(stopping == "simultaneous", sum(rej_mat[i, ]) > 0)) {
        break
      } else if (all(!present[-1])) {
        break
      }
      if (j < J) {
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        N[i, ]                <- N[i, ] + nj
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
  c(pi, length(which_discoveries)/replicates, sum(discoveries == K)/replicates,
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
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(pi[-1] > pi[1]),
                                              1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(pi[-1] <= pi[1]), 1)*replicates),
    sum(rowsums_N)/replicates, stats::sd(rowsums_N),
    stats::quantile(rowsums_N, 0.5), calculate_mode(rowsums_N), maxN)
}

sim_gs_pois_internal               <- function(lambda, completed_replicates, n,
                                               e, f, ratio, stopping, type,
                                               replicates, K, J, summary,
                                               total_replicates) {
  summary_i                       <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  seq_J                           <- 1:J
  seq_K                           <- 1:K
  Kp1                             <- K + 1L
  if (type == "variable") {
    N                             <- matrix(c(n, rep(n*ratio, K)), replicates,
                                            Kp1)
    nj_base                       <- c(n, rep(n*ratio, K))
    maxN                          <- J*n*(1 + ratio*K)
  } else {
    N                             <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                            replicates, K + 1L)
    maxN                          <- n*J
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
      sigma2                  <- lambda_hat
      Z_j                     <-
        (lambda_hat[-1] - lambda_hat[1])/sqrt(sigma2[1]/N[i, 1] + sigma2[-1]/N[i, -1])
      for (k in which(present[-1])) {
        if (Z_j[k] > e[j]) {
          rej_mat[i, k]       <- 1L
          present[k + 1]      <- FALSE
        } else if (Z_j[k] <= f[j]) {
          present[k + 1]      <- FALSE
        }
      }
      if (all(stopping == "simultaneous", sum(rej_mat[i, ]) > 0)) {
        break
      } else if (all(!present[-1])) {
        break
      }
      if (j < J) {
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        N[i, ]                <- N[i, ] + nj
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
  c(lambda, length(which_discoveries)/replicates, sum(discoveries == K)/replicates,
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
    sum(Rfast::rowsums(rej_mat*pos_mat))/(max(sum(lambda[-1] > lambda[1]),
                                              1)*replicates),
    sum(Rfast::rowsums((rej_mat == 0)*neg_mat))/
      (max(sum(lambda[-1] <= lambda[1]), 1)*replicates),
    sum(rowsums_N)/replicates, stats::sd(rowsums_N),
    stats::quantile(rowsums_N, 0.5), calculate_mode(rowsums_N), maxN)
}

sim_gs_norm_internal               <- function(tau, completed_replicates, n, e,
                                               f, sigma, ratio, stopping, type,
                                               replicates, K, J, summary,
                                               total_replicates) {
  summary_i                   <-
    round(seq(1, total_replicates, length.out = 11)[-c(1, 11)])
  tau                         <- c(0, tau)
  seq_J                       <- 1:J
  seq_K                       <- 1:K
  Kp1                         <- K + 1L
  if (type == "variable") {
    N                         <- matrix(c(n, rep(n*ratio, K)), replicates, Kp1)
    nj_base                   <- c(n, rep(n*ratio, K))
    maxN                      <- J*n*(1 + ratio*K)
  } else {
    N                         <- matrix(c(n, rep(n*ratio, K))/(1 + ratio*K),
                                        replicates, K + 1L)
    maxN                      <- n*J
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
      for (k in which(present[-1])) {
        if (Z_j[k] > e[j]) {
          rej_mat[i, k]       <- 1L
          present[k + 1]      <- FALSE
        } else if (Z_j[k] <= f[j]) {
          present[k + 1]      <- FALSE
        }
      }
      if (all(stopping == "simultaneous", sum(rej_mat[i, ]) > 0)) {
        break
      } else if (all(!present[-1])) {
        break
      }
      if (j < J) {
        if (type == "variable") {
          nj                  <- nj_base
          nj[which(!present)] <- 0
        } else {
          nj                  <-
            c(n, rep(n*ratio, K))/(1 + ratio*sum(present[-1]))
          nj[which(!present)] <- 0
        }
        N[i, ]                <- N[i, ] + nj
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
