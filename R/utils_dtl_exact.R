Kv                      <- c(3, 2)
pi                      <- c(0.1, 0.2, 0.3, 0.4)
n_j                     <- c(5, 6, 6, 6)
poss_responses_j        <- list()
for (k in (Kv + 1)) {
  poss_responses_j[[k]] <-
    iterpc::getall(iterpc::iterpc(n       = max(n_j) + 1,
                                  r       = k,
                                  labels  = 0:max(n_j),
                                  ordered = TRUE,
                                  replace = TRUE))
  poss_responses_j[[k]] <- poss_responses_j[[k]][poss_responses_j[[k]][, 1] <= n_j[1], ]
  for (l in 2:k) {
    poss_responses_j[[k]] <- poss_responses_j[[k]][poss_responses_j[[k]][, l] <= n_j[2], ]
  }
}

ordering                                      <-
  iterpc::getall(iterpc::iterpc(n = Kv[1], r = Kv[1],
                                labels  = 1:Kv[1], ordered = TRUE))
nrow_ordering                                 <- nrow(ordering)
nrow_outcomes                                 <-
  nrow_ordering*(Kv[2] + 1)
outcomes                                      <- matrix(0L, nrow_outcomes,
                                                        2*Kv[1])
for (i in 1:(Kv[2] + 1)) {
  fact                                        <- (i - 1)*nrow_ordering
  outcomes[(1 + fact):(fact + nrow_ordering), 1:Kv[1]]                     <-
    ordering
  for (j in 1:nrow_ordering) {
    outcomes[fact + j, Kv[1] + order(ordering[j, ], decreasing = TRUE)]   <-
      c(integer(Kv[1] + 1 - i), rep(1L, i - 1))
  }
}

dbinom_grid <- matrix(0, max(n_j) + 1, Kv[1] + 1)
for (k in 1:(Kv[1] + 1)) {
  dbinom_grid[1:(n_j[k] + 1), k] <- stats::dbinom(0:n_j[k], n_j[k], pi[k])
}
prob_stage_1      <- dbinom_grid[poss_responses_j[[Kv[1] + 1]][, 1] + 1, 1]
for (k in 2:(Kv[1] + 1)) {
  prob_stage_1    <- prob_stage_1*dbinom_grid[poss_responses_j[[Kv[1] + 1]][, k] + 1, k]
}
prob_stage_2_base <- dbinom_grid[poss_responses_j[[Kv[2] + 1]][, 1] + 1, 1]

T1                                     <-
  poss_responses_j[[Kv[1] + 1]][, -1] - poss_responses_j[[Kv[1] + 1]][, 1]
T1_array                               <- array(0, dim = n_j[-1] + n_j[1] + 1)
for (i in 1:nrow(T1)) {
  T1_array[t(T1[i, ]) +  + n_j[1] + 1] <-
    T1_array[t(T1[i, ]) + n_j[1] + 1] + prob_stage_1[i]
}


T2_tilde <- poss_responses_j[[Kv[2] + 1]][, -1, drop = FALSE] - poss_responses_j[[Kv[2] + 1]][, 1]


continued <-

# Summarise possible stagewise outcomes first in to smaller array
# Do want to do it by outcome for consistency with rest of the package...
prob_outcomes  <- numeric(nrow(outcomes))
for (i in 1:nrow(outcomes)) {
  drop_stage_1 <- which(outcomes[i, ] %in% (Kv[2] + 1):Kv[1])
  for (j in 1:nrow(poss_responses_j[[Kv[1] + 1]])) {

  }
}



