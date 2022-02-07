# Calculates the mode of a vector. When there are multiple modes, the mean of
# these values is returned
calculate_mode <- function(x) {
  unique_x <- unique(na.omit(x))
  mean(unique_x[which(tabulate(match(x, unique_x)) ==
                       max(tabulate(match(x, unique_x))))])
}

# Computes all unique permutations of the elements of a vector x
permutations   <- function(x) {
  n                  <- length(x)
  out                <- vector("list", gamma(n + 1))
  p                  <- ip <- seqn <- 1:n
  d                  <- rep(-1, n)
  d[1]               <- 0
  m                  <- n + 1
  p                  <- c(m, p, m)
  i                  <- 1
  use                <- -c(1, n + 2)
  while (m != 1) {
    out[[i]]         <- x[p[use]]
    i                <- i + 1
    m                <- n
    chk              <- (p[ip + d + 1] > seqn)
    m                <- max(seqn[!chk])
    if (m < n) {
      d[(m + 1):n]   <- -d[(m + 1):n]
    }
    index1           <- ip[m] + 1
    index2           <- p[index1] <- p[index1 + d[m]]
    p[index1 + d[m]] <- m
    tmp              <- ip[index2]
    ip[index2]       <- ip[m]
    ip[m]            <- tmp
  }
  out                <- matrix(unlist(out), length(out), n, byrow = TRUE)
  out[!duplicated(out), , drop = FALSE]
}
