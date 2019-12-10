#' @export
des_opt_gen_mams <- function(K = 3, J = 2, a = 1, b = 1, c = 1, d = 1,
                             alpha = 0.05, beta = 0.10, sigma = 1, ratio = 1,
                             delta1 = 0.5, delta0 = 0, w = rep(1/3, 3),
                             parallel = F, popSize = 100, maxiter = 5000,
                             run = maxiter, integer = F, seed = Sys.time(),
                             summary = T) {

  set.seed(seed)

  ##### Check input variables ##################################################

  if (length(sigma) == 1) {
    sigma <- c(sigma, sigma)
  }

  ##### Print summary ##########################################################

  if (summary) {
    message("")
  }

  ##### Perform main computations ##############################################

  components    <- components_HG_LFC_mams(K, 1, a, b, c, d, ratio, sigma,
                                          delta1, delta0, "variable")
  e             <- stats::uniroot(root_bound_gen_ma, c(-100, 100),
                                  K          = K,
                                  alpha      = alpha,
                                  components = components)$root
  n             <- stats::uniroot(root_ss_gen_ma, c(0, 10^6),
                                  K          = K,
                                  beta       = beta,
                                  components = components,
                                  e          = e)$root
  components    <- components_HG_LFC_mams(K, J, a, b, c, d, ratio, sigma,
                                          delta1, delta0, "variable", w)
  fitness       <- function(...) {
    -obj_fn_gen_mams(...)
  }
  start_time    <- Sys.time()
  GA            <- GA::ga(type                = "real-valued",
                          fitness             = fitness,
                          alpha               = alpha,
                          beta                = beta,
                          r                   = ratio,
                          w                   = w,
                          N_fixed             = n*(ratio*K + 1),
                          max_N_factor        = J*(ratio*K + 1),
                          components          = components,
                          lower               = c(0, rep(-20, J),
                                                  rep(0, J - 1)),
                          upper               = c(n, rep(20, 2*J - 1)),
                          popSize             = popSize,
                          maxiter             = ceiling(maxiter),
                          run                 = run,
                          parallel            = parallel)
  end_time      <- Sys.time()
  ss_factor     <- GA@solution[1]
  if (integer) {
    ss_factor   <- as.integer(ceiling(ss_factor))
    while (ss_factor%%ratio != 0) {
      ss_factor <- ss_factor + 1L
    }
  }
  e             <- c(GA@solution[2:J] + GA@solution[(J + 2):(2*J)],
                     GA@solution[J + 1])
  f             <- GA@solution[2:(J + 1)]
  nj0           <- ss_factor
  tau           <- rbind(rep(0, K), rep(delta1, K),
                         matrix(delta0, K, K) + (delta1 - delta0)*diag(K))
  opchar        <- opchar_mams_internal(tau, K, J, a, b, c, d, e, f, ratio,
                                        sigma, "variable", ss_factor)

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  output        <- list(a          = a,
                        alpha      = alpha,
                        b          = b,
                        beta       = beta,
                        c          = c,
                        d          = d,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = e,
                        f          = f,
                        GA         = GA,
                        integer    = integer,
                        J          = J,
                        K          = K,
                        maxiter    = maxiter,
                        nj0        = nj0,
                        O          = -GA@fitnessValue,
                        opchar     = opchar,
                        parallel   = parallel,
                        popSize    = popSize,
                        ratio      = ratio,
                        run        = run,
                        seed       = seed,
                        sigma      = sigma,
                        summary    = summary,
                        w          = w)
  class(output) <- c(class(output), "multiarm_des_mams")
  output

}
