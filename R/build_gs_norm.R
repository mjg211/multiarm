#' @export
build_gs_norm <- function(n1 = 162, n10 = 54, e = c(2.42, 2.42),
                          f = c(-2.42, 2.42), K = 2, alpha = 0.025, beta = 0.1,
                          delta1 = 0.5, delta0 = 0, sigma = 1, ratio = 1,
                          power = "marginal", stopping = "simultaneous",
                          type = "variable", summary = F) {

  ##### Check input variables ##################################################

  #check_n1_n10(n1, n10, "n1", "n10", int = F)
  #check_ef(e, f, "e", "f")
  J     <- length(e)
  K     <- check_integer_range(K, "K", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  sigma <- check_sigma(sigma, name_sigma = "sigma", des = "mams")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_belong(stopping, "stopping", c("simultaneous", "separate"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_build_gs_norm(n1, n10, e, f, K, alpha, beta, delta1, delta0, sigma,
    #                      ratio, power, stopping, type)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message(" Building outputs..")
  }
  integer    <- all(c(n1, n10)%%1 == 0)
  if (type == "variable") {
    n_factor <- n10
  } else {
    n_factor <- n1
  }
  comp       <- components_gs_init(alpha, beta, delta0, delta1, NA, NA, NA, NA,
                                   integer, J, K, power, ratio, stopping,
                                   summary, type, sigma, n_factor = n_factor,
                                   f = f, e = e)
  tau        <- rbind(numeric(K), rep(delta1, K),
                      matrix(delta0, K, K) + (delta1 - delta0)*diag(K))
  comp       <- components_gs_update(comp, tau)
  comp       <- opchar_gs_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = comp$e,
                        efix       = NA,
                        eshape     = NA,
                        f          = comp$f,
                        ffix       = NA,
                        fshape     = NA,
                        integer    = integer,
                        J          = J,
                        K          = K,
                        n_factor   = comp$n_factor,
                        n10        = n10,
                        n1         = n1,
                        opchar     = comp$opchar,
                        pmf_N      = comp$pmf_N,
                        power      = power,
                        ratio      = ratio,
                        sigma      = sigma,
                        stopping   = stopping,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_gs_norm", class(output))
  output

}
