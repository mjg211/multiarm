#' @export
des_gs_norm <- function(K = 2, J = 2, alpha = 0.025, beta = 0.1, delta1 = 0.5,
                        delta0 = 0, sigma = 1, ratio = 1, power = "marginal",
                        stopping = "simultaneous", type = "variable",
                        fshape = "pocock", eshape = "pocock", ffix = -3,
                        efix = 3, integer = F, summary = F) {

  ##### Check input variables ##################################################

  K     <- check_integer_range(K, "K", c(1, Inf), 1)
  J     <- check_integer_range(J, "J", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  sigma <- check_sigma(sigma, name_sigma = "sigma", des = "mams")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_belong(stopping, "stopping", c("simultaneous", "separate"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_boundaries(fshape, eshape, ffix, efix, "fshape", "eshape", "ffix",
                   "efix")
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  comp <- components_gs_init(alpha, beta, delta0, delta1, efix, eshape, ffix,
                             fshape, integer, J, K, power, ratio, stopping,
                             summary, type, sigma)
  if (summary) {
    #summary_des_gs_norm(comp)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Building required design components..")
  }
  comp          <- components_gs_hg_lfc(comp)
  if (summary) {
    message("..built required design components.")
    message("  Identifying the stopping boundaries..")
  }
  C             <- stats::uniroot(f        = root_bounds_gs,
                                  interval = c(1e-16, 1e2),
                                  comp     = comp)$root
  comp          <- bounds_gs(C, comp)
  if (summary) {
    message("..identified the stopping boundaries.")
    message("  Identifying the required sample size..")
  }
  comp$n_factor <- stats::uniroot(f        = root_ss_gs,
                                  interval = c(1e-16, 1000),
                                  comp     = comp)$root
  if (summary) {
    message("..identified the required sample size.")
    message("  Preparing for outputting..")
  }
  comp          <- integer_gs(comp)
  if (type == "variable") {
    n10         <- comp$n_factor
    n1          <- comp$n_factor*(1 + ratio*K)
  } else if (type == "fixed") {
    n10         <- comp$n_factor/(1 + ratio*K)
    n1          <- comp$n_factor
  }
  comp          <-
    components_gs_update(comp, rbind(rep(0, K), rep(delta1, K),
                                       matrix(delta0, K, K) +
                                         (delta1 - delta0)*diag(K)))
  comp          <- opchar_gs_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = comp$e,
                        efix       = efix,
                        eshape     = eshape,
                        f          = comp$f,
                        ffix       = ffix,
                        fshape     = fshape,
                        integer    = integer,
                        J          = J,
                        K          = K,
                        maxN       = comp$opchar$maxN[1],
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
