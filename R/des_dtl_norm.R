#' @export
des_dtl_norm <- function(Kv = c(2, 1), alpha = 0.025, beta = 0.1, delta1 = 0.5,
                         delta0 = 0, sigma = 1, ratio = 1, power = "marginal",
                         type = "variable", integer = F, summary = F) {

  ##### Check input variables ##################################################

  Kv     <- check_Kv(Kv, "Kv")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  sigma <- check_sigma(sigma, name_sigma = "sigma", des = "dtl")
  check_ratio(ratio, name_ratio = "ratio", des = "dtl")
  check_belong(power, "power", c("disjunctive", "marginal"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  comp <- components_dtl_init(alpha, beta, delta0, delta1, integer, Kv, power,
                              ratio, summary, type, sigma)
  if (summary) {
    #summary_des_dtl_norm(comp)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Building required design components..")
  }
  comp          <- components_dtl_hg_lfc(comp)
  if (summary) {
    message("..built required design components.")
    message("  Identifying the stopping boundaries..")
  }
  comp$e        <- stats::uniroot(f        = root_bounds_dtl,
                                  interval = c(-5, 5),
                                  comp     = comp)$root
  comp          <- components_dtl_update_bounds(comp$e, comp)
  N             <- des_ss_norm(Kv[1], alpha, beta, delta1, delta0,
                               c(sigma[1], rep(sigma[2], Kv[1])),
                               rep(ratio, Kv[1]), "dunnett", "marginal", F, F)$N
  comp$n_factor <- stats::uniroot(f        = root_ss_dtl,
                                  interval = c(1e-16, N),
                                  comp     = comp)$root
  if (summary) {
    message("..identified the required sample size.")
    message("  Preparing for outputting..")
  }
  comp          <- integer_dtl(comp)
  if (type == "variable") {
    n10         <- comp$n_factor
    n1          <- comp$n_factor*(1 + ratio*Kv[1])
  } else if (type == "fixed") {
    n10         <- comp$n_factor/(1 + ratio*Kv[1])
    n1          <- comp$n_factor
  }
  comp          <-
    components_dtl_update(comp, rbind(rep(0, Kv[1]), rep(delta1, Kv[1]),
                                       matrix(delta0, Kv[1], Kv[1]) +
                                         (delta1 - delta0)*diag(Kv[1])))
  comp          <- opchar_dtl_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = comp$e,
                        integer    = integer,
                        Kv         = Kv,
                        maxN       = comp$opchar$maxN[1],
                        n_factor   = comp$n_factor,
                        n1         = n1,
                        n10        = n10,
                        opchar     = comp$opchar,
                        power      = power,
                        ratio      = ratio,
                        sigma      = sigma,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_dtl_norm", class(output))
  output

}
