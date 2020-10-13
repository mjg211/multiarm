#' @export
build_dtl_bern <- function(n1 = 210, n10 = 70, e = 2.17, Kv = c(2, 1),
                           alpha = 0.025, beta = 0.1, pi0 = 0.3, delta1 = 0.2,
                           delta0 = 0, ratio = 1, power = "marginal",
                           type = "variable", summary = F) {

  ##### Check input variables ##################################################

  #check_n1_n10(n1, n10, "n1", "n10", int = F)
  check_real_range_strict(e, "e", c(-Inf, Inf), 1)
  Kv    <- check_Kv(Kv, "Kv")
  J     <- length(Kv)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("disjunctive", "marginal"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_build_dtl_bern(n1, n10, e, Kv, alpha, beta, pi0, delta1, delta0,
    #                       ratio, power, stopping, type)
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
  comp       <- components_dtl_init(alpha, beta, delta0, delta1, integer, Kv,
                                    power, ratio, summary, type, pi0 = pi0,
                                    n_factor = n_factor, e = e)
  tau        <- rbind(numeric(Kv[1]), rep(delta1, Kv[1]),
                      matrix(delta0, Kv[1], Kv[1]) +
                        (delta1 - delta0)*diag(Kv[1]))
  comp       <- components_dtl_update(comp, tau, cbind(pi0, tau + pi0))
  comp       <- opchar_dtl_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = e,
                        integer    = integer,
                        Kv         = Kv,
                        maxN       = opchar$maxN[1],
                        n_factor   = n_factor,
                        n1         = n1,
                        n10        = n10,
                        opchar     = comp$opchar,
                        pi0        = pi0,
                        power      = power,
                        ratio      = ratio,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_dtl_bern", class(output))
  output

}
