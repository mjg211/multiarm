#' @export
opchar_gs_norm <- function(des = des_gs_norm(), tau, summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_norm(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_gs_norm(des, tau, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_gs_init(des$alpha, des$beta, des$delta0, des$delta1,
                             des$efix, des$eshape, des$ffix, des$fshape,
                             des$integer, des$J, des$K, des$power, des$ratio,
                             des$stopping, des$summary, des$type, des$sigma,
                             n_factor = des$n_factor, f = des$f, e = des$e)
  comp <- components_gs_update(comp, tau)
  comp <- opchar_gs_internal(comp)
  if (summary) {
    message("..completed the required calculations.")
    message("  Preparing for outputting..")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  list(des     = des,
       opchar  = comp$opchar,
       pmf_N   = comp$pmf_N,
       summary = summary,
       tau     = tau)

}
