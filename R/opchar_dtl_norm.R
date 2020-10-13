#' @export
opchar_dtl_norm <- function(des = des_dtl_norm(), tau, summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_norm(des)
  tau <- check_tau(tau, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_dtl(des, tau, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_dtl_init(des$alpha, des$beta, des$delta0, des$delta1,
                              des$integer, des$Kv, des$power, des$ratio,
                              des$summary, des$type, des$sigma,
                              n_factor = des$n_factor, e = des$e)
  comp <- components_dtl_update(comp, tau)
  comp <- opchar_dtl_internal(comp)
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
       summary = summary,
       tau     = tau)

}
