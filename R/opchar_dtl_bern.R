#' @export
opchar_dtl_bern <- function(des = des_dtl_bern(), pi, summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_bern(des)
  pi <- check_pi(pi, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_dtl(des, pi, "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_dtl_init(des$alpha, des$beta, des$delta0, des$delta1,
                              des$integer, des$Kv, des$power, des$ratio,
                              des$summary, des$type, pi0 = des$pi0,
                              n_factor = des$n_factor, e = des$e)
  comp <- components_dtl_update(comp, pi = pi)
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
       pi      = pi,
       summary = summary)

}
