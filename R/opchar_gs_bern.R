#' @export
opchar_gs_bern <- function(des = des_gs_bern(), pi, summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_bern(des)
  pi <- check_pi(pi, des)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_opchar_gs_bern(des, pi, "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required calculations..")
  }
  comp <- components_gs_init(des$alpha, des$beta, des$delta0, des$delta1,
                             des$efix, des$eshape, des$ffix, des$fshape,
                             des$integer, des$J, des$K, des$power, des$ratio,
                             des$stopping, des$summary, des$type,
                             pi0 = des$pi0, n_factor = des$n_factor, f = des$f,
                             e = des$e)
  comp <- components_gs_update(comp, pi = pi)
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
       pi      = pi,
       pmf_N   = comp$pmf_N,
       summary = summary)

}
