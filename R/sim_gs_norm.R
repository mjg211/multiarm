sim_gs_norm <- function(des = des_gs_norm(integer = T), tau, replicates = 1e5,
                        summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_norm(des, T)
  tau        <- check_tau(tau, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_sim_gs(des, tau, replicates, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations..")
  }
  if (length(des$sigma) == 1) {
    sigma <- rep(des$sigma, des$K + 1)
  } else {
    sigma <- c(des$sigma[1], rep(des$sigma[2], des$K))
  }
  nrow_tau         <- nrow(tau)
  total_replicates <- nrow_tau*replicates
  sim              <- matrix(0, nrow_tau, 4*des$K + 12)
  for (i in 1:nrow_tau) {
    sim[i, ]       <-
      sim_gs_norm_internal(tau[i, ], (i - 1)*replicates, des$n_factor, des$e,
                           des$f, sigma, des$ratio, des$stopping, des$type,
                           replicates, des$K, des$J, summary,
                           total_replicates)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste0("tau", seq_K), "Pdis", "Pcon",
                        paste0("P", seq_K), paste0("FWERI", seq_K),
                        paste0("FWERII", seq_K), "PHER", "FDR", "pFDR", "FNDR",
                        "Sens", "Spec", "ESS", "SDSS", "MSS", "maxN")
  sim              <- tibble::as_tibble(sim)
  if (summary) {
    message("..completed the required simulations.")
    message("  Preparing for outputting..")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  list(des        = des,
       replicates = replicates,
       sim        = sim,
       summary    = summary,
       tau        = tau)

}
