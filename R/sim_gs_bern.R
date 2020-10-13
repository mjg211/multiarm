sim_gs_bern <- function(des = des_gs_bern(integer = T), pi, replicates = 1e5,
                        summary = F) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_bern(des, T)
  pi         <- check_pi(pi, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_sim_gs(des, pi, replicates, "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations..")
  }
  nrow_pi          <- nrow(pi)
  total_replicates <- nrow_pi*replicates
  sim              <- matrix(0, nrow_pi, 4*des$K + 13)
  for (i in 1:nrow_pi) {
    sim[i, ]       <-
      sim_gs_bern_internal(pi[i, ], (i - 1)*replicates, des$n_factor, des$e,
                           des$f, des$ratio, des$stopping, des$type, replicates,
                           des$K, des$J, summary, total_replicates)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste0("pi", c(0, seq_K)), "Pdis", "Pcon",
                        paste0("P", seq_K), paste0("FWERI", seq_K),
                        paste0("FWERII", seq_K), "PHER", "FDR", "pFDR", "FNDR",
                        "Sens", "Spec", "ESS", "SDSS", "MSS", "maxN")
  sim              <- tibble::as_tibble(sim, .name_repair = "minimal")
  if (summary) {
    message("..completed the required simulations.")
    message("  Preparing for outputting..")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  list(des        = des,
       pi         = pi,
       replicates = replicates,
       sim        = sim,
       summary    = summary)

}
