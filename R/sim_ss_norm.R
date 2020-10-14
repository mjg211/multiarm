#' Empirically determine the operating characteristics of a single-stage
#' multi-arm clinical trial for a normally distributed primary outcome
#'
#' \code{sim_ss_norm()} determines the operating characteristics of a specified
#' single-stage multi-arm clinical trial design assuming the primary outcome is
#' normally distributed, for given values of the true treatment effects, using
#' simulation.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ss_norm"}, as
#' returned by \code{\link{build_ss_norm}} or \code{\link{des_ss_norm}} (i.e., a
#' single-stage multi-arm clinical trial design for a normally distributed
#' outcome). \strong{Note:} The sample sizes in all arms must be whole numbers.
#' Defaults to \code{des_ss_norm(integer = TRUE)}.
#' @param tau A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations if
#' unspecified.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}}. Defaults to
#' \code{1e5}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$sim} giving the estimated
#' operating characteristics.
#' \item Each of the input variables.
#' }
#' @examples
#' \dontrun{
#' # The estimated operating characteristics for the default parameters
#' sim   <- sim_ss_norm()
#' # An A-optimal design, specifying tau explicitly
#' des_A <- des_ss_norm(ratio = "A", integer = T)
#' sim_A <- sim_ss_norm(des_A, rbind(c(0, 0),
#'                                   c(0.5, 0.5),
#'                                   c(0.5, 0),
#'                                   c(0, 0.5)))
#' }
#' @seealso \code{\link{build_ss_norm}}, \code{\link{des_ss_norm}},
#' \code{\link{opchar_ss_norm}}, \code{\link{plot.multiarm_des_ss_norm}}.
#' @export
sim_ss_norm <- function(des = des_ss_norm(integer = TRUE), tau,
                        replicates = 1e5, summary = FALSE) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_norm(des, T)
  tau        <- check_tau(tau, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_sim_ss(des, tau, replicates, "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations..")
  }
  nrow_tau         <- nrow(tau)
  total_replicates <- nrow_tau*replicates
  sim              <- matrix(0, nrow_tau, 4*des$K + 8)
  for (i in 1:nrow_tau) {
    sim[i, ]       <-
      sim_ss_norm_internal(tau[i, ], (i - 1)*replicates, des$correction,
                           des$gamma, des$gammaO, des$n, F, replicates,
                           des$sigma, des$sigma, summary, F, total_replicates)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste0("tau", seq_K), "Pdis", "Pcon",
                        paste0("P", seq_K), paste0("FWERI", seq_K),
                        paste0("FWERII", seq_K), "PHER", "FDR", "pFDR", "FNDR",
                        "Sens", "Spec")
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
       replicates = replicates,
       sim        = sim,
       summary    = summary,
       tau        = tau)

}
