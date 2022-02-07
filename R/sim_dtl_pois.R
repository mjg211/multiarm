#' Empirically determine the operating characteristics of a multi-stage
#' drop-the-losers multi-arm clinical trial for a Poisson distributed primary
#' outcome
#'
#' \code{sim_dtl_pois()} determines the operating characteristics of a specified
#' multi-stage drop-the-losers multi-arm clinical trial design assuming the
#' primary outcome is Poisson distributed, for given values of the true
#' treatment effects, using simulation.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_dtl_pois"}, as
#' returned by \code{\link{build_dtl_pois}} or \code{\link{des_dtl_pois}} (i.e.,
#' a multi-stage drop-the-losers multi-arm clinical trial design for a Poisson
#' distributed outcome). \strong{Note:} The sample sizes in all arms must be
#' whole numbers. Defaults to \code{des_dtl_pois(integer = TRUE)}.
#' @param lambda A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&lambda;</i></b>}}{\eqn{\bold{\lambda}}} at which to
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
#' sim   <- sim_dtl_pois()
#' }
#' @seealso \code{\link{build_dtl_pois}}, \code{\link{des_dtl_pois}},
#' \code{\link{opchar_dtl_pois}}, \code{\link{plot.multiarm_des_dtl_pois}}.
#' @export
sim_dtl_pois <- function(des = des_dtl_pois(integer = TRUE), lambda,
                         replicates = 1e5, summary = FALSE) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_pois(des, T)
  lambda         <- check_lambda(lambda, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_sim_dtl(des, lambda, replicates, "pois")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations..")
  }
  nrow_lambda          <- nrow(lambda)
  total_replicates <- nrow_lambda*replicates
  sim              <- matrix(0, nrow_lambda, 4*des$Kv[1] + 14)
  for (i in 1:nrow_lambda) {
    sim[i, ]       <-
      sim_dtl_pois_internal(lambda[i, ], (i - 1)*replicates, des$n_factor, des$e,
                            des$Kv, des$ratio, des$type, des$spacing,
                            replicates, summary, total_replicates)
  }
  seq_K            <- 1:des$Kv[1]
  colnames(sim)    <- c(paste0("lambda", c(0, seq_K)), "Pdis", "Pcon",
                        paste0("P", seq_K), paste0("FWERI", seq_K),
                        paste0("FWERII", seq_K), "PHER", "FDR", "pFDR", "FNDR",
                        "Sens", "Spec", "ESS", "SDSS", "MeSS", "MoSS", "maxN")
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
       lambda         = lambda,
       replicates = replicates,
       sim        = sim,
       summary    = summary)

}
