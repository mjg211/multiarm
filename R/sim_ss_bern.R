#' Empirically determine the operating characteristics of a single-stage
#' multi-arm clinical trial for a Bernoulli distributed primary outcome
#'
#' \code{sim_ss_bern()} determines the operating characteristics of a specified
#' single-stage multi-arm clinical trial design assuming the primary outcome is
#' Bernoulli distributed, for given values of the true treatment effects, using
#' simulation.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ss_bern"}, as
#' returned by \code{\link{build_ss_bern}} or \code{\link{des_ss_bern}} (i.e., a
#' single-stage multi-arm clinical trial design for a Bernoulli distributed
#' outcome). \strong{Note:} The sample sizes in all arms must be whole numbers.
#' Defaults to \code{des_ss_bern(integer = TRUE)}.
#' @param pi A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations if
#' unspecified.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}}. Defaults to
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
#' sim   <- sim_ss_bern()
#' # An A-optimal design, specifying tau explicitly
#' des_A <- des_ss_bern(ratio = "A", integer = T)
#' sim_A <- sim_ss_bern(des_A, rbind(c(0, 0),
#'                                   c(0.5, 0.5),
#'                                   c(0.5, 0),
#'                                   c(0, 0.5)))
#' }
#' @seealso \code{\link{build_ss_bern}}, \code{\link{des_ss_bern}},
#' \code{\link{opchar_ss_bern}}, \code{\link{plot.multiarm_des_ss_bern}}.
#' @export
sim_ss_bern <- function(des = des_ss_bern(integer = TRUE), pi, replicates = 1e5,
                        summary = FALSE) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_bern(des, T)
  pi         <- check_pi(pi, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_sim_ss(des, pi, replicates, "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations..")
  }
  nrow_pi          <- nrow(pi)
  total_replicates <- nrow_pi*replicates
  sim              <- matrix(0, nrow_pi, 4*des$K + 9)
  for (i in 1:nrow_pi) {
    sim[i, ]       <-
      sim_ss_bern_internal(pi[i, ], des$alpha, (i - 1)*replicates,
                           des$correction, des$gamma, des$gammaO, des$n,
                           replicates, summary, total_replicates)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste0("pi", c(0, seq_K)), "Pdis", "Pcon",
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
       pi         = pi,
       replicates = replicates,
       sim        = sim,
       summary    = summary)

}
