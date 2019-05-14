#' Empirically determine the operating characteristics of a fixed-sample
#' multi-arm clinical trial for a Bernoulli distributed primary outcome
#'
#' \code{sim_ma_bern()} determines the operating characteristics of a specified
#' fixed-sample multi-arm clinical trial design assuming the primary outcome is
#' Bernoulli distributed, for given values of the true treatment effects, using
#' simulation.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}, as
#' returned by \code{\link{build_ma_bern}} or \code{\link{des_ma_bern}} (i.e., a
#' fixed-sample multi-arm clinical trial design for a Bernoulli distributed
#' outcome). \strong{Note:} The sample sizes in all arms must be whole numbers.
#' Defaults to \code{des_ma_bern(integer = T)}.
#' @param pi A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations if
#' unspecified.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}}. Defaults to
#' \code{100000}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$sim} giving the estimated
#' operating characteristics.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' \dontrun{
#' # The estimated operating characteristics for the default parameters
#' sim   <- sim_ma_bern()
#' # An A-optimal design, specifying tau explicitly
#' des_A <- des_ma_bern(ratio = "A", integer = T)
#' sim_A <- sim_ma_bern(des_A, rbind(c(0, 0),
#'                                   c(0.5, 0.5),
#'                                   c(0.5, 0),
#'                                   c(0, 0.5)))
#' }
#' @seealso \code{\link{an_ma_bern}}, \code{\link{build_ma_bern}},
#' \code{\link{des_ma_bern}}, \code{\link{opchar_ma_bern}},
#' \code{\link{plot.multiarm_des_ma_bern}}.
#' @export
sim_ma_bern <- function(des = des_ma_bern(integer = T), pi,
                        replicates = 100000, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma_bern(des, T)
  pi        <- check_pi(pi, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_sim_ma(des, pi, replicates, "bernoulli")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations", uc("two_elip"))
  }
  nrow_pi          <- nrow(pi)
  total_replicates <- nrow_pi*replicates
  sim              <- matrix(0, nrow_pi, 4*des$K + 9L)
  for (i in 1:nrow_pi) {
    sim[i, ]       <-
      sim_ma_bern_internal(pi[i, ], des$n, des$alpha, des$correction,
                           replicates, des$gamma, des$gammaO,
                           (i - 1)*replicates, total_replicates, summary)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste("pi", c(0, seq_K), sep = ""), "Pdis", "Pcon",
                        paste("P", seq_K, sep = ""),
                        paste("FWERI", seq_K, sep = ""),
                        paste("FWERII", seq_K, sep = ""),
                        "PHER", "FDR", "pFDR", "FNDR", "Sens", "Spec")
  sim              <- tibble::as_tibble(sim)
  if (summary) {
    message(uc("two_elip"), "completed the required simulations.")
    message("  Preparing for outputting", uc("two_elip"))
  }

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  list(des        = des,
       pi         = pi,
       replicates = replicates,
       sim        = sim,
       summary    = summary)

}
