#' Empirically determine the operating characteristics of a fixed-sample
#' multi-arm clinical trial
#'
#' \code{sim_ma()} determines the operating characteristics of a specified
#' fixed-sample multi-arm clinical trial design, for given values of the true
#' treatment effects, using simulation.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma"}, as
#' returned by \code{\link{build_ma}}, \code{\link{des_ma}}, or
#' \code{\link{des_int_ma}} (i.e., a fixed-sample multi-arm clinical trial
#' design). \strong{Note:} The sample sizes in all arms must be whole numbers.
#' Defaults to \code{des_ma(integer = T)}.
#' @param tau A \code{\link{matrix}} whose rows indicate values of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}} at which to
#' evaluate the operating characteristics. Defaults internally to the global
#' null, global alternative, and each of the least favourable configurations if
#' unspecified.
#' @param replicates A \code{\link{numeric}} indicating the number of replicate
#' simulations to use for each value of
#' \ifelse{html}{\out{<b><i>&tau;</i></b>}}{\eqn{\bold{\tau}}}. Defaults to
#' \code{100000}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_sim_ma"} containing the
#' following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$sim} giving the estimated
#' operating characteristics.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' \dontrun{
#' # The estimated operating characteristics for the default parameters
#' sim   <- sim_ma()
#' # An A-optimal design, specifying tau explicitly
#' des_A <- des_int_ma(ratio = "A")
#' sim_A <- sim_ma(des_A, rbind(c(0, 0),
#'                              c(0.5, 0.5),
#'                              c(0.5, 0),
#'                              c(0, 0.5)))
#' }
#' @seealso \code{\link{an_ma}}, \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}.
#' @export
sim_ma <- function(des = des_ma(integer = T), tau, replicates = 100000,
                   summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma(des, T)
  tau        <- check_tau(tau, des)
  replicates <- as.numeric(check_integer_range(replicates, "replicates",
                                               c(0, Inf), 1))
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_sim_ma(des, tau, replicates)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the required simulations", uc("two_elip"))
  }
  nrow_tau         <- nrow(tau)
  total_replicates <- nrow_tau*replicates
  sim              <- matrix(0, nrow_tau, 2*des$K + 4L)
  for (i in 1:nrow_tau) {
    sim[i, ]       <-
      sim_ma_internal(tau[i, ], des$n, des$sigma, des$correction, des$sigma,
                      F, F, replicates, des$pi, des$piO, (i - 1)*replicates,
                      total_replicates, summary)
  }
  seq_K            <- 1:des$K
  colnames(sim)    <- c(paste("tau", seq_K, sep = ""), "Pdis", "Pcon",
                        paste("P", seq_K, sep = ""), "FWER", "FDR")
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
       replicates = replicates,
       sim        = sim,
       summary    = summary,
       tau        = tau)

}
