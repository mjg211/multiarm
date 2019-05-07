#' Design an optimal fixed-sample multi-arm clinical trial
#'
#' \code{des_int_ma()} determines \emph{A}-, \emph{D}-, and \emph{E}-optimal
#' allocation ratios for fixed-sample multi-arm clinical trial designs.
#'
#' @param K A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}, the number of experimental treatment
#' arms. Defaults to 2.
#' @param N A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total sample size across
#' all arms. Defaults to 183.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level.
#' Defaults to 0.05.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to 0.2.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to 0.5.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to 0.
#' @param sigma A \code{\link{numeric}} vector indicating the chosen value for
#' \ifelse{html}{\out{<b><i>&sigma;</i></b>}}{\eqn{\bold{\sigma}}}, the vector
#' of the standard deviations of the responses in each arm. Defaults to
#' \code{rep(1, K + 1)}.
#' @param ratio A \code{\link{character}} string indicating the criteria to use
#' in determining the optimal value of
#' \ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{\r}}}, the vector of
#' allocation ratios to the experimental arms. Can be any of \code{"A"},
#' \code{"D"}, or \code{"E"} to use the \emph{A}-, \emph{D}-, or
#' \emph{E}-optimality criteria. Defaults to \code{"D"}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"bonferroni"}, \code{"dunnett"}, \code{"hochberg"},
#' \code{"holm_bonferroni"}, \code{"holm_sidak"}, \code{"none"}, \code{"sidak"},
#' and \code{"step_down_dunnett"}. Defaults to \code{"dunnett"}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be one of \code{"conjunctive"},
#' \code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_des_ma"} containing the
#' following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{numeric}} vector in the slot \code{$n} specifying
#' \ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the vector of sample
#' sizes required in each arm.
#' \item A \code{\link{numeric}} in the slot \code{$pi} specifying the critical
#' threshold for \emph{p}-values,
#' \ifelse{html}{\out{<i>&pi;</i></b>}}{\eqn{\pi}}, below which null hypotheses
#' would be rejected. Will be \code{\link{NA}} if \code{correction} is not a
#' single-step testing procedure.
#' \item A \code{\link{numeric}} vector in the slot \code{$piO} specifying the
#' critical thresholds for ordered \emph{p}-values,
#' \ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}}, to use with the
#' chosen step-wise testing procedure. Will be \code{\link{NA}} if
#' \code{correction} is not a step-wise testing procedure.
#' \item A \code{\link{matrix}} in the slot \code{$CovZ} specifying the
#' covariance matrix,
#' \ifelse{html}{\out{Cov(<b><i>Z</i></b>)}}{\eqn{Cov(\bold{Z})}}, of the
#' standardised test statistics.
#' \item A \code{\link{numeric}} vector in the slot \code{$rho} specifying the
#' vector of allocation ratios,
#' \ifelse{html}{\out{<b><i>&rho;</i></b>}}{\eqn{\bold{\rho}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' \dontrun{
#' # The design for the default parameters
#' des     <- des_int_ma()
#' # An A-optimal design
#' des_A   <- des_int_ma(ratio = "A")
#' # Modifying the number of experimental treatments and the total sample size
#' des_K_N <- des_int_ma(K = 3, N = 100)
#' }
#' @seealso \code{\link{an_ma}}, \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}.
#' @export
des_int_ma <- function(K = 2, N = 183, alpha = 0.05, beta = 0.2, delta1 = 0.5,
                       delta0 = 0, sigma = rep(1, K + 1), ratio = "D",
                       correction = "dunnett", power = "marginal",
                       summary = F) {

  ##### Check input variables ##################################################

  K <- check_integer_range(K, "K", c(1, Inf), 1)
  N <- check_integer_range(N, "N", c(K, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  check_sigma(sigma, K, "sigma", "K")
  check_belong(ratio, "ratio", c("A", "D", "E"), 1)
  check_belong(correction, "correction",
               c("benjamini_hochberg", "bonferroni", "dunnett", "hochberg",
                 "holm_bonferroni", "holm_sidak", "none", "sidak",
                 "step_down_dunnett"), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    summary_des_int_ma(K, N, alpha, beta, delta1, delta0, sigma, ratio,
                       correction)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Beginning the search for the ", ratio, "-optimal design",
            uc("two_elip"))
  }
  if (ratio == "A") {
    obj_func                                 <- trace_ma
  } else if (ratio == "D") {
    obj_func                                 <- determinant_ma
  } else if (ratio == "E") {
    obj_func                                 <- max_eigenvalue_ma
  }
  Kp1                                        <- K + 1L
  Kp2                                        <- K + 2L
  I                                          <- iterpc::iterpc(K + 1L, N,
                                                               replace = T)
  null_mat                                   <- matrix(0L, 100000L, Kp2)
  poss_n                                     <- list()
  poss_n[[1]]                                <- null_mat
  seq_Kp1                                    <- 1:Kp1
  counter_list                               <- counter_elem <- 1L
  length_I                                   <- iterpc::getlength(I)
  summary_i                                  <-
    round(seq(1, length_I, length.out = 11)[-c(1, 11)])
  if (length_I > 100000) {
    warning(length_I, " combinations will be considered. This may take some ",
            "time to complete.")
  }
  null_matrix                                <- matrix(0L, 100000L, Kp2)
  for (i in 1:length_I) {
    n                                        <-
      as.numeric(table(factor(iterpc::getnext(I), levels = seq_Kp1)))
    if (all(n > 0)) {
      poss_n[[counter_list]][counter_elem, ] <- c(n, obj_func(n, K, sigma))
      counter_elem                           <- counter_elem + 1L
      if (counter_elem == 100001) {
        counter_list                         <- counter_list + 1L
        poss_n[[counter_list]]               <- null_matrix
        counter_elem                         <- 1L
      }
    }
    if (all(i %in% summary_i, summary)) {
      message(uc("two_elip"), "approximately ", 10*which(summary_i == i),
              "% through the search", uc("two_elip"))
    }
  }
  if (summary) {
    message(uc("two_elip"), "completed the search for the ", ratio, "-optimal ",
            "design.")
    message("  Preparing outputs", uc("two_elip"))
  }
  if (counter_elem > 1) {
    poss_n[[counter_list]]                   <-
      poss_n[[counter_list]][1:(counter_elem - 1L), ]
  }
  poss_n                                     <- do.call(rbind, poss_n)
  n                                          <- poss_n[which.min(poss_n[, Kp2]),
                                                       seq_Kp1]
  CovTau                                     <- covariance_ma(K, rho, sigma)
  diag_sqrt_I                                <- diag(1/sqrt(diag(CovTau)))
  CovZ                                       <-
    diag_sqrt_I%*%CovTau%*%diag_sqrt_I
  pi_u                                       <- pi_ma(K, alpha, correction,
                                                      CovZ)
  pi                                         <- pi_u$pi
  piO                                        <- pi_u$piO
  u                                          <- pi_u$u
  uO                                         <- pi_u$uO
  tau                                        <-
    rbind(rep(0, K), rep(delta1, K), matrix(delta0, K, K) +
                                       (delta1 - delta0)*diag(K))
  if (correction %in% c("benjamini_hochberg", "hochberg", "holm_bonferroni",
                        "holm_sidak", "step_down_dunnett")) {
    opchar                                   <-
      opchar_ma_step(tau, K, sigma, correction, n, CovZ, uO)
  } else {
    opchar                                   <- opchar_ma_single(tau, K, sigma,
                                                                 n, CovZ, u)
  }

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        correction = correction,
                        CovZ       = CovZ,
                        delta0     = delta0,
                        delta1     = delta1,
                        integer    = T,
                        K          = K,
                        N          = N,
                        n          = n,
                        opchar     = opchar,
                        pi         = pi,
                        piO        = piO,
                        power      = power,
                        ratio      = n[-1]/n[1],
                        sigma      = sigma,
                        summary    = summary)
  class(output) <- c(class(output), "multiarm_des_ma")
  output

}
