#' Analyse results from a fixed-sample multi-arm clinical trial for a Bernoulli
#' distributed primary outcome
#'
#' \code{an_ma_bern()} analyses results from a specified fixed-sample multi-arm
#' clinical trial design for a Bernoulli distributed primary outcome variable,
#' to determine which null hypotheses to reject.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}, as
#' returned by \code{\link{build_ma_bern}} or \code{\link{des_ma_bern}} (i.e., a
#' fixed-sample multi-arm clinical trial design for a Bernoulli distributed
#' outcome). Defaults to \code{des_ma_bern()}.
#' @param pval A \code{\link{matrix}} (or \code{\link{numeric}} vector that can
#' be coerced in to \code{\link{matrix}} form) whose rows indicate the values of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}} for which to determine
#' which null hypotheses would be rejected. Defaults to
#' \code{rep(0.5*des$alpha, des$K)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (i.e., \code{alpha} can be used to modify/update \code{des} if desired).
#' Defaults to \code{des$alpha}.
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
#' \code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
#' \code{"des$correction"} (i.e., \code{correction} can be used to modify/update
#' \code{des} if desired).
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} containing the following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$an} detailing which null
#' hypotheses would be rejected for each value of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The results for the default parameters
#' an         <- an_ma_bern()
#' # An A-optimal design and a selection of possible p-values.
#' des_A      <- des_ma_bern(ratio = "A")
#' an_A       <- an_ma_bern(des_A, rbind(c(0.01, 0.01),
#'                                       c(0.025, 0.025),
#'                                       c(0.04, 0.04),
#'                                       c(0.05, 0.05),
#'                                       c(0.01, 0.06)))
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and a selection of possible
#' # p-values
#' des_root_K <- des_ma_bern(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' an_root_K  <- an_ma_bern(des_root_K, rbind(c(0.01, 0.01),
#'                                            c(0.025, 0.025),
#'                                            c(0.04, 0.04),
#'                                            c(0.05, 0.05),
#'                                            c(0.01, 0.06)))
#' @seealso \code{\link{build_ma_bern}}, \code{\link{des_ma_bern}},
#' \code{\link{opchar_ma_bern}}, \code{\link{plot.multiarm_des_ma_bern}},
#' \code{\link{sim_ma_bern}}.
#' @export
an_ma_bern <- function(des = des_ma_bern(), pval = rep(0.5*des$alpha, des$K),
                       alpha = des$alpha, correction = des$correction,
                       sigmas, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma_bern(des)
  pval <- check_pval(pval, des$K, "pval", "des$K")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_belong(correction, "correction",
               c("benjamini_hochberg", "benjamini_yekutieli", "bonferroni",
                 "dunnett", "hochberg", "holm_bonferroni", "holm_sidak", "none",
                 "sidak", "step_down_dunnett"), 1)
  if (missing(sigmas)) {
    sigmas <- matrix(sqrt(des$pi0*(1 - des$pi0)), nrow(pval), des$K + 1)
  } else {
    check_sigmas(sigmas, pval)
  }
  check_logical(summary, "summary")

  ##### Update des #############################################################

  if (any(correction != des$correction, alpha != des$alpha)) {
    des$correction                   <- correction
    des$alpha                        <- alpha
    sigma                            <-
      c(sqrt(des$pi0*(1 - des$pi0)),
        rep(sqrt((des$pi0 + des$delta1)*(1 - des$pi0 - des$delta1)), des$K))
    CovZ                             <- covariance_ma(des$K, des$n, sigma, T)
    gamma_u                          <- gamma_ma(des$K, des$alpha,
                                                 des$correction, CovZ)
    des$gamma                        <- gamma_u$gamma
    des$gammaO                       <- gamma_u$gammaO
    pi                               <-
      rbind(rep(des$pi0, des$K + 1), c(des$pi0,
                                       rep(des$pi0 + des$delta1, des$K)),
            cbind(rep(des$pi0, des$K),
                  matrix(des$pi0 + des$delta0, des$K, des$K) +
                    (des$delta1 - des$delta0)*diag(des$K)))
    if (correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                          "hochberg", "holm_bonferroni", "holm_sidak",
                          "step_down_dunnett")) {
      opchar                          <-
        opchar_ma_step_bern(pi, des$K, des$alpha, des$correction, des$n,
                            gamma_u$uO)
    } else {
      opchar                          <-
        opchar_ma_single_bern(des$pi, des$K, des$alpha, des$correction, des$n,
                              gamma_u$u)
    }
  }

  ##### Print summary ##########################################################

  if (summary) {
    summary_an_ma(des, pval, "bernoulli")
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Identifying which hypotheses to reject", uc("two_elip"))
  }
  nrow_pval                          <- nrow(pval)
  reject                             <- matrix(0L, nrow_pval, des$K)
  gamma                              <- des$gamma
  gammaO                             <- des$gammaO
  seq_K                              <- 1:des$K
  Kp1                                <- des$K + 1L
  one_min_alpha                      <- 1 - des$alpha
  for (i in 1:nrow_pval) {
    if (des$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      if (des$correction == "dunnett") {
        gamma                        <-
          stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                        sigma = covariance_ma(des$K, des$n,
                                                              sigmas[i, ],
                                                              T))$quantile,
                       lower.tail = F)
      }
      reject[i, ]                    <- (pval[i, ] < gamma)
    } else if (des$correction %in% c("holm_bonferroni", "holm_sidak",
                                     "step_down_dunnett")) {
      if (des$correction == "step_down_dunnett") {
        corr                         <- covariance_ma(des$K, des$n, sigmas[i, ],
                                                      T)[2, 1]
        gammaO                       <-
          sapply(seq_K,
                 function(k) {
                   dim               <- Kp1 - k
                   stats::pnorm(
                     mvtnorm::qmvnorm(one_min_alpha,
                                      sigma = matrix(corr, dim, dim) +
                                        (1 - corr)*diag(dim))$quantile,
                     lower.tail = F) })
      }
      order_pval                     <- order(pval[i, ])
      k                              <- check <- 1
      while (all(k <= des$K, check == 1)) {
        if (pval[i, order_pval[k]] < gammaO[k]) {
          reject[i, order_pval[k]]   <- reject[i, order_pval[k]] + 1
          k                          <- k + 1
        }
        else {
          check                      <- 0
        }
      }
    } else if (des$correction %in% c("benjamini_hochberg",
                                     "benjamini_yekutieli", "hochberg")) {
      order_pval                     <- order(pval[i, ])
      for (k in des$K:1) {
        if (pval[i, order_pval[k]] < des$gammaO[k]) {
          reject[i, order_pval[1:k]] <- rep(1, k)
          break
        }
      }
    }
  }
  if (summary) {
    message(uc("two_elip"), "hypotheses to reject identified.")
    message("  Preparing for outputting", uc("two_elip"))
  }
  an                                 <- cbind(pval, reject)
  colnames(an)                       <- c(paste("p", 1:des$K, sep = ""),
                                          paste("H", 1:des$K, sep = ""))
  an                                 <- tibble::as_tibble(an)

  ##### Outputting #############################################################

  if (summary) {
    message(uc("two_elip"), "outputting.")
  }
  list(alpha      = alpha,
       an         = an,
       correction = correction,
       des        = des,
       pval       = pval,
       sigmas     = sigmas,
       summary    = summary)

}
