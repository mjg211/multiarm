#' Analyse results from a fixed-sample multi-arm clinical trial
#'
#' \code{an_ma()} analyses results from a specified fixed-sample multi-arm
#' clinical trial design, to determine which null hypotheses to reject.
#'
#' @param des A \code{\link{list}} of class \code{"multiarm_des_ma"}, as
#' returned by \code{\link{build_ma}}, \code{\link{des_ma}}, or
#' \code{\link{des_int_ma}} (i.e., a fixed-sample multi-arm clinical trial
#' design). Defaults to \code{des_ma()}.
#' @param pval A \code{\link{matrix}} (or \code{\link{numeric}} vector that can
#' be coerced in to \code{\link{matrix}} form) whose rows indicate the values of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}} for which to determine
#' which null hypotheses would be rejected. Defaults to
#' \code{rep(0.5*des$alpha, des$K)}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (i.e., \code{alpha} can be used to modify/update \code{des} if desired).
#' Defaults to \code{des$alpha}
#' @param correction A \code{\link{character}} string indicating the chosen
#' multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
#' \code{"bonferroni"}, \code{"dunnett"}, \code{"hochberg"}, \code{"holm"},
#' \code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}.
#' Defaults to \code{"dunnett"} (i.e., \code{correction} can be used to
#' modify/update \code{des} if desired). Defaults to \code{des$correction}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @return A \code{\link{list}} of class \code{"multiarm_an_ma"} containing the
#' following elements
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$an} detailing which null
#' hypotheses would be rejected for each value of
#' \ifelse{html}{\out{<b><i>p</i></b>}}{\eqn{\bold{p}}}.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The results for the default parameters
#' an         <- an_ma()
#' # An A-optimal design and a selection of possible p-values.
#' des_A      <- des_ma(ratio = "A")
#' an_A       <- an_ma(des_A, rbind(c(0.01, 0.01),
#'                                  c(0.025, 0.025),
#'                                  c(0.04, 0.04),
#'                                  c(0.05, 0.05),
#'                                  c(0.01, 0.06)))
#' # Using the root-K allocation rule, modifying the desired type of power and
#' # chosen multiple comparison correction, and a selection of possible
#' # p-values
#' des_root_K <- des_ma(ratio      = rep(1/sqrt(2), 2),
#'                      correction = "holm",
#'                      power      = "disjunctive")
#' an_root_K  <- an_ma(des_root_K, rbind(c(0.01, 0.01),
#'                                       c(0.025, 0.025),
#'                                       c(0.04, 0.04),
#'                                       c(0.05, 0.05),
#'                                       c(0.01, 0.06)))
#' @seealso \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}, and their
#' associated S3 \code{\link{print}} and \code{\link{summary}} generics.
#' @export
an_ma <- function(des = des_ma(), pval = rep(0.5*des$alpha, des$K),
                  alpha = des$alpha, correction = des$correction, summary = F) {

  ##### Check input variables ##################################################

  check_multiarm_des_ma(des)
  pval <- check_pval(pval, des$K, "pval", "des$K")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_belong(correction, "correction",
               c("benjamini_hochberg", "bonferroni", "dunnett", "hochberg",
                 "holm", "none", "sidak", "step_down_dunnett"), 1)
  check_logical(summary, "summary")

  ##### Update des #############################################################

  if (any(correction != des$correction, alpha != des$alpha)) {
    des$correction                   <- correction
    des$alpha                        <- alpha
    CovTau                           <- covariance_ma(des$K, des$n, des$sigma)
    diag_sqrt_I                      <- diag(1/sqrt(diag(CovTau)))
    des$CovZ                         <-
      diag_sqrt_I%*%CovTau%*%diag_sqrt_I
    des$pi                           <- des$piO <- NA
    if (correction == "benjamini_hochberg") {
      des$piO                        <- (1:des$K)*des$alpha/des$K
    } else if (correction == "bonferroni") {
      des$pi                         <- des$alpha/des$K
    } else if (correction == "dunnett") {
      des$pi                         <-
        stats::pnorm(mvtnorm::qmvnorm(1 - des$alpha, sigma = des$CovZ)$quantile,
                     lower.tail = F)
    } else if (correction %in% c("hochberg", "holm")) {
      des$piO                        <- des$alpha/(des$K:1)
    } else if (correction == "none") {
      des$pi                         <- des$alpha
    } else if (correction == "sidak") {
      des$pi                         <- 1 - (1 - des$alpha)^(1/des$K)
    } else if (correction == "step_down_dunnett") {
      Ksq                            <- des$K^2
      if (length(unique(des$CovZ[(1:Ksq)[-seq(1, Ksq, des$K + 1)]])) > 1) {
        stop("The step-down Dunnett correction is only supported for scenarios",
             " with a shared covariance between the test statistics")
      }
      corr                           <- des$CovZ[2, 1]
      one_min_alpha                  <- 1 - des$alpha
      des$piO                        <-
        sapply(1:des$K, function(k) {
          dim <- des$K - (k - 1L)
          stats::pnorm(mvtnorm::qmvnorm(one_min_alpha,
                                        sigma = matrix(corr, dim, dim) +
                                          (1 - corr)*diag(dim))$quantile,
                       lower.tail = F) })
    }
    des$u                            <- stats::qnorm(1 - des$pi)
    des$uO                           <- stats::qnorm(1 - des$piO)
    tau                              <- rbind(rep(0, des$K),
                                              rep(des$delta1, des$K),
                                              matrix(des$delta0, des$K,
                                                     des$K) +
                                                (des$delta1 - des$delta0)*
                                                  diag(des$K))
    if (des$correction %in% c("benjamini_hochberg", "hochberg", "holm",
                              "step_down_dunnett")) {
      des$opchar                     <- opchar_ma_step(tau, des$K, des$sigma,
                                                       des$correction, des$n,
                                                       des$CovZ, des$uO)
    } else {
      des$opchar                     <- opchar_ma_single(tau, des$K, des$sigma,
                                                         des$n, des$CovZ, des$u)
    }
  }

  ##### Print summary ##########################################################

  if (summary) {
    summary_an_ma(des, pval)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Identifying which hypotheses to reject", uc("two_elip"))
  }
  nrow_pval                          <- nrow(pval)
  reject                             <- matrix(0L, nrow_pval, des$K)
  for (i in 1:nrow_pval) {
    if (des$correction %in% c("bonferroni", "dunnett", "none", "sidak")) {
      reject[i, ]                    <- (pval[i, ] < des$pi)
    }
    else if (des$correction %in% c("holm", "step_down_dunnett")) {
      order_pval                     <- order(pval[i, ])
      k                              <- check <- 1
      while (all(k <= des$K, check == 1)) {
        if (pval[i, order_pval[k]] < des$piO[k]) {
          reject[i, order_pval[k]]   <- reject[i, order_pval[k]] + 1
          k                          <- k + 1
        }
        else {
          check                      <- 0
        }
      }
    }
    else if (des$correction %in% c("benjamini_hochberg", "hochberg")) {
      order_pval                     <- order(pval[i, ])
      for (k in des$K:1) {
        if (pval[i, order_pval[k]] < des$piO[k]) {
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
       summary    = summary)

}
