#' Build a multi-stage group-sequential multi-arm clinical trial for a normally
#' distributed primary outcome
#'
#' \code{build_gs_norm()} builds a multi-stage group-sequential multi-arm
#' clinical trial design object assuming the primary outcome variable is
#' normally distributed, like those returned by \code{\link{des_gs_norm}}.
#'
#' @param n10 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>n</i><sub>10</sub>}}{\eqn{n_{10}}}, the sample size
#' required in the control arm in stage one of the trial. Defaults to \code{54}.
#' @param n1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the total sample size
#' required in stage one of the trial. Defaults to \code{n10\*(1 + K\*ratio)}.
#' @param e A \code{\link{numeric}} \code{\link{vector}} indicating the chosen
#' value for \ifelse{html}{\out{<b><i>e</i></b>}}{\eqn{\bold{e}}}, the efficacy
#' (upper) stopping boundaries. Defaults to \code{c(2.42, 2.42)}.
#' @param f A \code{\link{numeric}} \code{\link{vector}} indicating the chosen
#' value for \ifelse{html}{\out{<b><i>f</i></b>}}{\eqn{\bold{f}}}, the futility
#' (lower) stopping boundaries. Defaults to \code{c(-2.42, 2.42)}.
#' @param K A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{\bold{K}}}, the (initial) number of
#' experimental treatment arms. Defaults to \code{2}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (family-wise error-rate). Defaults to \code{0.025}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to \code{0.1}.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to \code{0.5}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
#' @param sigma A \code{\link{numeric}} \code{\link{vector}} indicating the
#' chosen values for
#' \ifelse{html}{\out{<i>&sigma;</i><sub>0</sub>}}{\eqn{\sigma_0}} and
#' \ifelse{html}{\out{<i>&sigma;</i><sub>1</sub>}}{\eqn{\sigma_1}}, the standard
#' deviations of the responses in the control and experimental arms. Must be of
#' \code{\link{length}} 1 or 2. If of \code{\link{length}} 1, it is assumed that
#' \ifelse{html}{\out{<i>&sigma;</i><sub>0</sub>
#' =<i>&sigma;</i><sub>1</sub>}}{\eqn{\sigma_0=\sigma_1}}.
#' Defaults to \code{1}.
#' @param ratio A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>r</i>}}{\eqn{r}}, the stage-wise allocation ratio to
#' present experimental arms. Defaults to \code{1}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be \code{"disjunctive"} or
#' \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param stopping A \code{\link{character}} string indicating the chosen type
#' of stopping rule. Can be \code{"separate"} or \code{"simultaneous"}. Defaults
#' to \code{"simultaneous"}.
#' @param type A \code{\link{character}} string indicating the choice for the
#' stage-wise sample size. Can be \code{"variable"} or \code{"fixed"}. Defaults
#' to \code{"variable"}.
#' @param spacing A \code{\link{numeric}} \code{\link{vector}} indicating the
#' chosen spacing of the interim analyses in terms of the proportion of the
#' maximal possible sample size. It must contain strictly increasing values,
#' with final element equal to \code{1}. Defaults to
#' \code{(1:length(e))/length(e)} (i.e., to equally spaced analyses).
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}}, with additional class
#' \code{"multiarm_des_gs_norm"}, containing the following elements:
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{tibble}} in the slot \code{$pmf_N} summarising the
#' probability mass function of the random required sample size under key
#' scenarios.
#' \item A \code{\link{numeric}} in the slot \code{$maxN} specifying
#' \ifelse{html}{\out{max <i>N</i>}}{max \eqn{N}}, the trial's maximum required
#' sample size.
#' \item A \code{\link{numeric}} in the slot \code{$n_factor}, for internal use
#' in other functions.
#' \item Each of the input variables.
#' }
#' @examples
#' # The design for the default parameters
#' des <- build_gs_norm()
#' @seealso \code{\link{des_gs_norm}}, \code{\link{gui}},
#' \code{\link{opchar_gs_norm}}, \code{\link{plot.multiarm_des_gs_norm}},
#' \code{\link{sim_gs_norm}}.
#' @export
build_gs_norm <- function(n10 = 77, n1 = n10*(1 + K*ratio), e = c(2.42, 2.42),
                          f = c(-2.42, 2.42), K = 2, alpha = 0.025, beta = 0.1,
                          delta1 = 0.5, delta0 = 0, sigma = 1, ratio = 1,
                          power = "marginal", stopping = "simultaneous",
                          type = "variable", spacing = (1:length(e))/length(e),
                          summary = FALSE) {

  ##### Check input variables ##################################################

  check_real_range_strict(n10, "n10", c(0, Inf), 1)
  check_real_range_strict(n1, "n1", c(0, Inf), 1)
  #check_ef(e, f, "e", "f")
  J             <- length(e)
  K             <- check_integer_range(K, "K", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1")
  sigma         <- check_sigma(sigma, name_sigma = "sigma", des = "mams")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_belong(stopping, "stopping", c("simultaneous", "separate"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  #check_spacing(spacing, "spacing", J)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_build_gs_norm(n10, n1, e, f, K, alpha, beta, delta1, delta0, sigma,
    #                      ratio, power, spacing, stopping, type)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message(" Building outputs..")
  }
  integer       <- all(c(n1, n10) %% 1 == 0)
  if (type == "variable") {
    n_factor    <- n10
  } else {
    n_factor    <- n1
  }
  comp          <- components_gs_init(alpha, beta, delta0, delta1, NA, NA, NA,
                                      NA, integer, J, K, power, ratio, spacing,
                                      stopping, summary, type, sigma,
                                      n_factor = n_factor, f = f, e = e)
  tau           <- rbind(numeric(K), rep(delta1, K),
                         matrix(delta0, K, K) + (delta1 - delta0)*diag(K))
  comp          <- components_gs_update(comp, tau)
  comp          <- opchar_gs_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = comp$e,
                        efix       = NA,
                        eshape     = NA,
                        f          = comp$f,
                        ffix       = NA,
                        fshape     = NA,
                        integer    = integer,
                        J          = J,
                        K          = K,
                        maxN       = comp$opchar$maxN[1],
                        n_factor   = comp$n_factor,
                        n10        = n10,
                        n1         = n1,
                        opchar     = comp$opchar,
                        pmf_N      = comp$pmf_N,
                        power      = power,
                        ratio      = ratio,
                        sigma      = sigma,
                        spacing    = spacing,
                        stopping   = stopping,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_gs_norm", class(output))
  output

}
