#' Design a multi-stage group-sequential multi-arm clinical trial for a
#' Bernoulli distributed primary outcome
#'
#' \code{des_gs_bern()} determines multi-stage group-sequential multi-arm
#' clinical trial designs assuming the primary outcome variable is Bernoulli
#' distributed. It computes required design components and returns information
#' on key operating characteristics.
#'
#' @param K A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}, the (initial) number of experimental
#' treatment arms. Defaults to \code{2}.
#' @param J A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>J</i>}}{\eqn{J}}, the (maximum) number of allowed
#' stages. Defaults to \code{2}.
#' @param alpha A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level
#' (family-wise error-rate). Defaults to \code{0.025}.
#' @param beta A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
#' the desired power. Defaults to \code{0.1}.
#' @param pi0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&pi;</i><sub>0</sub>}}{\eqn{\pi_0}}, the
#' response rate in the control arm. Defaults to \code{0.3}.
#' @param delta1 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
#' 'interesting' treatment effect. Defaults to \code{0.5}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
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
#' @param fshape A \code{\link{character}} string indicating the choice for the
#' futility (lower) stopping boundaries. Can be any of \code{"fixed"},
#' \code{"obf"}, \code{"pocock"}, and \code{"triangular"}. Defaults to
#' \code{"pocock"}.
#' @param eshape A \code{\link{character}} string indicating the choice for the
#' efficacy (upper) stopping boundaries. Can be any of \code{"fixed"},
#' \code{"obf"}, \code{"pocock"}, and \code{"triangular"}. Defaults to
#' \code{"pocock"}.
#' @param ffix A \code{\link{numeric}} indicating the chosen value for the fixed
#' interim futility bounds. Only used when \code{fshape = "fixed"}. Defaults to
#' \code{"-3"}.
#' @param efix A \code{\link{numeric}} indicating the chosen value for the fixed
#' interim efficacy bounds. Only used when \code{eshape = "fixed"}. Defaults to
#' \code{"3"}.
#' @param integer A \code{\link{logical}} variable indicating whether the
#' computed possible sample sizes required in each arm in each stage should be
#' forced to be whole numbers. Defaults to \code{FALSE}. WARNING: If you set
#' \code{integer = TRUE} and \code{ratio != 1}, obscure results can occur due to
#' difficulties in identifying a suitable whole number sample size that meets
#' the allocation ratio requirement.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @return A \code{\link{list}}, with additional class
#' \code{"multiarm_des_gs_bern"}, containing the following elements:
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{tibble}} in the slot \code{$pmf_N} summarising the
#' probability mass function of the random required sample size under key
#' scenarios.
#' \item A \code{\link{numeric}} \code{\link{vector}} in the slot \code{$e}
#' specifying \ifelse{html}{\out{<b><i>e</i></b>}}{\eqn{\bold{e}}}, the trial's
#' efficacy (upper) stopping boundaries.
#' \item A \code{\link{numeric}} \code{\link{vector}} in the slot \code{$f}
#' specifying \ifelse{html}{\out{<b><i>f</i></b>}}{\eqn{\bold{f}}}, the trial's
#' futility (lower) stopping boundaries.
#' \item A \code{\link{numeric}} in the slot \code{$maxN} specifying
#' \ifelse{html}{\out{max <i>N</i>}}{max \eqn{N}}, the trial's maximum required
#' sample size.
#' \item A \code{\link{numeric}} in the slot \code{$n_factor}, for internal use
#' in other functions.
#' \item A \code{\link{numeric}} in the slot \code{$n1} specifying
#' \ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_1}}, the total sample size
#' required in stage one of the trial.
#' \item A \code{\link{numeric}} in the slot \code{$n10} specifying
#' \ifelse{html}{\out{<i>n</i><sub>10</sub>}}{\eqn{n_{10}}}, the sample size
#' required in the control arm in stage one of the trial.
#' \item Each of the input variables.
#' }
#' @examples
#' # The design for the default parameters
#' des <- des_gs_bern()
#' @seealso \code{\link{build_gs_bern}}, \code{\link{gui}},
#' \code{\link{opchar_gs_bern}}, \code{\link{plot.multiarm_des_gs_bern}},
#' \code{\link{sim_gs_bern}}.
#' @export
des_gs_bern <- function(K = 2, J = 2, alpha = 0.025, beta = 0.1, pi0 = 0.3,
                        delta1 = 0.2, delta0 = 0, ratio = 1,
                        power = "marginal", stopping = "simultaneous",
                        type = "variable", fshape = "pocock",
                        eshape = "pocock", ffix = -3, efix = 3, integer = F,
                        summary = F) {

  ##### Check input variables ##################################################

  K <- check_integer_range(K, "K", c(1, Inf), 1)
  J <- check_integer_range(J, "J", c(1, Inf), 1)
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1", pi0, "pi0")
  check_real_range_strict(ratio, "ratio", c(0, Inf), 1)
  check_belong(power, "power", c("conjunctive", "disjunctive", "marginal"), 1)
  check_belong(stopping, "stopping", c("simultaneous", "separate"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  check_boundaries(fshape, eshape, ffix, efix, "fshape", "eshape", "ffix",
                   "efix")
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  comp <- components_gs_init(alpha, beta, delta0, delta1, efix, eshape, ffix,
                             fshape, integer, J, K, power, ratio, stopping,
                             summary, type, pi0 = pi0)
  if (summary) {
    #summary_des_gs_bern(comp)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Building required design components..")
  }
  comp          <- components_gs_hg_lfc(comp)
  if (summary) {
    message("..built required design components.")
    message("  Identifying the stopping boundaries..")
  }
  C             <- stats::uniroot(f        = root_bounds_gs,
                                  interval = c(1e-16, 1e2),
                                  comp     = comp)$root
  comp          <- bounds_gs(C, comp)
  if (summary) {
    message("..identified the stopping boundaries.")
    message("  Identifying the required sample size..")
  }
  comp$n_factor <- stats::uniroot(f        = root_ss_gs,
                                  interval = c(1e-16, 1e4),
                                  comp     = comp)$root
  if (summary) {
    message("..identified the required sample size.")
    message("  Preparing for outputting..")
  }
  comp          <- integer_gs(comp)
  if (type == "variable") {
    n10         <- comp$n_factor
    n1          <- comp$n_factor*(1 + ratio*K)
  } else if (type == "fixed") {
    n10         <- comp$n_factor/(1 + ratio*K)
    n1          <- comp$n_factor
  }
  comp          <-
    components_gs_update(comp,
                         pi = pi0 +
                           cbind(0, rbind(rep(0, K), rep(delta1, K),
                                          matrix(delta0, K, K) +
                                            (delta1 - delta0)*diag(K))))
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
                        efix       = efix,
                        eshape     = eshape,
                        f          = comp$f,
                        ffix       = ffix,
                        fshape     = fshape,
                        integer    = integer,
                        J          = J,
                        K          = K,
                        maxN       = comp$opchar$maxN[1],
                        n_factor   = comp$n_factor,
                        n10        = n10,
                        n1         = n1,
                        opchar     = comp$opchar,
                        pi0        = pi0,
                        pmf_N      = comp$pmf_N,
                        power      = power,
                        ratio      = ratio,
                        stopping   = stopping,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_gs_bern", class(output))
  output

}
