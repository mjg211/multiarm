#' Design a multi-stage drop-the-losers multi-arm clinical trial for a Bernoulli
#' distributed primary outcome
#'
#' \code{des_dtl_bern()} determines multi-stage drop-the-losers multi-arm
#' clinical trial designs assuming the primary outcome variable is Bernoulli
#' distributed. It computes required design components and returns information
#' on key operating characteristics.
#'
#' @param Kv A \code{\link{numeric}} \code{\link{vector}} of strictly decreasing
#' values, indicating the chosen value for
#' \ifelse{html}{\out{<b><i>K</i></b>}}{\eqn{\bold{K}}}, the number of
#' experimental treatment arms present in each stage. Defaults to
#' \code{c(2, 1)}.
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
#' 'interesting' treatment effect. Defaults to \code{0.2}.
#' @param delta0 A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
#' 'uninteresting' treatment effect. Defaults to \code{0}.
#' @param ratio A \code{\link{numeric}} indicating the chosen value for
#' \ifelse{html}{\out{<i>r</i>}}{\eqn{r}}, the stage-wise allocation ratio to
#' present experimental arms. Defaults to \code{1}.
#' @param power A \code{\link{character}} string indicating the chosen type of
#' power to design the trial for. Can be \code{"disjunctive"} or
#' \code{"marginal"}. Defaults to \code{"marginal"}.
#' @param type A \code{\link{character}} string indicating the choice for the
#' stage-wise sample size. Can be \code{"variable"} or \code{"fixed"}. Defaults
#' to \code{"variable"}.
#' @param spacing A \code{\link{numeric}} \code{\link{vector}} indicating the
#' chosen spacing of the interim analyses in terms of the proportion of the
#' maximal possible sample size. It must contain strictly increasing values,
#' with final element equal to \code{1}. Defaults to
#' \code{((1:length(Kv))/length(Kv)} (i.e., to equally spaced analyses).
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
#' \code{"multiarm_des_dtl_bern"}, containing the following elements:
#' \itemize{
#' \item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
#' operating characteristics of the identified design.
#' \item A \code{\link{numeric}} in the slot \code{$e} specifying
#' \ifelse{html}{\out{<i>e</i>}}{\eqn{e}}, the trial's critical rejection
#' boundary for the final analysis.
#' \item A \code{\link{numeric}} in the slot \code{$maxN} specifying
#' \ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
#' size.
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
#' des <- des_dtl_bern()
#' @seealso \code{\link{build_dtl_bern}}, \code{\link{gui}},
#' \code{\link{opchar_dtl_bern}}, \code{\link{plot.multiarm_des_dtl_bern}},
#' \code{\link{sim_dtl_bern}}.
#' @export
des_dtl_bern <- function(Kv = c(2, 1), alpha = 0.025, beta = 0.1, pi0 = 0.3,
                         delta1 = 0.2, delta0 = 0, ratio = 1,
                         power = "marginal", type = "variable",
                         spacing = (1:length(Kv))/length(Kv), integer = F,
                         summary = F) {

  ##### Check input variables ##################################################

  Kv <- check_Kv(Kv, "Kv")
  check_real_range_strict(alpha, "alpha", c(0, 1), 1)
  check_real_range_strict(beta, "beta", c(0, 1), 1)
  check_real_range_strict(pi0, "pi0", c(0, 1), 1)
  check_delta0_delta1(delta0, delta1, "delta0", "delta1", pi0, "pi0")
  check_ratio(ratio, name_ratio = "ratio", des = "dtl")
  check_belong(power, "power", c("disjunctive", "marginal"), 1)
  check_belong(type, "type", c("fixed", "variable"), 1)
  #check_spacing(spacing, "spacing", J)
  check_logical(integer, "integer")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  comp <- components_dtl_init(alpha, beta, delta0, delta1, integer, Kv, power,
                              ratio, spacing, summary, type, pi0 = pi0)
  if (summary) {
    #summary_des_dtl_bern(comp)
    message("")
  }

  ##### Perform main computations ##############################################

  if (summary) {
    message("  Building required design components..")
  }
  comp          <- components_dtl_hg_lfc(comp)
  if (summary) {
    message("..built required design components.")
    message("  Identifying the stopping boundaries..")
  }
  comp$e        <- stats::uniroot(f        = root_bounds_dtl,
                                  interval = c(-10, 10),
                                  comp     = comp)$root
  comp          <- components_dtl_update_bounds(comp$e, comp)
  N             <- des_ss_bern(Kv[1], alpha, beta, pi0, delta1, delta0,
                               rep(ratio, Kv[1]), "dunnett", "marginal", F,
                               "HG", F)$N
  comp$n_factor <- stats::uniroot(f        = root_ss_dtl,
                                  interval = c(1e-16, N),
                                  comp     = comp)$root
  if (summary) {
    message("..identified the required sample size.")
    message("  Preparing for outputting..")
  }
  comp          <- integer_dtl(comp)
  if (type == "variable") {
    n10         <- comp$n_factor
    n1          <- comp$n_factor*(1 + ratio*Kv[1])
  } else if (type == "fixed") {
    n10         <- comp$n_factor/(1 + ratio*Kv[1])
    n1          <- comp$n_factor
  }
  comp          <-
    components_dtl_update(comp,
                          pi = pi0 +
                            cbind(0, rbind(rep(0, Kv[1]), rep(delta1, Kv[1]),
                                           matrix(delta0, Kv[1], Kv[1]) +
                                             (delta1 - delta0)*diag(Kv[1]))))
  comp          <- opchar_dtl_internal(comp)

  ##### Outputting #############################################################

  if (summary) {
    message("..outputting.")
  }
  output        <- list(alpha      = alpha,
                        beta       = beta,
                        delta0     = delta0,
                        delta1     = delta1,
                        e          = comp$e,
                        integer    = integer,
                        Kv         = Kv,
                        maxN       = comp$opchar$maxN[1],
                        n_factor   = comp$n_factor,
                        n1         = n1,
                        n10        = n10,
                        opchar     = comp$opchar,
                        pi0        = pi0,
                        power      = power,
                        ratio      = ratio,
                        spacing    = spacing,
                        summary    = summary,
                        type       = type)
  class(output) <- c("multiarm_des_dtl_bern", class(output))
  output

}
