% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_ss_pois.R
\name{build_ss_pois}
\alias{build_ss_pois}
\title{Build a single-stage multi-arm clinical trial for a Poisson distributed
primary outcome}
\usage{
build_ss_pois(
  n = rep(141, 3),
  alpha = 0.025,
  beta = 0.1,
  lambda0 = 5,
  delta1 = 1,
  delta0 = 0,
  correction = "dunnett",
  power = "marginal",
  summary = F
)
}
\arguments{
\item{n}{A \code{\link{numeric}} vector indicating the chosen value for
\ifelse{html}{\out{<b><i>n</i></b>}}{\eqn{\bold{n}}}, the sample size in each
arm. Defaults to \code{rep(88, 3)}.}

\item{alpha}{A \code{\link{numeric}} indicating the chosen value for
\ifelse{html}{\out{<i>&alpha;</i>}}{\eqn{\alpha}}, the significance level.
Defaults to \code{0.05}.}

\item{beta}{A \code{\link{numeric}} indicating the chosen value for
\ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}}, used in the definition of
the desired power. Defaults to \code{0.2}.}

\item{lambda0}{A \code{\link{numeric}} indicating the chosen value for
\ifelse{html}{\out{<i>&lambda;</i><sub>0</sub>}}{\eqn{\lambda_0}}, the
event rate in the control arm. Defaults to \code{5}.}

\item{delta1}{A \code{\link{numeric}} indicating the chosen value for
\ifelse{html}{\out{<i>&delta;</i><sub>1</sub>}}{\eqn{\delta_1}}, the
'interesting' treatment effect. Defaults to \code{1}.}

\item{delta0}{A \code{\link{numeric}} indicating the chosen value for
\ifelse{html}{\out{<i>&delta;</i><sub>0</sub>}}{\eqn{\delta_0}}, the
'uninteresting' treatment effect. Defaults to \code{0}.}

\item{correction}{A \code{\link{character}} string indicating the chosen
multiple comparison correction. Can be any of \code{"benjamini_hochberg"},
\code{"benjamini_yekutieli"}, \code{"bonferroni"}, \code{"dunnett"},
\code{"hochberg"}, \code{"holm_bonferroni"}, \code{"holm_sidak"},
\code{"none"}, \code{"sidak"}, and \code{"step_down_dunnett"}. Defaults to
\code{"dunnett"}.}

\item{power}{A \code{\link{character}} string indicating the chosen type of
power to design the trial for. Can be one of \code{"conjunctive"},
\code{"disjunctive"}, and \code{"marginal"}. Defaults to \code{"marginal"}.}

\item{summary}{A \code{\link{logical}} variable indicating whether a summary
of the function's progress should be printed to the console. Defaults to
\code{F}.}
}
\value{
A \code{\link{list}} of class \code{"multiarm_des_ss_pois"}
containing the following elements
\itemize{
\item A \code{\link{tibble}} in the slot \code{$opchar} summarising the
operating characteristics of the chosen design.
\item A \code{\link{numeric}} in the slot \code{$N} specifying
\ifelse{html}{\out{<i>N</i>}}{\eqn{N}}, the trial's total required sample
size.
\item A \code{\link{numeric}} in the slot \code{$gamma} specifying the
critical threshold for \emph{p}-values,
\ifelse{html}{\out{<i>&gamma;</i></b>}}{\eqn{\gamma}}, below which null
hypotheses would be rejected. Will be \code{\link{NA}} if \code{correction}
is not a single-step testing procedure.
\item A \code{\link{numeric}} vector in the slot \code{$gammaO} specifying
the critical thresholds for ordered \emph{p}-values,
\ifelse{html}{\out{<b><i>&gamma;</i></b>}}{\eqn{\bold{\gamma}}}, to use with
the chosen step-wise testing procedure. Will be \code{\link{NA}} if
\code{correction} is not a step-wise testing procedure.
\item A \code{\link{numeric}} vector in the slot \code{$ratio} specifying the
vector of allocation ratios,
\ifelse{html}{\out{<b><i>r</i></b>}}{\eqn{\bold{r}}}.
\item Each of the input variables.
}
}
\description{
\code{build_ss_pois()} builds a single-stage multi-arm clinical trial design
object assuming the primary outcome variable is Poisson distributed, like
those returned by \code{\link{des_ss_pois}}.
}
\examples{
# The design for the default parameters
des     <- build_ss_pois()
# Modifying the number of experimental treatments and the sample size in each
# arm
des_K_n <- build_ss_pois(n = rep(100, 4))
}
\seealso{
\code{\link{des_ss_pois}}, \code{\link{opchar_ss_pois}},
\code{\link{plot.multiarm_des_ss_pois}}, \code{\link{sim_ss_pois}}.
}
