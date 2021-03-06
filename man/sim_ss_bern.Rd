% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ss_bern.R
\name{sim_ss_bern}
\alias{sim_ss_bern}
\title{Empirically determine the operating characteristics of a single-stage
multi-arm clinical trial for a Bernoulli distributed primary outcome}
\usage{
sim_ss_bern(
  des = des_ss_bern(integer = TRUE),
  pi,
  replicates = 1e+05,
  summary = FALSE
)
}
\arguments{
\item{des}{A \code{\link{list}} of class \code{"multiarm_des_ss_bern"}, as
returned by \code{\link{build_ss_bern}} or \code{\link{des_ss_bern}} (i.e., a
single-stage multi-arm clinical trial design for a Bernoulli distributed
outcome). \strong{Note:} The sample sizes in all arms must be whole numbers.
Defaults to \code{des_ss_bern(integer = TRUE)}.}

\item{pi}{A \code{\link{matrix}} whose rows indicate values of
\ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}} at which to
evaluate the operating characteristics. Defaults internally to the global
null, global alternative, and each of the least favourable configurations if
unspecified.}

\item{replicates}{A \code{\link{numeric}} indicating the number of replicate
simulations to use for each value of
\ifelse{html}{\out{<b><i>&pi;</i></b>}}{\eqn{\bold{\pi}}}. Defaults to
\code{1e5}.}

\item{summary}{A \code{\link{logical}} variable indicating whether a summary
of the function's progress should be printed to the console. Defaults to
\code{FALSE}.}
}
\value{
A \code{\link{list}} containing the following elements
\itemize{
\item A \code{\link{tibble}} in the slot \code{$sim} giving the estimated
operating characteristics.
\item Each of the input variables.
}
}
\description{
\code{sim_ss_bern()} determines the operating characteristics of a specified
single-stage multi-arm clinical trial design assuming the primary outcome is
Bernoulli distributed, for given values of the true treatment effects, using
simulation.
}
\examples{
\dontrun{
# The estimated operating characteristics for the default parameters
sim   <- sim_ss_bern()
# An A-optimal design, specifying tau explicitly
des_A <- des_ss_bern(ratio = "A", integer = T)
sim_A <- sim_ss_bern(des_A, rbind(c(0, 0),
                                  c(0.5, 0.5),
                                  c(0.5, 0),
                                  c(0, 0.5)))
}
}
\seealso{
\code{\link{build_ss_bern}}, \code{\link{des_ss_bern}},
\code{\link{opchar_ss_bern}}, \code{\link{plot.multiarm_des_ss_bern}}.
}
