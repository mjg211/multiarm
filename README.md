
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiarm <img src='man/figures/multiarm.png' align="right" height="139" />

[![](https://img.shields.io/badge/devel%20version-0.10.0-blue.svg)](https://github.com/mjg211/multiarm)
[![](https://img.shields.io/github/languages/code-size/mjg211/multiarm.svg)](https://github.com/mjg211/multiarm)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
![](https://img.shields.io/badge/contributions-welcome-blue.svg)

## Description

`multiarm` provides functions to assist with the design and analysis of
multi-arm clinical trials. Available functions allow for sample size
determination when utilising a variety of multiple comparison
corrections (including for *A*-, *D*-, and *E*-optimal designs), trial
simulation, analytical operating characteristic calculation (including
the conjunctive power, disjunctive power, family-wise error-rate, and
false discovery rate), and the production of several plots. An R Shiny
graphical user interface is also provided to aid design determination.

## Getting started

You can install the latest development version of multiarm from
[Github](https://github.com/) with:

``` r
devtools::install_github("mjg211/multiarm")
```

An introductory example of how to make use of the package’s core
functionality can be found below. More detailed support is available in
the package vignette, which can be accessed with `vignette("multiarm")`.
For further help, please contact Michael Grayling at
<michael.grayling@newcastle.ac.uk>.

## Details

Currently, functions are provided to support trials in which the primary
outcome variable is assumed to be eiher normally or Bernoulli
distributed. In total, 14 functions are available. Their naming
conventions are such that several character strings are joined together
separated by underscores. The first character string indicates the type
of calculation the function performs (e.g., design determination,
operating characteristic calculations), and the remainder which type of
design it is for (normally or Bernoulli distributed outcomes):

  - `an_ma()` and `an_ma_bern()`: Analyse summary statistics from
    fixed-sample multi-arm clinical trial designs, in order to determine
    which null hypotheses to reject.
  - `build_ma()` and `build_ma_bern()`: Build multi-arm clinical trial
    design objects, like those returned by `des_ma()` and
    `des_ma_bern()`. For use when a specific design is of interest.
  - `des_int_ma()`: Determines the *A*-, *D*-, and *E*-optimal
    allocation of a set of patients in a multi-arm clinical trial with
    normally distributed outcomes.
  - `des_ma()` and `des_ma_bern()`: Determine the sample size required
    by a multi-arm clinical trial when one of several multiple
    comparison corrections is used.
  - `gui()`: Provides a graphical user interface to design
    determination.
  - `opchar_ma()` and `opchar_ma_bern()`: Determine the operating
    characteristics (power, family-wise error rates, etc.) of supplied
    fixed-sample multi-arm clinical trial designs using multivariate
    normal integration.
  - `plot.multiarm_des_ma()` and `plot.multiarm_des_ma_bern()`: Produce
    informative plots (power, false discovery rate curves, etc.)
    relating to supplied fixed-sample multi-arm clinical trial designs.
  - `sim_ma()` and `sim_ma_bern()`: Empirically estimate the operating
    characteristics (power, family-wise error rates, etc.) of supplied
    fixed-sample multi-arm clinical trial designs, via simulation.

## Example: Normally distributed outcomes

Typically, `des_ma()` would be used first to identify a design for the
trial parameters of interest. For example, consider designing a trial
for:

  - three experimental treatment arms (see `K`);
  - desiring a familywise error-rate of at most 5%, controlling using
    Dunnett’s correction (see `alpha` and `correction`);
  - desiring marginal power to reject each null hypothesis of 80% for a
    clinically relevant difference of 1 (see `beta`, `delta1`, and
    `power`);
  - assuming the standard deviation of all responses is 1 (see `sigma`);
  - allocating patients equally to each arm (see `ratio`).

To compute the implied design, we would run:

``` r
des <- des_ma(K          = 3,
              alpha      = 0.05,
              beta       = 0.2,
              delta1     = 1,
              sigma      = rep(1, 4),
              ratio      = rep(1, 3),
              correction = "dunnett",
              power      = "marginal")
```

Then, the total required sample size is:

``` r
des$N
#> [1] 67.46725
```

In addition, the operating characteristics under the *global null*,
*global alternative*, and each of the *least favourable configurations*,
can be accessed with:

``` r
des$opchar
#> # A tibble: 5 x 20
#>    tau1  tau2  tau3   Pdis    Pcon     P1     P2     P3 FWERI1  FWERI2
#>   <dbl> <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#> 1     0     0     0 0.0500 0.00108 0.0196 0.0196 0.0196 0.0500 0.00771
#> 2     1     1     1 0.950  0.611   0.800  0.800  0.800  0      0      
#> 3     1     0     0 0.800  0.00328 0.800  0.0196 0.0196 0.0359 0.00329
#> 4     0     1     0 0.801  0.00328 0.0196 0.800  0.0196 0.0359 0.00329
#> 5     0     0     1 0.800  0.00328 0.0196 0.0196 0.800  0.0359 0.00329
#> # … with 10 more variables: FWERI3 <dbl>, FWERII1 <dbl>, FWERII2 <dbl>,
#> #   FWERII3 <dbl>, PHER <dbl>, FDR <dbl>, pFDR <dbl>, FNDR <dbl>,
#> #   Sens <dbl>, Spec <dbl>
```

Useful plots can then be produced with `plot.multiarm_des_ma()` as
follows:

``` r
plot(des)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-4.png" width="100%" />
