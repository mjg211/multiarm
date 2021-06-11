# multiarm 0.13

* Support added for computing median and modal sample sizes.
* 'Code for reproduction' box added to the GUI.

# multiarm 0.12

* Support added for clinical trials with Poisson distributed outcomes
(`build_dtl_pois()`, `build_gs_pois()`, `build_ss_pois()`, `des_dtl_pois()`,
`des_gs_pois()`, `des_ss_pois()`, `opchar_dtl_pois()`, `opchar_gs_pois()`,
`opchar_ss_pois()`, `plot.multiarm_des_dtl_pois()`,
`plot.multiarm_des_gs_pois()`, `plot.multiarm_des_ss_pois()`, `sim_dtl_pois()`,
`sim_gs_pois()`, `sim_ss_pois()`).

# multiarm 0.11

* Analysis functions removed; focus purely on design for now.
* Support added for group-sequential multi-arm multi-stage clinical trials with
normally or Bernoulli distributed outcomes (`build_gs_bern()`,
`build_gs_norm()`, `des_gs_bern()`,  `des_gs_norm()`, `opchar_gs_bern()`,
`opchar_gs_norm()`, `plot.multiarm_des_gs_bern()`,
`plot.multiarm_des_gs_norm()`, `sim_gs_bern()`, `sim_gs_norm()`).
* Corresponding support also added for multi-stage drop-the-losers designs
(`build_dtl_bern()`, `build_dtl_norm()`, `des_dtl_bern()`, `des_dtl_norm()`,
`opchar_dtl_bern()`, `opchar_dtl_norm()`, `plot.multiarm_des_dtl_bern()`, `plot.multiarm_des_dtl_norm()`, `sim_dtl_bern()`, `sim_dtl_norm()`).
* Suffix of `_norm` added to all relevant functions given below to indicate
outcome variable.
* `_ma_` replaced in function names with `_ss_`.
* `des_int_ma()` removed for simplicity; does not add much over `des_ss_norm()`.

# multiarm 0.10

* Corresponding support added for trials with Bernoulli outcomes
(`an_ma_bern()`, `build_ma_bern()`, `des_ma_bern()`, `opchar_ma()`,
`plot.multiarm_des_ma_bern()`, `sim_ma_bern()`).
* `gui_ma()` renamed `gui()` to reflect use for multiple types of outcome.

# multiarm 0.9

* Package launched with support for the design (`build_ma()`, `des_ma()`,
`des_int_ma()`), assessment (`opchar_ma()`), analysis (`an_ma()`), simulation
(`sim_ma()`), and visualisation (`plot.multiarm_des_ma()`) of single-stage
multi-arm clinical trials with normally distributed outcomes. An R Shiny
graphical user interface is also provided (`gui_ma()`).
