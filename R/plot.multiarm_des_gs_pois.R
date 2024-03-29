#' Plot operating characteristics of a multi-stage group-sequential multi-arm
#' clinical trial for a Poisson distributed primary outcome
#'
#' \code{plot.multiarm_des_gs_pois()} produces power curve plots for a specified
#' multi-stage group-sequential multi-arm clinical trial design assuming the
#' primary outcome is Poisson distributed.
#'
#' @param x A \code{\link{list}} of class \code{"multiarm_des_gs_pois"}, as
#' returned by \code{\link{build_gs_pois}} or \code{\link{des_gs_pois}} (i.e., a
#' multi-stage group-sequential multi-arm clinical trial design for a Poisson
#' distributed outcome). Defaults to \code{des_gs_pois()}.
#' @param delta_min A \code{\link{numeric}} specifying the chosen minimum value
#' for the treatment effects to include on the produced plots. Defaults to
#' \code{-x$delta1}.
#' @param delta_max A \code{\link{numeric}} specifying the chosen maximum
#' value for the treatment effects to include on the produced plots. Defaults to
#' \code{2*x$delta1}.
#' @param delta A \code{\link{numeric}} specifying the chosen treatment effect
#' shift to use in the 'shifted treatment effects plot'. Defaults to
#' \code{x$delta1 - x$delta0}.
#' @param density A \code{\link{numeric}} variable indicating the number of
#' treatment effect scenarios to consider for each power curve. Increasing
#' \code{density} increases the smoothness of the produced plots, at the cost of
#' increased run time. Defaults to \code{100}.
#' @param output A \code{\link{logical}} variable indicating whether the
#' available outputs from the function (see below) should be returned. Defaults
#' to \code{FALSE}.
#' @param print_plots A \code{\link{logical}} variable indicating whether to
#' print produced plots. Defaults to \code{TRUE}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{FALSE}.
#' @param ... Not currently used.
#' @return If \code{output = T}, a list containing the following elements
#' \itemize{
#' \item A \code{\link{list}} in the slot \code{$plots} containing the produced
#' plots.
#' \item Each of the input variables.
#' }
#' @examples
#' \dontrun{
#' # The design for the default parameters
#' des        <- des_gs_pois()
#' plot(des)
#' }
#' @seealso \code{\link{build_gs_pois}}, \code{\link{des_gs_pois}},
#' \code{\link{gui}}, \code{\link{opchar_gs_pois}}, \code{\link{sim_gs_pois}}.
#' @method plot multiarm_des_gs_pois
#' @export
plot.multiarm_des_gs_pois <- function(x = des_gs_pois(),
                                      delta_min = -x$delta1,
                                      delta_max = 2*x$delta1,
                                      delta = x$delta1 - x$delta0,
                                      density = 100, output = FALSE,
                                      print_plots = TRUE, summary = FALSE,
                                      ...) {

  ##### Check input variables ##################################################

  #check_multiarm_des_gs_pois(x, name = "x")
  check_real_range_strict(delta_min, "delta_min", c(-x$lambda0, Inf), 1)
  check_real_range_strict(delta_max, "delta_max", c(-x$lambda0, Inf), 1)
  if (delta_min >= delta_max) {
    stop("delta_min must be strictly less than delta_max")
  }
  check_real_range_strict(delta, "delta", c(0, Inf), 1)
  check_integer_range(density, "density", c(0, Inf), 1)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_plot_multiarm_des_gs(x, delta_min, delta_max, delta, density,
    #                             "pois")
    message("")
  }

  ##### Perform main computations ##############################################

  if (all(summary, output)) {
    message("  Beginning production of plots with equal treatment effects..")
  }
  K                           <- x$K
  alpha                       <- x$alpha
  beta                        <- x$beta
  lambda0                         <- x$lambda0
  delta0                      <- x$delta0
  delta1                      <- x$delta1
  seq_K                       <- 1:K
  plots                       <- list()
  boundaries                  <-
    tibble::tibble(Stage               = rep(1:x$J, 2),
                   Type                = rep(c("Efficacy", "Futility"),
                                             each = x$J),
                   `Stopping boundary` = c(x$e, x$f))
  plots$boundaries            <-
    ggplot2::ggplot(boundaries,
                    ggplot2::aes(x = .data$Stage,
                                 y = .data$`Stopping boundary`,
                                 colour = .data$Type,
                                 by = .data$Type)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("forestgreen", "firebrick2")) +
    ggplot2::scale_x_continuous(breaks = 1:x$J) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title    = ggplot2::element_blank())
  lambda                          <- cbind(rep(lambda0, density), matrix(0, density, K))
  if (all(delta_min < 0, delta_max > 0)) {
    lambda[, 2]                   <- lambda0 + c(seq(delta_min, -1e-6,
                                             length.out = 0.5*density),
                                         seq(1e-6, delta_max,
                                             length.out = 0.5*density))
  } else {
    lambda[, 2]                   <- lambda0 + seq(delta_min, delta_max,
                                           length.out = density)
  }
  for (k in 3:(K + 1)) {
    lambda[, k]                   <- lambda[, 2]
  }
  opchar_equal_og             <- opchar_equal <- opchar_gs_pois(x, lambda)$opchar
  opchar_equal                <-
    tidyr::pivot_longer(opchar_equal, .data$`Pdis`:.data$MoSS,
                        names_to = "type", values_to = "P")
  opchar_equal$type           <- factor(opchar_equal$type,
                                        c("Pdis", "Pcon", paste0("P", seq_K),
                                          paste0("FWERI", seq_K),
                                          paste0("FWERII", seq_K), "PHER",
                                          "FDR", "pFDR", "FNDR", "Sens",
                                          "Spec", "ESS", "SDSS", "MeSS",
                                          "MoSS"))
  labels_power                <- numeric(K + 2)
  labels_power[1:2]           <- c(parse(text = "italic(P)[dis]"),
                                   parse(text = "italic(P)[con]"))
  for (i in 3:(K + 2)) {
    labels_power[i]           <- parse(text = paste("italic(P)[", i - 2, "]",
                                                    sep = ""))
  }
  colours_power               <- ggthemes::ptol_pal()(K + 2)
  plots$equal_power           <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal, .data$type %in% c("Pdis", "Pcon",
                                                           paste0("P", seq_K))),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power,
                                 labels = labels_power) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], sep = "")))
  } else if (K == 3) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], " = ", lambda[3], sep = "")))
  } else {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ... = ",
                                 lambda[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_power)
  }
  labels_error                <- numeric(2*K + 1)
  for (i in seq_K) {
    labels_error[i]           <-
      parse(text = paste("italic(FWER)[italic(I)][", i, "]", sep = ""))
    labels_error[K + i]       <-
      parse(text = paste("italic(FWER)[italic(II)][", i, "]", sep = ""))
  }
  labels_error[2*K + 1]       <- parse(text = "italic(PHER)")
  colours_error               <- ggthemes::ptol_pal()(2*K + 1)
  plots$equal_error           <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                              paste0("FWERII", seq_K), "PHER")) &
                             (.data$lambda1 <= .data$lambda0)),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                              paste0("FWERII", seq_K), "PHER")) &
                             (.data$lambda1 > .data$lambda0)),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_error,
                                 labels = labels_error) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], sep = "")))
  } else if (K == 3) {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], " = ", lambda[3], sep = "")))
  } else {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ... = ",
                                 lambda[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_error)
  }
  labels_other                 <- c(parse(text = "italic(FDR)"),
                                    parse(text = "italic(pFDR)"),
                                    parse(text = "italic(FNDR)"),
                                    parse(text = "italic(Sensitivity)"),
                                    parse(text = "italic(Specificity)"))
  colours_other                <- ggthemes::ptol_pal()(5)
  plots$equal_other            <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                              "Spec")) & (.data$lambda1 <= .data$lambda0)),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                              "Spec")) & (.data$lambda1 > .data$lambda0)),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_other,
                                 labels = labels_other) +
    ggplot2::ylab("Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], sep = "")))
  } else if (K == 3) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], " = ", lambda[3], sep = "")))
  } else {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ... = ",
                                 lambda[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_other)
  }
  labels_ss                    <- c(parse(text = "italic(ESS)"),
                                    parse(text = "italic(SDSS)"),
                                    parse(text = "italic(MeSS)"),
                                    parse(text = "italic(MoSS)"))
  colours_ss                   <- ggthemes::ptol_pal()(4)
  plots$equal_sample_size      <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("ESS", "SDSS", "MeSS", "MoSS"))),
      ggplot2::aes(.data$lambda1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_ss,
                                 labels = labels_ss) +
    ggplot2::ylab("Value") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_vline(xintercept = lambda0, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_sample_size         <- plots$equal_sample_size +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], sep = "")))
  } else if (K == 3) {
    plots$equal_sample_size         <- plots$equal_sample_size +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                                 lambda[2], " = ", lambda[3], sep = "")))
  } else {
    plots$equal_sample_size         <- plots$equal_sample_size +
      ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ... = ",
                                 lambda[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_sample_size)
  }
  if (all(summary, output)) {
    message("..completed production of plots with equal treatment effects.")
    message("  Beginning production of plots with shifted treatment effects..")
  }
  lambda                        <- cbind(rep(lambda0, density), matrix(0, density, K))
  lambda[, 2]                   <- lambda0 + seq(delta_min, delta_max,
                                         length.out = density)
  for (l in seq_K[-1]) {
    lambda[, l + 1]             <- lambda[, 2] - delta
  }
  lambda                        <- lambda[as.logical(apply(lambda >= 0, 1, prod)), ]
  opchar_1                    <- opchar_gs_pois(x, lambda)$opchar
  opchar_shifted_og           <- opchar_shifted <- opchar_1
  opchar_shifted              <- tidyr::gather(opchar_shifted_og, "type",
                                               "Value", .data$P1:.data$MoSS)
  opchar_shifted$type         <- factor(opchar_shifted$type,
                                        c("P1", "ESS", "SDSS", "MeSS", "MoSS"))
  plots$shifted_power         <- ggplot2::ggplot() +
    ggplot2::geom_line(data = dplyr::filter(opchar_shifted, .data$type == "P1"),
                       ggplot2::aes(.data$lambda1, .data$Value)) +
    ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                               lambda[2], " + ", .(delta), " = ... = ",
                               lambda[italic(K)], " + ", .(delta), sep = ""))) +
    ggplot2::ylab(expression(paste(italic(P), "(Reject ", italic(H)[1], " | ",
                                   lambda, ")", sep = ""))) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (print_plots) {
    print(plots$shifted_power)
  }
  plots$shifted_sample_size      <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_shifted, .data$type %in% c("ESS", "SDSS",
                                                             "MeSS", "MoSS")),
      ggplot2::aes(.data$lambda1, .data$Value, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_ss,
                                 labels = labels_ss) +
    ggplot2::xlab(bquote(paste(lambda[0], " = ", .(lambda0), ", ", lambda[1], " = ",
                               lambda[2], " + ", .(delta), " = ... = ",
                               lambda[italic(K)], " + ", .(delta), sep = ""))) +
    ggplot2::ylab("Value") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_vline(xintercept = lambda0 + delta1, linetype = 2)
  if (print_plots) {
    print(plots$shifted_sample_size)
  }
  if (all(summary, output)) {
    message("..completed production of plots with shifted treatment effects.")
    message("  Beginning production of sample size PMF plot..")
  }
  pmf_N <- opchar_gs_pois(x, lambda0 + rbind(c(0, rep(delta0, K)),
                                         c(0, rep(delta1, K)),
                                         c(0, delta1,
                                           rep(delta0, K - 1))))$pmf_N
  pmf_N <- dplyr::mutate(pmf_N,
                         Scenario = factor(rep(c("HG", "HA", "LFC1"),
                                               each = nrow(pmf_N)/3),
                                           levels = c("HG", "HA", "LFC1"),
                                           labels = c("italic(H[G])",
                                                      "italic(H[A])",
                                                      "italic(LFC)[1]")))
  pmf_N <- dplyr::mutate(pmf_N, n = factor(round(.data$n, 1)))
  plots$pmf_N <- ggplot2::ggplot(data = pmf_N,
                                 mapping = ggplot2::aes(.data$n, .data$Prob)) +
    ggplot2::geom_bar(stat = "identity", fill = "black", colour = "black") +
    ggplot2::facet_wrap(.~Scenario, nrow = 1, ncol = 3,
                        labeller = ggplot2::label_parsed) +
    ggplot2::xlab(expression(italic(n))) +
    ggplot2::ylab(expression(paste(italic(P), "(", italic(N), " = ", italic(n),
                                   ")", sep = ""))) +
    ggplot2::theme_bw() + ggplot2::ylim(0, 1)
  if (print_plots) {
    print(plots$pmf_N)
  }

  ##### Outputting #############################################################

  if (all(summary, output)) {
    message("..completed production of sample size PMF plot.")
    message("  Preparing for outputting..")
    message("..outputting.")
  }
  if (output) {
    list(density = density,
         opchar  = rbind(opchar_equal_og, opchar_shifted_og),
         output  = output,
         plots   = plots,
         summary = summary,
         x       = x)
  }

}
