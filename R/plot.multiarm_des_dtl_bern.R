#' @method plot multiarm_des_dtl_bern
#' @export
plot.multiarm_des_dtl_bern <- function(x = des_dtl_bern(),
                                       delta_min = -x$pi0 + 1e-6,
                                       delta_max = 1 - x$pi0 - 1e-6,
                                       delta = x$delta1 - x$delta0,
                                       density = 100, output = F,
                                       print_plots = T, summary = F, ...) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_bern(x, name = "x")
  check_real_range_strict(delta_min, "delta_min", c(-x$pi0, 1 - x$pi0), 1)
  check_real_range_strict(delta_max, "delta_max", c(-x$pi0, 1 - x$pi0), 1)
  if (delta_min >= delta_max) {
    stop("delta_min must be strictly less than delta_max")
  }
  check_real_range_strict(delta, "delta", c(0, 1 - x$pi0), 1)
  check_integer_range(density, "density", c(0, Inf), 1)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_plot_multiarm_des_dtl(x, delta_min, delta_max, delta, density,
    #                             "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (all(summary, output)) {
    message("  Beginning production of plots with equal treatment effects..")
  }
  K                           <- x$Kv[1]
  alpha                       <- x$alpha
  beta                        <- x$beta
  pi0                         <- x$pi0
  delta0                      <- x$delta0
  delta1                      <- x$delta1
  seq_K                       <- 1:K
  plots                       <- list()
  pi                          <- cbind(rep(pi0, density), matrix(0, density, K))
  if (all(delta_min < 0, delta_max > 0)) {
    pi[, 2]                   <- pi0 + c(seq(delta_min, -1e-6,
                                             length.out = 0.5*density),
                                         seq(1e-6, delta_max,
                                             length.out = 0.5*density))
  } else {
    pi[, 2]                   <- pi0 + seq(delta_min, delta_max,
                                           length.out = density)
  }
  for (k in 3:(K + 1)) {
    pi[, k]                   <- pi[, 2]
  }
  opchar_equal_og             <- opchar_equal <- opchar_dtl_bern(x, pi)$opchar
  opchar_equal                <-
    tidyr::pivot_longer(opchar_equal, .data$`Pdis`:.data$Spec,
                        names_to = "type", values_to = "P")
  opchar_equal$type           <- factor(opchar_equal$type,
                                        c("Pdis", "Pcon", paste0("P", seq_K),
                                          paste0("FWERI", seq_K),
                                          paste0("FWERII", seq_K), "PHER",
                                          "FDR", "pFDR", "FNDR", "Sens",
                                          "Spec"))
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
      ggplot2::aes(.data$pi1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power,
                                 labels = labels_power) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], sep = "")))
  } else if (K == 3) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(K)], sep = "")))
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
                             (.data$pi1 <= .data$pi0)),
      ggplot2::aes(.data$pi1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                              paste0("FWERII", seq_K), "PHER")) &
                             (.data$pi1 > .data$pi0)),
      ggplot2::aes(.data$pi1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_error,
                                 labels = labels_error) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], sep = "")))
  } else if (K == 3) {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$equal_error         <- plots$equal_error +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(K)], sep = "")))
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
                                              "Spec")) & (.data$pi1 <= .data$pi0)),
      ggplot2::aes(.data$pi1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                              "Spec")) & (.data$pi1 > .data$pi0)),
      ggplot2::aes(.data$pi1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_other,
                                 labels = labels_other) +
    ggplot2::ylab("Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1, linetype = 2)
  if (K == 2) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], sep = "")))
  } else if (K == 3) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_other)
  }
  if (all(summary, output)) {
    message("..completed production of plots with equal treatment effects.")
    message("  Beginning production of plots with shifted treatment effects..")
  }
  pi                        <- cbind(rep(pi0, density), matrix(0, density, K))
  pi[, 2]                   <- pi0 + seq(delta_min, delta_max,
                                         length.out = density)
  for (l in seq_K[-1]) {
    pi[, l + 1]             <- pi[, 2] - delta
  }
  pi                        <- pi[as.logical(apply(pi >= 0, 1, prod)), ]
  opchar_1                    <- opchar_dtl_bern(x, pi)$opchar
  opchar_shifted_og           <- opchar_shifted <- opchar_1
  plots$shifted_power         <- ggplot2::ggplot() +
    ggplot2::geom_line(data = opchar_shifted,
                       ggplot2::aes(.data$pi1, .data$P1)) +
    ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                               pi[2], " + ", .(delta), " = ... = ",
                               pi[italic(K)], " + ", .(delta), sep = ""))) +
    ggplot2::ylab(expression(paste(italic(P), "(Reject ", italic(H)[1], " | ",
                                   pi, ")", sep = ""))) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0, linetype = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1, linetype = 2)
  if (print_plots) {
    print(plots$shifted_power)
  }
  if (all(summary, output)) {
    message("..completed production of plots with shifted treatment effects.")
  }

  ##### Outputting #############################################################

  if (all(summary, output)) {
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
