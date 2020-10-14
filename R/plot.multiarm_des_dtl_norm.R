#' Plot operating characteristics of a multi-stage drop-the-losers multi-arm
#' clinical trial for a normally distributed primary outcome
#'
#' \code{plot.multiarm_des_dtl_norm()} produces power curve plots for a
#' specified multi-stage drop-the-losers multi-arm clinical trial design
#' assuming the primary outcome is normally distributed.
#'
#' @param x A \code{\link{list}} of class \code{"multiarm_des_dtl_norm"}, as
#' returned by \code{\link{build_dtl_norm}} or \code{\link{des_dtl_norm}} (i.e.,
#' a multi-stage drop-the-losers multi-arm clinical trial design for a normally
#' distributed outcome). Defaults to \code{des_dtl_norm()}.
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
#' des        <- des_dtl_norm()
#' plot(des)
#' }
#' @seealso \code{\link{build_dtl_norm}}, \code{\link{des_dtl_norm}},
#' \code{\link{gui}}, \code{\link{opchar_dtl_norm}}, \code{\link{sim_dtl_norm}}.
#' @method plot multiarm_des_dtl_norm
#' @export
plot.multiarm_des_dtl_norm <- function(x = des_dtl_norm(),
                                       delta_min = -x$delta1,
                                       delta_max = 2*x$delta1,
                                       delta = x$delta1 - x$delta0,
                                       density = 100, output = FALSE,
                                       print_plots = TRUE, summary = FALSE,
                                       ...) {

  ##### Check input variables ##################################################

  #check_multiarm_des_dtl_norm(x, name = "x")
  check_real_range_strict(delta_min, "delta_min", c(-Inf, Inf), 1)
  check_real_range_strict(delta_max, "delta_max", c(-Inf, Inf), 1)
  if (delta_min >= delta_max) {
    stop("delta_min must be strictly less than delta_max")
  }
  check_real_range_strict(delta, "delta", c(0, Inf), 1)
  check_integer_range(density, "density", c(0, Inf), 1)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_plot_multiarm_des_dtl(x, delta_min, delta_max, delta, density,
    #                              "norm")
    message("")
  }

  ##### Perform main computations ##############################################

  if (all(summary, output)) {
    message("  Beginning production of plots with equal treatment effects..")
  }
  K                           <- x$Kv[1]
  alpha                       <- x$alpha
  beta                        <- x$beta
  delta0                      <- x$delta0
  delta1                      <- x$delta1
  seq_K                       <- 1:K
  plots                       <- list()
  tau                         <- matrix(0, density, K)
  if (all(delta_min < 0, delta_max > 0)) {
    tau[, 1]                  <- c(seq(delta_min, -1e-6,
                                       length.out = 0.5*density),
                                   seq(1e-6, delta_max,
                                       length.out = 0.5*density))
  } else {
    tau[, 1]                  <- seq(delta_min, delta_max, length.out = density)
  }
  for (k in 2:K) {
    tau[, k]                  <- tau[, 1]
  }
  opchar_equal_og             <- opchar_equal <- opchar_dtl_norm(x, tau)$opchar
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
      ggplot2::aes(.data$tau1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power,
                                 labels = labels_power) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta1, linetype = 2)
  if (K == 2) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
  } else if (K == 3) {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                     sep = "")))
  } else {
    plots$equal_power         <- plots$equal_power +
      ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
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
                             (.data$tau1 <= 0)),
      ggplot2::aes(.data$tau1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                              paste0("FWERII", seq_K), "PHER")) &
                             (.data$tau1 > 0)),
      ggplot2::aes(.data$tau1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_error,
                                 labels = labels_error) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta0, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta1, linetype = 2)
  if (K == 2) {
    plots$equal_error          <- plots$equal_error +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
  } else if (K == 3) {
    plots$equal_error          <- plots$equal_error +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                     sep = "")))
  } else {
    plots$equal_error          <- plots$equal_error +
      ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
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
                                              "Spec")) & (.data$tau1 <= 0)),
      ggplot2::aes(.data$tau1, .data$P, col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                              "Spec")) & (.data$tau1 > 0)),
      ggplot2::aes(.data$tau1, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_other,
                                 labels = labels_other) +
    ggplot2::ylab("Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta0, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta1, linetype = 2)
  if (K == 2) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
  } else if (K == 3) {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                     sep = "")))
  } else {
    plots$equal_other         <- plots$equal_other +
      ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
  }
  if (print_plots) {
    print(plots$equal_other)
  }
  if (all(summary, output)) {
    message("..completed production of plots with equal treatment effects.")
    message("  Beginning production of plots with shifted treatment effects..")
  }
  opchar_matrix               <- NULL
  tau_init                    <- matrix(seq(-delta1, 2*delta1,
                                            length.out = density) - delta,
                                        density, K)
  tau                         <- tau_init
  tau[, 1]                    <- tau[, 1] + delta
  opchar_1                    <- opchar_dtl_norm(x, tau)$opchar
  opchar_shifted_og           <- opchar_shifted <- opchar_1
  plots$shifted_power         <- ggplot2::ggplot() +
    ggplot2::geom_line(data = opchar_shifted,
                       ggplot2::aes(.data$tau1, .data$P1)) +
    ggplot2::xlab(bquote(paste(tau[1], " = ", tau[2], " + ",
                               .(delta), " = ... = ", tau[italic(K)],
                               " + ", .(delta), sep = ""))) +
    ggplot2::ylab(expression(paste(italic(P), "(Reject ", italic(H)[1], " | ",
                                   tau, ")", sep = ""))) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = alpha, linetype = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta0, linetype = 2) +
    ggplot2::geom_vline(xintercept = delta1, linetype = 2)
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
