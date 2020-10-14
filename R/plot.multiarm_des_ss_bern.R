#' Plot operating characteristics of a single-stage multi-arm clinical trial
#' for a Bernoulli distributed primary outcome
#'
#' \code{plot.multiarm_des_ss_bern()} produces power curve plots for a specified
#' single-stage multi-arm clinical trial design assuming the primary outcome is
#' Bernoulli distributed.
#'
#' @param x A \code{\link{list}} of class \code{"multiarm_des_ss_bern"}, as
#' returned by \code{\link{build_ss_bern}} or \code{\link{des_ss_bern}} (i.e., a
#' single-stage multi-arm clinical trial design for a Bernoulli distributed
#' outcome). Defaults to \code{des_ss_bern()}.
#' @param delta_min A \code{\link{numeric}} specifying the chosen minimum value
#' for the treatment effects to include on the produced plots. Defaults to
#' \code{-x$pi0 + 1e-6}.
#' @param delta_max A \code{\link{numeric}} specifying the chosen maximum
#' value for the treatment effects to include on the produced plots. Defaults to
#' \code{1 - x$pi0 - 1e-6}.
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
#' des        <- des_ss_bern()
#' plot(des)
#' # An A-optimal design, returning the avaiable outputs from
#' # plot.multiarm_des_ss_bern()
#' des_A      <- des_ss_bern(ratio = "A")
#' plots      <- plot(des_A, output = T)
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ss_bern(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' plot(des_root_K)
#' }
#' @seealso \code{\link{build_ss_bern}}, \code{\link{des_ss_bern}},
#' \code{\link{gui}}, \code{\link{opchar_ss_bern}}, \code{\link{sim_ss_bern}}.
#' @method plot multiarm_des_ss_bern
#' @export
plot.multiarm_des_ss_bern <- function(x = des_ss_bern(),
                                      delta_min = -x$pi0 + 1e-6,
                                      delta_max = 1 - x$pi0 - 1e-6,
                                      delta = x$delta1 - x$delta0,
                                      density = 100, output = FALSE,
                                      print_plots = TRUE, summary = FALSE,
                                      ...) {

  ##### Check input variables ##################################################

  check_multiarm_des_ss_bern(x, name = "x")
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
    summary_plot_multiarm_des_ss(x, delta_min, delta_max, delta, density,
                                 "bern")
    message("")
  }

  ##### Perform main computations ##############################################

  if (all(summary, output)) {
    message("  Beginning production of plots with equal treatment effects..")
  }
  K                           <- x$K
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
  opchar_equal                <- opchar_ss_bern(x, pi)$opchar
  opchar_equal_og             <- opchar_equal
  opchar_equal                <- tidyr::gather(opchar_equal, "type", "P",
                                               .data$`Pdis`:.data$`Spec`)
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
  labels_other                <- c(parse(text = "italic(FDR)"),
                                   parse(text = "italic(pFDR)"),
                                   parse(text = "italic(FNDR)"),
                                   parse(text = "italic(Sensitivity)"),
                                   parse(text = "italic(Specificity)"))
  colours_other               <- ggthemes::ptol_pal()(5)
  plots$equal_other           <- ggplot2::ggplot() +
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
  opchar_matrix               <- NULL
  for (k in seq_K) {
    pi                        <- cbind(rep(pi0, density), matrix(0, density, K))
    pi[, k + 1]               <- pi0 + seq(delta_min, delta_max,
                                           length.out = density)
    for (l in seq_K[-k]) {
      pi[, l + 1]             <- pi[, k + 1] - delta
    }
    pi                        <- pi[as.logical(apply(pi >= 0, 1, prod)), ]
    opchar_k                  <- opchar_ss_bern(x, pi)$opchar
    opchar_matrix             <-
      rbind(opchar_matrix, cbind(as.matrix(opchar_k[, c(k + 1, K + 3 + k)]),
                                 as.matrix(opchar_k)))
  }
  opchar_shifted_og           <- tibble::as_tibble(opchar_matrix,
                                                   .name_repair = "minimal")
  opchar_shifted              <- opchar_shifted_og[, 1:2]
  colnames(opchar_shifted)    <- c("pik", "P")
  opchar_shifted_og           <- opchar_shifted_og[, -(1:2)]
  colnames(opchar_shifted_og) <- colnames(opchar_equal_og)
  opchar_shifted              <-
    dplyr::mutate(opchar_shifted,
                  type = factor(rep(paste0("P", seq_K),
                                    each = nrow(opchar_shifted)/K)))
  labels_shifted              <- numeric(K)
  for (i in seq_K) {
    labels_shifted[i]         <- parse(text = paste("italic(P)[", i, "]",
                                                    sep = ""))
  }
  plots$shifted_power         <- ggplot2::ggplot() +
    ggplot2::geom_line(data = opchar_shifted,
                       ggplot2::aes(.data$pik, .data$P, col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power[-(1:2)],
                                 labels = labels_shifted) +
    ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ... = ",
                               pi[italic(k) - 1], " + ", .(delta), " = ",
                               pi[italic(k)], " = ", pi[italic(k) + 1],
                               " + ", .(delta), " = ... ", sep = ""))) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
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
