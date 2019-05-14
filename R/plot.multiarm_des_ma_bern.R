#' Plot operating characteristics of a fixed-sample multi-arm clinical trial
#' for a Bernoulli distributed primary outcome
#'
#' \code{plot.multiarm_des_ma_bern()} produces power curve plots for a specified
#' fixed-sample multi-arm clinical trial design assuming the primary outcome is
#' Bernoulli distributed.
#'
#' @param x A \code{\link{list}} of class \code{"multiarm_des_ma_bern"}, as
#' returned by \code{\link{build_ma_bern}} or \code{\link{des_ma_bern}} (i.e., a
#' fixed-sample multi-arm clinical trial design for a Bernoulli distributed
#' outcome). Defaults to \code{des_ma_bern()}.
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
#' to \code{F}.
#' @param summary A \code{\link{logical}} variable indicating whether a summary
#' of the function's progress should be printed to the console. Defaults to
#' \code{F}.
#' @param ... Not currently used.
#' @return If \code{output = T}, a list containing the following elements
#' \itemize{
#' \item A \code{\link{list}} in the slot \code{$plots} containing the produced
#' plots.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' \dontrun{
#' # The design for the default parameters
#' des        <- des_ma_bern()
#' plot(des)
#' # An A-optimal design, returning the avaiable outputs from
#' # plot.multiarm_des_ma_bern()
#' des_A      <- des_ma_bern(ratio = "A")
#' plots      <- plot(des_A, output = TRUE)
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ma_bern(ratio      = rep(1/sqrt(2), 2),
#'                           correction = "holm_bonferroni",
#'                           power      = "disjunctive")
#' plot(des_root_K)
#' }
#' @seealso \code{\link{build_ma_bern}}, \code{\link{des_ma_bern}},
#' \code{\link{gui}}, \code{\link{opchar_ma_bern}},
#' \code{\link{plot.multiarm_des_ma_bern}}, \code{\link{sim_ma_bern}}.
#' @export
plot.multiarm_des_ma_bern <- function(x = des_ma_bern(),
                                      delta_min = -x$pi0 + 1e-6,
                                      delta_max = 1 - x$pi0 - 1e-6,
                                      delta = x$delta1 - x$delta0,
                                      density = 100, output = F, summary = F,
                                      ...) {

  ##### Check input variables ##################################################

  if (missing(x)) {
    des <- x <- des_ma_bern()
  } else {
    des <- x
    check_multiarm_des_ma_bern(des)
  }
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
    summary_plot_multiarm_des_ma(des, delta_min, delta_max, delta, density,
                                 "bernoulli")
    message("")
  }

  ##### Perform main computations ##############################################

  seq_K                    <- 1:des$K
  plots                    <- list()
  pi                       <- cbind(rep(des$pi0, density),
                                    matrix(0, nrow = density, ncol = des$K))
  if (all(delta_min < 0, delta_max > 0)) {
    pi[, 2]                <- des$pi0 + c(seq(delta_min, -1e-6,
                                              length.out = 0.5*density),
                                          seq(1e-6, delta_max,
                                              length.out = 0.5*density))
  } else {
    pi[, 2]                <- des$pi0 + seq(delta_min, delta_max,
                                            length.out = density)
  }
  for (k in 3:(des$K + 1)) {
    pi[, k]                <- pi[, 2]
  }
  opchar_equal             <- opchar_ma_bern(des, pi)$opchar
  opchar_equal             <- tidyr::gather(opchar_equal, "type", "P",
                                            .data$`Pdis`:.data$`Spec`)
  opchar_equal$type        <- factor(opchar_equal$type,
                                     c("Pdis", "Pcon",
                                       paste0("P", seq_K),
                                       paste0("FWERI", seq_K),
                                       paste0("FWERII", seq_K), "PHER", "FDR",
                                       "pFDR", "FNDR", "Sens", "Spec"))
  labels_power             <- numeric(des$K + 2)
  labels_power[1:2]        <- c(parse(text = "italic(P)[dis]"),
                                parse(text = "italic(P)[con]"))
  for (i in 3:(des$K + 2)) {
    labels_power[i]        <- parse(text = paste("italic(P)[", i - 2, "]",
                                                 sep = ""))
  }
  colours_power            <- ggthemes::ptol_pal()(2 + des$K)
  alpha                    <- des$alpha
  beta                     <- des$beta
  pi0                      <- des$pi0
  delta0                   <- des$delta0
  delta1                   <- des$delta1
  plots$plot_equal_power   <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal, .data$type %in% c("Pdis", "Pcon",
                                                     paste0("P", seq_K))),
      ggplot2::aes(x   = .data$pi1,
                   y   = .data$P,
                   col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power,
                                 labels = labels_power) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1,
                        linetype   = 2)
  if (des$K == 2) {
    plots$plot_equal_power <- plots$plot_equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], sep = "")))
  } else if (des$K == 3) {
    plots$plot_equal_power <- plots$plot_equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$plot_equal_power <- plots$plot_equal_power +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(des$K)], sep = "")))
  }
  print(plots$plot_equal_power)

  labels_er                <- numeric(2*des$K + 1)
  for (i in 1:des$K) {
    labels_er[i]           <-
      parse(text = paste("italic(FWER)[italic(I)][", i, "]", sep = ""))
    labels_er[des$K + i]   <-
      parse(text = paste("italic(FWER)[italic(II)][", i, "]", sep = ""))
  }
  labels_er[2*des$K + 1]   <- parse(text = "italic(PHER)")
  colours_er               <- ggthemes::ptol_pal()(2*des$K + 1)
  plots$plot_equal_er      <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                        paste0("FWERII", seq_K), "PHER")) &
                             (.data$pi1 <= .data$pi0)),
      ggplot2::aes(x   = .data$pi1,
                   y   = .data$P,
                   col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c(paste0("FWERI", seq_K),
                                        paste0("FWERII", seq_K), "PHER")) &
                             (.data$pi1 > .data$pi0)),
      ggplot2::aes(x   = .data$pi1,
                   y   = .data$P,
                   col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_er, labels = labels_er) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1,
                        linetype   = 2)
  if (des$K == 2) {
    plots$plot_equal_er    <- plots$plot_equal_er +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], sep = "")))
  } else if (des$K == 3) {
    plots$plot_equal_er    <- plots$plot_equal_er +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$plot_equal_er    <- plots$plot_equal_er +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(des$K)], sep = "")))
  }
  print(plots$plot_equal_er)

  labels_other             <- c(parse(text = "italic(FDR)"),
                                parse(text = "italic(pFDR)"),
                                parse(text = "italic(FNDR)"),
                                parse(text = "italic(Sensitivity)"),
                                parse(text = "italic(Specificity)"))
  colours_other            <- ggthemes::ptol_pal()(5)
  plots$plot_equal_other   <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                        "Spec")) & (.data$pi1 <= .data$pi0)),
      ggplot2::aes(x   = .data$pi1,
                   y   = .data$P,
                   col = .data$type)) +
    ggplot2::geom_line(
      data = dplyr::filter(opchar_equal,
                           (.data$type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                        "Spec")) & (.data$pi1 > .data$pi0)),
      ggplot2::aes(x   = .data$pi1,
                   y   = .data$P,
                   col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_other,
                                 labels = labels_other) +
    ggplot2::ylab("Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1,
                        linetype   = 2)
  if (des$K == 2) {
    plots$plot_equal_other <- plots$plot_equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], sep = "")))
  } else if (des$K == 3) {
    plots$plot_equal_other <- plots$plot_equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                 pi[2], " = ", pi[3], sep = "")))
  } else {
    plots$plot_equal_other <- plots$plot_equal_other +
      ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                 pi[.(des$K)], sep = "")))
  }
  print(plots$plot_equal_other)

  opchar_matrix            <- NULL
  for (k in 1:des$K) {
    pi                     <- cbind(rep(pi0, density),
                                    matrix(0, nrow = density, ncol = des$K))
    pi[, k + 1]            <- pi0 + seq(delta_min, delta_max,
                                        length.out = density)
    for (l in (1:des$K)[-k]) {
      pi[, l + 1]          <- pi[, k + 1] - delta
    }
    pi                     <- pi[as.logical(apply(pi >= 0, 1, prod)), ]
    opchar_k               <- opchar_ma_bern(des, pi)$opchar
    opchar_matrix          <- rbind(opchar_matrix,
                                    as.matrix(opchar_k[, c(k + 1,
                                                           des$K + 3 + k)]))
  }
  opchar_shifted           <- tibble::as_tibble(opchar_matrix)
  colnames(opchar_shifted) <- c("pik", "P")
  opchar_shifted           <-
    dplyr::mutate(opchar_shifted,
                  type = factor(rep(paste0("P", 1:des$K),
                                    each = nrow(opchar_shifted)/des$K)))
  labels_shifted           <- numeric(des$K)
  for (i in 1:des$K) {
    labels_shifted[i]      <- parse(text = paste("italic(P)[", i, "]", sep = ""))
  }
  plots$plot_shifted       <- ggplot2::ggplot() +
    ggplot2::geom_line(data = opchar_shifted,
                       ggplot2::aes(x   = .data$pik,
                                    y   = .data$P,
                                    col = .data$type)) +
    ggplot2::scale_colour_manual(values = colours_power[-(1:2)],
                                 labels = labels_shifted) +
    ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ... = ",
                               pi[italic(k)-1], " + ", .(delta), " = ",
                               pi[italic(k)], " = ", pi[italic(k)+1],
                               " + ", .(delta), " = ... ", sep = ""))) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = pi0 + delta1,
                        linetype   = 2)
  print(plots$plot_shifted)

  ##### Outputting #############################################################

  if (all(summary, output)) {
    message(uc("two_elip"), "outputting.")
  }
  if (output) {
    list(density = density,
         output  = output,
         plots   = plots,
         summary = summary,
         x       = x)
  }
}
