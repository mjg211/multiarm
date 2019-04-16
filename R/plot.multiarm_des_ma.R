#' Plot operating characteristics of a fixed-sample multi-arm clinical trial
#'
#' \code{plot.multiarm_des_ma()} produces power curve plots for a specified
#' fixed-sample multi-arm clinical trial design.
#'
#' @param x A \code{\link{list}} of class \code{"multiarm_des_ma"}, as
#' returned by \code{\link{build_ma}}, \code{\link{des_ma}}, or
#' \code{\link{des_int_ma}} (i.e., a fixed-sample multi-arm clinical trial
#' design). Defaults to \code{des_ma()}.
#' @param delta_min A \code{\link{numeric}} specifying the chosen minimum value
#' for the treatment effects to include on the produced plots. Defaults to
#' \code{-x$delta1}.
#' @param delta_max A \code{\link{numeric}} specifying the chosen maximum
#' value for the treatment effects to include on the produced plots. Defaults to
#' \code{2*x$delta1}.
#' @param delta A \code{\link{numeric}} specifying the chosen treatment effect
#' shift to use the 'shifted treatment effects plot'. Defaults to
#' \code{delta_max - delta_min}.
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
#' @return If \code{output = T}, a list containing the following elements
#' \itemize{
#' \item A \code{\link{list}} in the slot \code{$plots} containing the produced
#' plots.
#' \item Each of the input variables, subject to possible internal modification.
#' }
#' @examples
#' # The design for the default parameters
#' des        <- des_ma()
#' plot(des)
#' # An A-optimal design, returning the avaiable outputs from
#' # plot.multiarm_des_ma()
#' des_A      <- des_ma(ratio = "A")
#' plots      <- plot(des_A, output = T)
#' # Using the root-K allocation rule, modifying the desired type of power, and
#' # choosing an alternative multiple comparison correction
#' des_root_K <- des_ma(ratio      = rep(1/sqrt(2), 2),
#'                      correction = "holm",
#'                      power      = "disjunctive")
#' plot(des_root_K)
#' @seealso \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{gui_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}, and their
#' associated S3 \code{\link{print}} and \code{\link{summary}} generics.
#' @export
plot.multiarm_des_ma <- function(x = des_ma(), delta_min = -x$delta1,
                                 delta_max = 2$delta1,
                                 delta = delta_max - delta_min, density = 100,
                                 output = F, summary = F, ...) {

  ##### Check input variables ##################################################

  if (missing(x)) {
    des <- x <- des_ma()
  } else {
    des <- x
    check_multiarm_des_ma(des)
  }
  if (missing(delta_min)) {
    delta_min <- -des$delta1
  } else {
    check_real_range_strict(delta_min, "delta_min", c(-Inf, Inf), 1)
  }
  if (missing(delta_max)) {
    delta_max <- 2*des$delta1
  } else {
    check_real_range_strict(delta_max, "delta_max", c(-Inf, Inf), 1)
  }
  if (delta_min >= delta_max) {
    stop("delta_min must be strictly less than delta_max")
  }
  if (missing(delta)) {
    delta <- des$delta1 - des$delta0
  } else {
    check_real_range_strict(delta, "delta", c(-Inf, Inf), 1)
  }
  check_integer_range(density, "density", c(0, Inf), 1)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    #summary_plot_multiarm_des_ma(des, delta_min, delta_max, delta, density)
    message("")
  }

  ##### Perform main computations ##############################################

  plots            <- list()
  tau              <- matrix(0, nrow = density, ncol = des$K)
  if (all(delta_min < 0, delta_max > 0)) {
    tau[, 1]       <- c(seq(delta_min, -1e-6, length.out = 0.5*density),
                        seq(1e-6, delta_max, length.out = 0.5*density))
  } else {
    tau[, 1]       <- seq(delta_min, delta_max, length.out = density)
  }
  for (k in 2:des$K) {
    tau[, k]       <- tau[, 1]
  }
  opchar_global <- opchar_ma(des, tau)$opchar
  opchar_global <- tidyr::gather(opchar_global, "type", "P",
                                 .data$`Pdis`:.data$`FWER`)
  opchar_global$type <- factor(opchar_global$type,
                               levels = c(paste("P", 1:des$K, sep = ""),
                                          "Pcon", "Pdis", "FWER"))
  labels <- numeric(des$K + 3L)
  labels[1] <- parse(text = paste("italic(FWER)", "/", "italic(FDR)", sep = ""))
  for (i in 2:(des$K + 1L)) {
    labels[i] <- parse(text = paste("italic(P)[", i - 1L, "]", sep = ""))
  }
  labels[(des$K + 2L):(des$K + 3L)] <- c(parse(text = "italic(P)[con]"),
                                 parse(text = "italic(P)[dis]"))
  colours <- ggthemes::ptol_pal()(3 + des$K)
  alpha <- des$alpha
  beta  <- des$beta
  delta0 <- des$delta0
  delta1 <- des$delta1
  plots$plot_global <- ggplot2::ggplot() +
    ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                            !(type %in% c("FWER"))),
                       ggplot2::aes(x   = tau1,
                                    y   = P,
                                    col = type)) +
    ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                            (type %in% c("FWER")) &
                                              tau1 <= 0),
                       ggplot2::aes(x   = tau1,
                                    y   = P,
                                    col = type)) +
    ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                            (type %in% c("FWER")) &
                                              tau1 > 0),
                       ggplot2::aes(x   = tau1,
                                    y   = P,
                                    col = type)) +
    ggplot2::scale_colour_manual(values = colours, labels = labels) +
    ggplot2::ylab("Probability/Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = delta0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = delta1,
                        linetype   = 2)
  if (des$K == 2) {
    plots$plot_global <- plots$plot_global +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
  } else if (des$K == 3) {
    plots$plot_global <- plots$plot_global +
      ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3], sep = "")))
  } else {
    plots$plot_global <- plots$plot_global +
      ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(design$K)], sep = "")))
  }
  print(plots$plot_global)

  opchar_matrix    <- NULL
  for (k in 1:des$K) {
    tau            <- matrix(0, nrow = density, ncol = des$K)
    tau[, k]       <- seq(delta_min, delta_max, length.out = density)
    for (l in (1:des$K)[-k]) {
      tau[, l]     <- tau[, k] - delta
    }
    opchar_k       <- opchar_ma(des, tau)$opchar
    opchar_matrix  <- rbind(opchar_matrix,
                            as.matrix(opchar_k[, c(k, des$K + 2 + k)]))
  }
  opchar_LFC       <- tibble::as_tibble(opchar_matrix)
  colnames(opchar_LFC) <- c("tauk", "P")
  opchar_LFC       <-
    dplyr::mutate(opchar_LFC, type = factor(rep(paste("P", 1:des$K, sep = ""),
                                                each = density)))
  labels <- numeric(des$K)
  for (i in 1:des$K) {
    labels[i] <- parse(text = paste("italic(P)[", i, "]", sep = ""))
  }
  plots$plot_LFC <- ggplot2::ggplot() +
    ggplot2::geom_line(data = opchar_LFC,
                       ggplot2::aes(x   = .data$tauk,
                                    y   = P,
                                    col = type)) +
    ggplot2::scale_colour_manual(values = colours[2:(des$K + 1)], labels = labels) +
    ggplot2::xlab(bquote(paste("... = ", tau[italic(k)-1], " + ", .(delta), " = ",
                                tau[italic(k)], " = ",
                                tau[italic(k)+1], " + ", .(delta), " = ... ", sep = ""))) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position  = "bottom",
                   legend.title     = ggplot2::element_blank(),
                   legend.spacing.x = grid::unit(0.2, "cm")) +
    ggplot2::geom_hline(yintercept = alpha,
                        linetype   = 2) +
    ggplot2::geom_hline(yintercept = 1 - beta,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = delta0,
                        linetype   = 2) +
    ggplot2::geom_vline(xintercept = delta1,
                        linetype   = 2)
  print(plots$plot_LFC)

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
