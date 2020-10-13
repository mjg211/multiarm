#' Graphical user interface to multi-arm trial design determination
#'
#' \code{gui()} run an R Shiny web browser based graphical user interface for
#' running \code{\link{des_dtl_bern}}, \code{\link{des_dtl_norm}},
#' \code{\link{des_gs_bern}}, \code{\link{des_gs_norm}},
#' \code{\link{des_ss_bern}}, \code{\link{des_ss_norm}}, and their associated S3
#' plotting functions.
#'
#' @examples
#' # Launch the graphical user interface
#' \dontrun{gui()}
#' @seealso \code{\link{des_dtl_bern}}, \code{\link{des_dtl_norm}},
#' \code{\link{des_gs_bern}}, \code{\link{des_gs_norm}},
#' \code{\link{des_ss_bern}}, \code{\link{des_ss_norm}},
#' \code{\link{plot.multiarm_des_dtl_bern}},
#' \code{\link{plot.multiarm_des_dtl_norm}},
#' \code{\link{plot.multiarm_des_gs_bern}},
#' \code{\link{plot.multiarm_des_gs_norm}},
#' \code{\link{plot.multiarm_des_ss_bern}},
#' \code{\link{plot.multiarm_des_ss_norm}}.
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "multiarm", package = "multiarm")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing multiarm.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE, display.mode = "normal")
}
