#' Plot operating characteristics of a fixed-sample multi-arm clinical trial
#'
#' \code{gui_ma()} run an R Shiny web browser based graphical user interface for
#' running the functions \code{\link{des_ma}} and
#' \code{\link{plot.multiarm_des_ma}}.
#'
#' @examples
#' # Launch the GUI
#' \dontrun{
#' gui_ma()
#' }
#' @seealso \code{\link{an_ma}}, \code{\link{build_ma}}, \code{\link{des_ma}},
#' \code{\link{des_int_ma}}, \code{\link{opchar_ma}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{sim_ma}}, and their
#' associated S3 \code{\link{print}} and \code{\link{summary}} generics.
#' @export
gui_ma <- function() {
  app_dir <- system.file("shiny", "multiarm", package = "multiarm")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing multiarm.", call. = F)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = T)
}
