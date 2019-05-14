#' Graphical user interface to multi-arm trial design determination
#'
#' \code{gui()} run an R Shiny web browser based graphical user interface for
#' running the functions \code{\link{des_ma}}, \code{\link{des_ma_bern}},
#' \code{\link{plot.multiarm_des_ma}}, and
#' \code{\link{plot.multiarm_des_ma_bern}}.
#'
#' @examples
#' # Launch the GUI
#' \dontrun{
#' gui()
#' }
#' @seealso \code{\link{des_ma}}, \code{\link{des_ma_bern}},
#' \code{\link{plot.multiarm_des_ma}}, \code{\link{plot.multiarm_des_ma_bern}}.
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "multiarm", package = "multiarm")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing multiarm.", call. = F)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = T)
}
