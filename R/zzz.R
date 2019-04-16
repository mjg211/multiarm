.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    rep("-", 71), "\nmultiarm: Design and analysis of fixed-sample multi-arm ",
    "clinical trials\n", rep("-", 71), "\n\nv.0.9: For an overview of the ",
    "package's functionality enter: ?multiarm\n\nFor news on the latest ",
    "updates enter: news(package = \"multiarm\")")
}
