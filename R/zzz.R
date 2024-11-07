#' @import methods
#' @importFrom reticulate import py_install py_module_available

.onLoad <- function(libname, pkgname) {
  py_version <- "0.7.21"
  if (!reticulate::py_module_available("neer_match")) {
    reticulate::py_install(paste0("neer_match==", py_version))
    if (!reticulate::py_module_available("neer_match")) {
      stop(paste0(
        "Failed to automatically install the python neer_match package. ",
        "Please install it manually."
      ))
    }
  }

  options(neermatch.py = reticulate::import("neer_match"))
  if (getOption("neermatch.py")$`__version__` != py_version) {
    stop(paste0(
      "The installed version of the python neer_match package (",
      getOption("neermatch.py")$`__version__`,
      ") does not match the required version (",
      py_version,
      "). Please install the correct version."
    ))
  }
}
