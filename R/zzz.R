.onLoad <- function(libname, pkgname) {
  options(warn = -1)  # Suppress all warnings
}

.onUnload <- function(libname, pkgname) {
  options(warn = 0)   # Reset warnings to default
}
