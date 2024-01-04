.onAttach = function(libname, pkgname) {
  version = utils::packageDescription(pkgname, fields = "Version")

  msg = paste0(
    "================================================================================
  ", pkgname, " version ", version, "

  Github page: https://github.com/junjunlab/ggSCvis
  Documentation: https://github.com/junjunlab/ggSCvis

  The ggSCvis is currently on developing.
  We strongly recommend that you do not rely on this for production, but,
  feel free to explore. If you encounter a clear bug, please file a
  minimal reproducible example at https://github.com/junjunlab/ggSCvis/issues.
  For questions and other discussion, please use,
  https://github.com/junjunlab/ggSCvis/issues.

  This message can be suppressed by:
  suppressPackageStartupMessages(library(ggSCvis))
================================================================================
  ")
  packageStartupMessage(msg)
}
