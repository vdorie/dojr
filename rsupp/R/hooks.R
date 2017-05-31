.onUnload <- function(libpath)
{
  if (is.loaded("localSuppression", PACKAGE = "rsupp")) {
    library.dynam.unload("rsupp", libpath)
  }
}
