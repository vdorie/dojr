.onUnload <- function(libpath)
{
  if (is.loaded("anonymize", PACKAGE = "dbarts")) {
    library.dynam.unload("mcsupp", libpath)
  }
}
