# Requirements

bash shell, R with `rmarkdown` package installed

# Usage

From command line, run:

    ./compile DATANAME

where `DATANAME` should be in lower-case and have a matching directory, e.g. `macr` or `obts`.

# Windows Instructions

You can either manually copy/edit the inventory file by replacing all instances of `__DATANAME__` with the *lower-case* version of the dataset and `__DATANAME_U__` with the *upper-case*, and then Knit in RStudio, or you can:

  1. Install cygwin with the default settings
  2. Open a cygwin terminal (default `C:\\cygwin\\bin\\bash`)
  3. `cd` to the inventory directory
  4. Run as above
