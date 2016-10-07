## Overview

Takes the MACR de-identified files from fixed-width tables and converts them into an Rdata file.


## Requirements

Some sort of shell (can use Cygwin on Windows), a C compiler (currently hard-coded to use GCC), and R.

## Input/Output

Place the MACR de-identified files ARRPT80LNG.TXT - whenever inside a folder named `input`. The output will be a file named `macr.Rdata` in `../../common/data`.

## To Run

Execute the `import` script, either by running `./import` or `sh import`.
