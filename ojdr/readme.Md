## Pre-requisities

  1. A working installation of [PostgreSQL](https://www.postgresql.org)
  2. [R](https://cran.r-project.org)
  3. If on Windows
    1. A unix-like subsystem ([Cygwin](https://www.cygwin.com) recommended)

## Installation

Create a file named `dbCredentials.properties` in the installation directory. Edit it to contain contents of the form:

    host=localhost
    port=5432
    user=non_privileged_user
    password=non_privileged_user_password
    user.su=privileged_user
    password.su=privileged_user_password

The default privileged user for a PostgreSQL installation is `postgres`. That account (and password) can be used for both roles, although this is not recommended. To protect the credentials file, make sure that it is only readable by the system account that will be used to run the importation/cleaning/anonymization scripts.

## Structure

Within the `bin` folder are three executables:

    bin/import
    bin/clean
    bin/anonymize

These scripts all take the name of a dataset as their single argument. Each dataset should be matched by a folder in the dataset folder, e.g.: 

    datasets/macr
    datasets/obts

The scripts look inside these folders for files such as:

    datasets/macr/import.R
    datasets/macr/clean.R
    datasets/macr/anonymize.Rmd

Finally, all of the raw files for a dataset go inside a folder named `raw` within the dataset's subdirectory.

## To Run

Using the example of the `MACR`:

    bin/import macr

This goes through the raw files and imports them into the PostgreSQL database, performing type-checking for illegal values in the process.

    bin/import clean

The clean process pulls the dataset from the database, drops invalid rows, modifies correct-able values, and writes out the result into the dataset directory.

    bin/anonymize

Taking the clean file, anonymization modifies it to create a "Public Use File" (PUF). A pdf documenting that process will written to the top-level directory, while the PUF files themselves remain in the dataset subdirectory.
