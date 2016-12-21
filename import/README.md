Tools for creating a lovely data oasis for the California Department of Justice.

# Installation and usage

* install heroku toolbelt  and login. Instructions for this found [here](https://toolbelt.heroku.com/).
* clone repo using `git clone git@gitlab.datakind.org:ca-open-justice/data-oasis.git`
* in the terminal, `cd` intot the repo's base directory and run `pip install .`
* associate repo with the heroku app using: `heroku config -a cadoj-data-oasis`
* to run the 'app', you'll need to set the `DATABASE_URL`. The crude way to do this is by running
  `heroku config -a cadoj-data-oasis` followed by `export
  DATABASE_URL=postgres://user:password@host:port/dbname`
* the main script for populating the database is `data-oasis/scripts/populate_db.sh`. To run this
  locally, you'll need to change the file paths (see script) to point to where you've downloaded the
CSVs. Once that's done, all you have you to is run `./populate_db.sh`.


# Python style guide

* For the sake of consistency, will follow Google's [python style
  guide](https://google.github.io/styleguide/pyguide.html#Lint). One exception: line length
  of 100 characters instead of 80.

