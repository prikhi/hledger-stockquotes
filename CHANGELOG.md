# CHANGELOG

## master

* Bump package dependencies.


## v0.1.2.1

* Fix breaking changes in `hledger-lib` v1.26.


## v0.1.2.0

* Add support for fetching cryptocurrency prices with the `-c` flag and
  `cryptocurrencies` config option.
* Add support for config file at `$XDG_CONFIG_HOME/hstockquotes/config.yaml`
  with `api-key`, `exclude`, & `rate-limit` options.


## v0.1.1.0

* Don't write out a journal file if no prices were successfully fetched.
* Log API errors to `stderr` instead of `stdout`.
* Improve error messages when the AlphaVantage API returns a
  rate-limit-exceeded error.
* Improve documentation in README & `--help` flag.
* Add trailing newline to generated files.


## v0.1.0.0

* Initial release
