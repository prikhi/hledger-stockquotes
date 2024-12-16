# CHANGELOG

## master


## v0.1.3.2

* Support breaking changes in `hledger-lib` v1.41.


## v0.1.3.1

* AlphaVantage changed the message field for API errors to `Error Message` so
  we now try to parse this field out of the response as well.


## v0.1.3.0

* Change `Prices` volume field from `Integer` to `Scientific` to support
  decimal amounts returned by cryptocurrency routes.
* AlphaVantage changed the information message field from `Note` to
  `Information` so we now attempt to parse both and throw an `ApiError` if
  either exist. This usually occurs when you've run out of API calls for the
  day.
* AlphaVantage changed the `DIGITAL_CURRENCY_DAILY` endpoint to return the same
  price fields as the `TIME_SERIES_DAILY` endpoint, so we dropped the
  `CryptoPrices` type and return the `Prices` type from both the stock & crypto
  API calls.
* AlphaVantage has swapped premium-only endpoints on us again - now
  `TIME_SERIES_DAILY` is free and `TIME_SERIES_DAILY_ADJUSTED` is paid-only so
  we had to switch back.


## v0.1.2.2

* Switch from the (now premium-only) `TIME_SERIES_DAILY` AlphaVantage endpoint
  to the free `TIME_SERIES_DAILY_ADJUSTED` endpoint.
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
