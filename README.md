# hledger-stockquotes

[![hledger-stockquotes Build Status](https://travis-ci.org/prikhi/hledger-stockquotes.svg?branch=master)](https://travis-ci.org/prikhi/hledger-stockquotes)

`hledger-stockquotes` is a CLI addon for [hledger](https://hledger.org) that
reads a journal file and pulls the historical prices for commodities from
[AlphaVantage](https://www.alphavantage.co/). To use this application, you'll
need a [free AlphaVantage API key](https://www.alphavantage.co/support/#api-key).


## Running

This application is still in early development, so you'll need to clone this repository first:

```
git clone https://github.com/prikhi/hledger-stockquotes.git
cd hledger-stockquotes
```

Then you can run the application:

```
stack run -- --help
```

Use the `-a` flag to pass in your API key and pass your journal file & output
file as positional arguments:

```
stack run -- -a API_KEY accounting.journal prices.journal
```

Note that the contents of the output file will be overwritten if the file
already exists.

By default, the application will limit itself to 5 API requests a minute, as
specified by the AlphaVantage documentation. You can override this by using the
`-n` flag.


## Manual Builds

You can build the project with stack: `stack build`

For development, you can enable fast builds with file-watching,
documentation-building, & test-running: `stack test --haddock --fast --file-watch`

To build & open the documentation, run `stack haddock --open hledger-stockquotes`


## TODO

* Fix dates that are outputted. Currently only outputs dates that there are
  journal entries for.
* Allow appending to the output file, only inserting new prices.
* CLI flag to set price period to first day of earliest year to last day of
  latest year or yesterday/today if latest year is this year.
* Replace `-n` flag with one that lets you set the requests per minute.


## LICENSE

BSD-3
