# hledger-stockquotes

[![hledger-stockquotes Build Status](https://travis-ci.org/prikhi/hledger-stockquotes.svg?branch=master)](https://travis-ci.org/prikhi/hledger-stockquotes)

`hledger-stockquotes` is a CLI addon for [hledger](https://hledger.org) that
reads a journal file and pulls the historical prices for commodities from
[AlphaVantage](https://www.alphavantage.co/). To use this application, you'll
need a [free AlphaVantage API key](https://www.alphavantage.co/support/#api-key).


## Running

This application is still in early development, so you'll need to clone this
repository first:

```
git clone https://github.com/prikhi/hledger-stockquotes.git
cd hledger-stockquotes
```

Then you can run the application:

```
stack run -- --help
```

Use the `-a` flag to pass in your API key and optionally pass the path to your
journal file:

```
stack run -- -a API_KEY -f accounting.journal
```

If you omit the `-f` flag, the journal file will fallback to the value of the
`LEDGER_FILE` environmental variable. If `LEDGER_FILE` is undefined, a fallback
of `~/.hledger.journal` will be used.

You can omit the `-a` flag by setting the `ALPHAVANTAGE_KEY` environmental
variable.

The output file defaults to `prices.journal`. You can customize this with the
`-o` flag. Note that the contents of the output file will be overwritten if the
file already exists.

By default, the application will limit itself to 5 API requests a minute, as
specified by the AlphaVantage documentation. You can override this by using the
`-n` flag. You can have the application print the dates and commodities it will
fetch by passing the `--dry-run` flag.


## Manual Builds

You can build the project with stack: `stack build`

For development, you can enable fast builds with file-watching,
documentation-building, & test-running: `stack test --haddock --fast --file-watch`

To build & open the documentation, run `stack haddock --open hledger-stockquotes`

To install the executable to `~/.local/bin`, run `stack install`.


## LICENSE

BSD-3
