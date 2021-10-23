# hledger-stockquotes

[![hledger-stockquotes Build Status](https://github.com/prikhi/hledger-stockquotes/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/hledger-stockquotes/actions/workflows/main.yml)

`hledger-stockquotes` is a CLI addon for [hledger](https://hledger.org) that
reads a journal file and pulls the historical prices for commodities from
[AlphaVantage](https://www.alphavantage.co/). To use this application, you'll
need a [free AlphaVantage API key](https://www.alphavantage.co/support/#api-key).


## Usage

`hledger-stockquotes` parses your journal file, determines what commodities are
defined, and queries AlphaVantage for prices on the date range present in the
journal file.

By default, the program will use the HLedger default file location of
`~/.hledger.journal`. A `LEDGER_FILE` environmental variable can be used to
override the location. The `-f` flag can be used to override both the default
and `LEDGER_FILE` locations.

At the bare minimum, you need to set an `ALPHAVANTAGE_KEY` environmental
variable or use the `-a` switch to specify your AlphaVantage key:

```sh
hledger-stockquotes -a MY_API_KEY -f accounting.journal
```

This will print out price directive to a `prices.journal` file.


### Custom Output Files

The output file can be set with the `-o` flag:

```sh
hledger-stockquotes -a MY_API_KEY -o prices/2021.journal
```

NOTE: the contents of the output file will be overwritten if the file already
exists!


### Excluding Commodities

By default, we query AlphaVantage for all non-USD commodities included in your
journal file. We do not currently support AlphaVantage's FOREX or Crypto API
routes, so if you have those commodities, `stockquotes` will print an error
when fetching them. You can exclude commodities by passing them as arguments to
`hledger-stockquotes`:

```sh
hledger-stockquotes -a MY_API_KEY AUTO TA_VFFVX
```

NOTE: hledger defines an `AUTO` commodity if you use the default commodity
directive(`D`).


### Cryptocurrencies

You can specify a list of cryptocurrencies that you wish to pull prices for
with the `-c` or `--crypto` flag. You can pass a comma-separated list of
currencies or pass the flag multiple times. We will split the commodities from
your journal file into a list of equities & cryptocurrencies and hit the
appropriate AlphaVantage route for each.

```sh
hledger-stockquotes -a MY_API_KEY -c BTC,ETH --crypto XMR -c BNB
```


### API Limits

AlphaVantage has an API request limit of 5 requests per minute.
`hledger-stockquotes` enforces this limit on a per-command basis. A single run
will fetch 5 price histories, wait 60 seconds, fetch 5 more, etc. Running
multiple `hledger-stockquotes` commands in sequence will not enforce this limit
over multiple runs and may result in API errors. You can ignore the request
limiting with the `-n` flag. To test a command without hitting the API, pass
the `--dry-run` flag. This will simply print out the commodities and date
ranges that would be queried instead of making requests to AlphaVantage.


### Configuration File

`hledger-stockquotes` can also be configured via a YAML file at
`$XDG_CONFIG_HOME/hledger-stockquotes/config.yaml`(`$XDG_CONFIG_HOME` is
usually `~/.config/`).

You can set the `api-key`, `rate-limit`, `cryptocurrencies`, & `exclude`
options via this file:

```yaml
rate-limit: false
api-key: DeAdBeEf9001
crypto-currencies:
    - BTC
    - XMR
exclude:
    - USD
    - AUTO
```

CLI flags & environmental variables will override config file settings.


### Additional Documentation

The `--help` flag provides more thorough documentation on all available flags:

```sh
hledger-stockquotes --help
```


## Build / Install

This project has not yet been packaged for any OSes or Linux distributions, so
you'll have to clone this repository & compile/install the code yourself:

```sh
git clone https://github.com/prikhi/hledger-stockquotes.git
cd hledger-stockquotes
stack install
```

This will put the `hledger-stockquotes` exe into your `~/.local/bin/`
directory. Ensure that the directory is included in your `PATH` environmental
variable. Then you can run the application:

```sh
hledger-stockquotes --help
```

Since the executable has the `hledger-` prefix, you can also use it with the
`hledger` command:

```sh
hledger stockquotes -- --help
```


## Development/Manual Builds

You can build the project with stack: `stack build`

For development, you can enable fast builds with file-watching,
documentation-building, & test-running: `stack test --haddock --fast --file-watch`

To build & open the documentation, run `stack haddock --open hledger-stockquotes`

To install the executable to `~/.local/bin`, run `stack install`.


## LICENSE

BSD-3
