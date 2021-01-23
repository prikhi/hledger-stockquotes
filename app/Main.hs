{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad                  ( forM_ )
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , args
                                                , cmdArgs
                                                , details
                                                , enum
                                                , explicit
                                                , help
                                                , helpArg
                                                , ignore
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitFailure )

import           Hledger.StockQuotes
import           Paths_hledger_stockquotes      ( version )
import           Web.AlphaVantage               ( Config(..) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


main :: IO ()
main = do
    Args {..}      <- cmdArgs argSpec
    journalFileEnv <- lookupEnv "LEDGER_FILE"
    apiKeyEnv      <- lookupEnv "ALPHAVANTAGE_KEY"
    apiKey         <- case asum [apiKey_, apiKeyEnv] of
        Just k -> return k
        Nothing ->
            putStrLn
                    "Error: Pass an AlphaVantage API Key with `-a` or $ALPHAVANTAGE_KEY."
                >> exitFailure

    let journalFile =
            fromMaybe "~/.hledger.journal" $ asum [journalFile_, journalFileEnv]
        cfg = Config $ T.pack apiKey
    (commodities, start, end) <- getCommoditiesAndDateRange
        (T.pack <$> excludedCurrencies)
        journalFile
    if not dryRun
        then do
            prices <- fetchPrices cfg commodities start end rateLimit
            LBS.writeFile outputFile $ makePriceDirectives prices
        else do
            putStrLn
                $  "Querying from "
                <> showDate start
                <> " to "
                <> showDate end
            putStrLn "Querying Commodities:"
            forM_ commodities
                $ \commodity -> putStrLn $ "\t" <> T.unpack commodity
  where
    showDate :: Day -> String
    showDate = formatTime defaultTimeLocale "%Y-%m-%d"


data Args = Args
    { apiKey_            :: Maybe String
    , rateLimit          :: Bool
    , journalFile_       :: Maybe FilePath
    , outputFile         :: FilePath
    , excludedCurrencies :: [String]
    , dryRun             :: Bool
    }
    deriving (Data, Typeable, Show, Eq)

argSpec :: Args
argSpec =
    Args
            { apiKey_            =
                Nothing
                &= help "Your AlphaVantage API key. Default: $ALPHAVANTAGE_KEY"
                &= explicit
                &= name "api-key"
                &= name "a"
                &= typ "ALPHAVANTAGE_KEY"
            , rateLimit          = enum
                                       [ True
                                       &= help "Apply rate-limting for the API"
                                       &= ignore
                                       , False
                                       &= help "Disable rate-limiting for the API"
                                       &= explicit
                                       &= name "no-rate-limit"
                                       &= name "n"
                                       ]
            , journalFile_       =
                Nothing
                &= help
                       "Journal file to read commodities from. Default: $LEDGER_FILE or ~/.hledger.journal"
                &= explicit
                &= name "journal-file"
                &= name "f"
                &= typ "FILE"
            , outputFile         =
                "prices.journal"
                &= help
                       "File to write prices into. Existing files will be overwritten. Default: prices.journal"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
            , excludedCurrencies = ["$", "USD"] &= args &= typ
                                       "EXCLUDED_CURRENCY ..."
            , dryRun = False &= explicit &= name "dry-run" &= name "d" &= help
                "Print the commodities and dates that would be processed."
            }
        &= summary
               (  "hledger-stockquotes v"
               ++ showVersion version
               ++ ", Pavan Rikhi 2020"
               )
        &= program "hledger-stockquotes"
        &= helpArg [name "h"]
        &= help "Generate HLedger Price Directives From Daily Stock Quotes."
        &= details
               [ "hledger-stockquotes reads a HLedger journal file, queries the "
               , "AlphaVantage stock quote API, and writes a new journal file "
               , "containing price directives for each commodity."
               , ""
               , ""
               , "DESCRIPTION"
               , ""
               , "By default, we find all non-USD commodities in your "
               , "journal file and query AlphaVantage for their stock prices "
               , "over the date range used in the journal file. Currently, we "
               , "only support public U.S. equities & do not call out to AlphVantage's"
               , "FOREX or Crypto API routes. If you have commodities that are "
               , "not supported by AlphaVantage, hledger-stockquotes will output "
               , "an error when attempting to processing them. To avoid processing "
               , "of unsupported currencies, you can pass in any commodities to "
               , "exclude as arguments. If you use the default commodity directive "
               , "in your journal file, hledger will include an `AUTO` commodity "
               , "when parsing your journal."
               , ""
               , ""
               , "API LIMITS"
               , ""
               , "AlphVantage's API limits users to 5 requests per minute. We respect "
               , "this limit by waiting for 60 seconds after every 5 commities we process. "
               , "You can ignore the rate-limiting by using the `-n` flag, but "
               , "requests are more likely to fail. You can use the `-d` flag to print "
               , "out the dates & currencies that we will fetch to avoid any unecessary "
               , "processing or API requests."
               , ""
               , ""
               , "OUTPUT FILE"
               , ""
               , "You can use the `-o` flag to set the file we will write the "
               , "generated price directives into. By default, we write to "
               , "`prices.journal`."
               , ""
               , "Warning: the output file will always be overwritten with the new "
               , "price directives. We currently do not support appending to the "
               , "output file."
               , ""
               , ""
               , "ENVIRONMENTAL VARIABLES"
               , ""
               , "If no `-f` flag is passed and the LEDGER_FILE environmental "
               , "variable is set, the program will use that as the default "
               , "HLedger file. Otherwise ~/.hledger.journal will be used."
               , ""
               , "Instead of passing the `-a` flag with your AlphaVantage API key, "
               , "you can set the ALPHAVANTAGE_KEY environmental variable instead."
               , ""
               , ""
               , "USAGE EXAMPLES"
               , ""
               , "Fetch prices for all commodities in the default journal file:"
               , "    hledger-stockquotes -a <your-api-key>"
               , ""
               , "Output prices into a custom journal file:"
               , "    hledger-stockquotes -a <your-api-key> -o prices/2021.journal"
               , ""
               , "Ignore the default, foreign, & crypto commodities:"
               , "    hledger-stockquotes -a <your-api-key> AUTO BTC ETH EUR"
               ]
