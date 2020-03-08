{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad                  ( forM_ )
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time                      ( Day
                                                , formatTime
                                                , defaultTimeLocale
                                                )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , typ
                                                , def
                                                , help
                                                , enum
                                                , ignore
                                                , explicit
                                                , name
                                                , summary
                                                , program
                                                , helpArg
                                                , cmdArgs
                                                , args
                                                )
import           System.Environment             ( lookupEnv )

import           Lib
import           Web.AlphaVantage               ( Config(..) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


main :: IO ()
main = do
    Args {..}  <- cmdArgs argSpec
    ledgerFile <- lookupEnv "LEDGER_FILE"
    let journalFile =
            fromMaybe "~/.hledger.journal" $ asum [journalFile_, ledgerFile]
    let cfg = Config $ T.pack apiKey
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


data Args =
    Args
        { apiKey :: String
        , rateLimit :: Bool
        , journalFile_ :: Maybe FilePath
        , outputFile :: FilePath
        , excludedCurrencies :: [String]
        , dryRun :: Bool
        } deriving (Data, Typeable, Show, Eq)

argSpec :: Args
argSpec =
    Args
            { apiKey             = def &= help "Your AlphaVantage API key"
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
                &= help "File to write prices into. Default: prices.journal"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
            , excludedCurrencies = ["$", "USD"] &= args &= typ
                                       "EXCLUDED_CURRENCY ..."
            , dryRun = False &= explicit &= name "dry-run" &= name "d" &= help
                "Print the commodities and dates that would be processed."
            }
        &= summary "hledger-stockquotes, v0.1.0.0"
        &= program "hledger-stockquotes"
        &= helpArg [name "h"]
