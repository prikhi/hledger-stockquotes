{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
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

import           Lib
import           Web.AlphaVantage               ( Config(..) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


main :: IO ()
main = do
    Args {..} <- cmdArgs argSpec
    let cfg = Config $ T.pack apiKey
    (commodities, start, end) <- getCommoditiesAndDateRange
        (T.pack <$> excludedCurrencies)
        journalFile
    prices <- fetchPrices cfg commodities start end rateLimit
    LBS.writeFile outputFile $ makePriceDirectives prices


data Args =
    Args
        { apiKey :: String
        , rateLimit :: Bool
        , journalFile :: FilePath
        , outputFile :: FilePath
        , excludedCurrencies :: [String]
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
            , journalFile        = def &= argPos 0 &= typ "JOURNAL_FILE"
            , outputFile         = "prices.journal"
                                   &= explicit
                                   &= name "output-file"
                                   &= name "o"
                                   &= typ "OUTPUT_FILE"
            , excludedCurrencies = ["$", "USD"] &= args &= typ
                                       "EXCLUDED_CURRENCY ..."
            }
        &= summary "hledger-stockquotes, v0.1.0.0"
        &= program "hledger-stockquotes"
        &= helpArg [name "h"]
