{-# LANGUAGE DeriveDataTypeable #-}
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
                                                )

import           Lib
import           Web.AlphaVantage               ( Config(..) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


main :: IO ()
main = do
    args <- cmdArgs argSpec
    let cfg = Config $ T.pack $ apiKey args
    (commodities, start, end) <- getCommoditiesAndDateRange $ journalFile args
    prices <- fetchPrices cfg commodities start end (rateLimit args)
    LBS.writeFile (outputFile args) $ makePriceDirectives prices
    return ()


data Args =
    Args
        { apiKey :: String
        , rateLimit :: Bool
        , journalFile :: FilePath
        , outputFile :: FilePath
        } deriving (Data, Typeable, Show, Eq)

argSpec :: Args
argSpec =
    Args
            { apiKey      = def &= help "Your AlphaVantage API key"
            , rateLimit   = enum
                                [ True
                                &= help "Apply rate-limting for the API"
                                &= ignore
                                , False
                                &= help "Disable rate-limiting for the API"
                                &= explicit
                                &= name "no-rate-limit"
                                &= name "n"
                                ]
            , journalFile = def &= argPos 0 &= typ "JOURNAL_FILE"
            , outputFile  = def &= argPos 1 &= typ "OUTPUT_FILE"
            }
        &= summary "hledger-stockquotes, v0.1.0.0"
        &= program "hledger-stockquotes"
        &= helpArg [name "h"]
