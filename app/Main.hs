{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Applicative            ( (<|>) )
import           Control.Exception.Safe         ( try )
import           Control.Monad                  ( forM_ )
import           Data.Aeson                     ( (.:?)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Version                   ( showVersion )
import           Data.Yaml                      ( prettyPrintParseException )
import           Data.Yaml.Config               ( ignoreEnv
                                                , loadYamlSettings
                                                )
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
import           System.Directory               ( doesFileExist )
import           System.Environment             ( lookupEnv )
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Hledger.StockQuotes
import           Paths_hledger_stockquotes      ( version )
import           Web.AlphaVantage               ( Config(..) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


main :: IO ()
main = do
    cfgArgs        <- cmdArgs argSpec
    cfgFile        <- loadConfigFile
    AppConfig {..} <- mergeArgsEnvCfg cfgFile cfgArgs
    let cfg = Config $ T.pack apiKey
    (commodities, start, end) <- getCommoditiesAndDateRange
        (T.pack <$> excludedCurrencies)
        journalFile
    if not dryRun
        then do
            prices <- fetchPrices cfg commodities start end rateLimit
            if null prices
                then logError "No price directives were able to be fetched."
                else LBS.writeFile outputFile $ makePriceDirectives prices
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


logError :: String -> IO ()
logError = hPutStrLn stderr . ("[ERROR] " <>)



-- CONFIGURATION

data AppConfig = AppConfig
    { apiKey             :: String
    , rateLimit          :: Bool
    , journalFile        :: FilePath
    , outputFile         :: FilePath
    , excludedCurrencies :: [String]
    , dryRun             :: Bool
    }
    deriving (Show, Eq)

defaultExcludedCurrencies :: [String]
defaultExcludedCurrencies = ["$", "USD"]

-- | Merge the Arguments, Environmental Variables, & Configuration File
-- into an 'AppConfig.
--
-- Arguments override environmental variables, which overrides the
-- configuration file.
mergeArgsEnvCfg :: ConfigFile -> Args -> IO AppConfig
mergeArgsEnvCfg ConfigFile {..} Args {..} = do
    envJournalFile <- lookupEnv "LEDGER_FILE"
    envApiKey      <- lookupEnv "ALPHAVANTAGE_KEY"
    apiKey         <- case argApiKey <|> envApiKey <|> cfgApiKey of
        Just k -> return k
        Nothing ->
            logError
                    "Pass an AlphaVantage API Key with `-a` or $ALPHAVANTAGE_KEY."
                >> exitFailure
    let journalFile =
            fromMaybe "~/.hledger.journal" $ argJournalFile <|> envJournalFile
        rateLimit =
            fromMaybe True $ either (const cfgRateLimit) Just argRateLimit
        excludedCurrencies =
            if argExcludedCurrencies == defaultExcludedCurrencies
                then fromMaybe defaultExcludedCurrencies cfgExcludedCurrencies
                else argExcludedCurrencies
        outputFile = argOutputFile
        dryRun     = argDryRun
    return AppConfig { .. }


data ConfigFile = ConfigFile
    { cfgApiKey             :: Maybe String
    , cfgRateLimit          :: Maybe Bool
    , cfgExcludedCurrencies :: Maybe [String]
    }
    deriving (Show, Eq)

instance FromJSON ConfigFile where
    parseJSON = withObject "ConfigFile" $ \o -> do
        cfgApiKey             <- o .:? "api-key"
        cfgRateLimit          <- o .:? "rate-limit"
        cfgExcludedCurrencies <- o .:? "exclude"
        return ConfigFile { .. }

loadConfigFile :: IO ConfigFile
loadConfigFile = do
    configFile <- getUserConfigFile "hledger-stockquotes" "config.yaml"
    hasConfig  <- doesFileExist configFile
    if hasConfig
        then try (loadYamlSettings [configFile] [] ignoreEnv) >>= \case
            Left (lines . prettyPrintParseException -> errorMsg) ->
                hPutStrLn stderr "[WARN] Invalid Configuration File Format:"
                    >> mapM_ (hPutStrLn stderr . ("\t" <>)) errorMsg
                    >> return defaultConfig
            Right c -> return c
        else return defaultConfig
  where
    defaultConfig :: ConfigFile
    defaultConfig = ConfigFile Nothing Nothing Nothing



data Args = Args
    { argApiKey             :: Maybe String
    , argRateLimit          :: Either () Bool
    , argJournalFile        :: Maybe FilePath
    , argOutputFile         :: FilePath
    , argExcludedCurrencies :: [String]
    , argDryRun             :: Bool
    }
    deriving (Data, Typeable, Show, Eq)

argSpec :: Args
argSpec =
    Args
            { argApiKey             =
                Nothing
                &= help "Your AlphaVantage API key. Default: $ALPHAVANTAGE_KEY"
                &= explicit
                &= name "api-key"
                &= name "a"
                &= typ "ALPHAVANTAGE_KEY"
            , argRateLimit          = enum
                [ Left ()
                &= help "Fall back to the configuration file, or True."
                &= ignore
                , Right True
                &= help "Apply rate-limting for the API"
                &= explicit
                &= name "rate-limit"
                &= name "r"
                , Right False
                &= help "Disable rate-limiting for the API"
                &= explicit
                &= name "no-rate-limit"
                &= name "n"
                ]
            , argJournalFile        =
                Nothing
                &= help
                       "Journal file to read commodities from. Default: $LEDGER_FILE or ~/.hledger.journal"
                &= explicit
                &= name "journal-file"
                &= name "f"
                &= typ "FILE"
            , argOutputFile         =
                "prices.journal"
                &= help
                       "File to write prices into. Existing files will be overwritten. Default: prices.journal"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
            , argExcludedCurrencies = defaultExcludedCurrencies &= args &= typ
                                          "EXCLUDED_CURRENCY ..."
            , argDryRun             =
                False &= explicit &= name "dry-run" &= name "d" &= help
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
               , "only support public U.S. equities & do not call out to AlphaVantage's"
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
               , "AlphaVantage's API limits users to 5 requests per minute. We respect "
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
               , "CONFIGURATION FILE"
               , ""
               , "If you have common options you constantly pass to the application, "
               , "you can specify them in a YAML configuration file. We attempt "
               , "to parse a configuration file in $XDG_CONFIG_HOME/hledger-stockquotes/config.yaml. "
               , "It currently supports the following top-level keys: "
               , ""
               , "- `api-key`:      (string) Your AlphaVantage API Key"
               , "- `exclude`:      (list of strings) Currencies to Exclude"
               , "- `rate-limit`:   (bool) Obey AlphaVantage's Rate Limit"
               , ""
               , "Environmental variables will overide any config file options, "
               , "and CLI flags will override both environmental variables & "
               , "config file options."
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
