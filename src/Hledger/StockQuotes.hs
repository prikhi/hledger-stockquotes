{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{- | Helper functions for the @hledger-stockquotes@ application.

-}
module Hledger.StockQuotes
    ( getCommoditiesAndDateRange
    , fetchPrices
    , makePriceDirectives
    , GenericPrice(..)
    , getClosePrice
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , displayException
                                                , try
                                                )
import           Data.Bifunctor                 ( second )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( catMaybes )
import           Data.Scientific                ( Scientific )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( Day
                                                , UTCTime(utctDay)
                                                , defaultTimeLocale
                                                , formatTime
                                                , fromGregorian
                                                , getCurrentTime
                                                , toGregorian
                                                )
import           Hledger
import           Safe.Foldable                  ( maximumMay
                                                , minimumMay
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Web.AlphaVantage               ( AlphaVantageResponse(..)
                                                , Config
                                                , CryptoPrices(..)
                                                , Prices(..)
                                                , getDailyCryptoPrices
                                                , getDailyPrices
                                                )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


-- | Given a list of Commodities to exclude and a Journal File, return the
-- Commodities in the Journal and the minimum/maximum days from the
-- Journal.
getCommoditiesAndDateRange
    :: [T.Text] -> FilePath -> IO ([CommoditySymbol], Day, Day)
getCommoditiesAndDateRange excluded journalPath = do
    journal     <- either error id <$> readJournalFile definputopts journalPath
    currentTime <- getCurrentTime
    let commodities =
            filter (`notElem` excluded)
                $  M.keys (jcommodities journal)
                <> M.keys (jinferredcommodities journal)
        dates       = map tdate $ jtxns journal
        currentYear = (\(y, _, _) -> y) $ toGregorian $ utctDay currentTime
        minDate     = case minimumMay dates of
            Just d  -> d
            Nothing -> fromGregorian currentYear 1 1
        maxDate = case maximumMay dates of
            Just d  -> d
            Nothing -> utctDay currentTime
    return (L.sort $ L.nub commodities, minDate, maxDate)


-- | Fetch the Prices for the Commodities from the AlphaVantage API,
-- limiting the returned prices between the given Days.
--
-- Note: Fetching errors are currently logged to 'stderr'.
fetchPrices
    :: Config
    -- ^ AlphaVantage Configuration
    -> [CommoditySymbol]
    -- ^ Commodities to Fetch
    -> [T.Text]
    -- ^ Commodities to Classify as Cryptocurrencies
    -> Day
    -- ^ Start of Price Range
    -> Day
    -- ^ End of Price Range
    -> Bool
    -- ^ Rate Limit Requests
    -> IO [(CommoditySymbol, [(Day, GenericPrice)])]
fetchPrices cfg symbols cryptoCurrencies start end rateLimit = do
    let (stockSymbols, cryptoSymbols) =
            L.partition (`notElem` cryptoCurrencies) symbols
        genericAction =
            map FetchStock stockSymbols <> map FetchCrypto cryptoSymbols
    if rateLimit
        then fmap catMaybes $ rateLimitActions $ map fetch genericAction
        else catMaybes <$> mapM fetch genericAction
  where
    fetch :: AlphaRequest -> IO (Maybe (CommoditySymbol, [(Day, GenericPrice)]))
    fetch req = do
        (symbol, label, resp) <- case req of
            FetchStock symbol ->
                (symbol, "Stock", )
                    <$> try
                            (   fmap (map (second Stock))
                            <$> getDailyPrices cfg symbol start end
                            )
            FetchCrypto symbol -> (symbol, "Cryptocurrency", ) <$> try
                (   fmap (map (second Crypto))
                <$> getDailyCryptoPrices cfg symbol "USD" start end
                )
        case resp of
            Left (e :: SomeException) -> do
                logError
                    $  "Error Fetching Prices for "
                    <> label
                    <> "  `"
                    <> T.unpack symbol
                    <> "`:\n\t"
                    ++ displayException e
                    ++ "\n"
                return Nothing

            Right (ApiError note) -> do
                logError
                    $  "Error Fetching Prices for "
                    <> label
                    <> " `"
                    <> T.unpack symbol
                    <> "`:\n\t"
                    <> T.unpack note
                    <> "\n"
                return Nothing

            Right (ApiResponse prices) -> return $ Just (symbol, prices)

    logError :: String -> IO ()
    logError = hPutStrLn stderr


-- | Types of AlphaVantage requests we make. Unified under one type so we
-- write a generic fetching function that can be rate limited.
data AlphaRequest
    = FetchStock CommoditySymbol
    | FetchCrypto CommoditySymbol

-- | Union type for all the various prices we can return.
data GenericPrice
    = Stock Prices
    | Crypto CryptoPrices

-- | Get the day's closing price.
getClosePrice :: GenericPrice -> Scientific
getClosePrice = \case
    Stock  Prices { pClose }        -> pClose
    Crypto CryptoPrices { cpClose } -> cpClose

-- | Perform the actions at a rate of 5 per second, then return all the
-- results.
--
-- Note: Will log waiting times to stdout.
rateLimitActions :: [IO a] -> IO [a]
rateLimitActions a = case chunksOf 5 a of
    [     first]    -> sequence first
    first :    rest -> do
        rest_  <- concat <$> mapM runAndDelay rest
        first_ <- sequence first
        return $ first_ ++ rest_
    [] -> return []
  where
    runAndDelay actions = do
        results <- sequence actions
        putStrLn "Waiting 60 seconds to respect API rate limits."
        threadDelay (60 * 1_000_000)
        return results


-- | Build the Price Directives for the Daily Prices of the given
-- Commodities.
makePriceDirectives
    :: [(CommoditySymbol, [(Day, GenericPrice)])] -> LBS.ByteString
makePriceDirectives = (<> "\n") . LBS.intercalate "\n\n" . map makeDirectives
  where
    makeDirectives :: (CommoditySymbol, [(Day, GenericPrice)]) -> LBS.ByteString
    makeDirectives (symbol, prices) =
        LBS.intercalate "\n"
            $ ("; " <> LBS.fromStrict (encodeUtf8 symbol))
            : map (makeDirective symbol) prices
    makeDirective :: CommoditySymbol -> (Day, GenericPrice) -> LBS.ByteString
    makeDirective symbol (day, prices) = LBS.intercalate
        " "
        [ "P"
        , LC.pack $ formatTime defaultTimeLocale "%F" day
        , LBS.fromStrict $ encodeUtf8 symbol
        , "$" <> LC.pack (show $ getClosePrice prices)
        ]
