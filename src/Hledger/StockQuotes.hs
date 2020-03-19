{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Helper functions for the @hledger-stockquotes@ application.

-}
module Hledger.StockQuotes where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Concurrent             ( threadDelay )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( catMaybes )
import           Data.Time                      ( Day
                                                , UTCTime(utctDay)
                                                , formatTime
                                                , defaultTimeLocale
                                                , getCurrentTime
                                                , fromGregorian
                                                , toGregorian
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Hledger
import           Safe.Foldable                  ( minimumMay
                                                , maximumMay
                                                )

import           Web.AlphaVantage               ( Config
                                                , Prices(..)
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
-- Note: Fetching errors are currently logged to stdout.
fetchPrices
    :: Config
    -> [CommoditySymbol]
    -> Day
    -> Day
    -> Bool
    -> IO [(CommoditySymbol, [(Day, Prices)])]
fetchPrices cfg symbols start end rateLimit = do
    let action symbol = try (getDailyPrices cfg symbol start end) >>= \case
            Left (e :: SomeException) -> do
                putStrLn
                    $  "Error Fetching Symbol `"
                    <> T.unpack symbol
                    <> "`: "
                    ++ show e
                return Nothing
            Right prices -> return $ Just (symbol, prices)
    if rateLimit
        then fmap catMaybes $ rateLimitActions $ map action symbols
        else catMaybes <$> mapM action symbols


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
makePriceDirectives :: [(CommoditySymbol, [(Day, Prices)])] -> LBS.ByteString
makePriceDirectives = LBS.intercalate "\n\n" . map makeDirectives
  where
    makeDirectives :: (CommoditySymbol, [(Day, Prices)]) -> LBS.ByteString
    makeDirectives (symbol, prices) =
        LBS.intercalate "\n"
            $ ("; " <> LBS.fromStrict (encodeUtf8 symbol))
            : map (makeDirective symbol) prices
    makeDirective :: CommoditySymbol -> (Day, Prices) -> LBS.ByteString
    makeDirective symbol (day, prices) = LBS.intercalate
        " "
        [ "P"
        , LC.pack $ formatTime defaultTimeLocale "%F" day
        , LBS.fromStrict $ encodeUtf8 symbol
        , "$" <> LC.pack (show $ pClose prices)
        ]
