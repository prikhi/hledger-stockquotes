{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Helper functions for the @hledger-stockquotes@ application.
module Hledger.StockQuotes
    ( getCommoditiesAndDateRange
    , fetchPrices
    , makePriceDirectives
    , unaliasAndBucketCommodities
    , reAliasCommodities
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
    ( SomeException
    , displayException
    , try
    )
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( Day
    , UTCTime (utctDay)
    , defaultTimeLocale
    , formatTime
    , fromGregorian
    , getCurrentTime
    , toGregorian
    )
import Hledger
    ( CommoditySymbol
    , Journal (..)
    , Transaction (..)
    , definputopts
    , readJournalFile
    , runExceptT
    )
import Safe.Foldable
    ( maximumMay
    , minimumMay
    )
import System.IO
    ( hPutStrLn
    , stderr
    )

import Web.AlphaVantage
    ( AlphaVantageResponse (..)
    , Config
    , Prices (..)
    , getDailyCryptoPrices
    , getDailyPrices
    )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T


-- | Given a list of Commodities to exclude and a Journal File, return the
-- Commodities in the Journal and the minimum/maximum days from the
-- Journal.
getCommoditiesAndDateRange
    :: [T.Text] -> FilePath -> IO ([CommoditySymbol], Day, Day)
getCommoditiesAndDateRange excluded journalPath = do
    journal <-
        fmap (either error id) . runExceptT $
            readJournalFile
                definputopts
                journalPath
    currentTime <- getCurrentTime
    let commodities =
            filter (`notElem` excluded) $
                M.keys (jcommodities journal)
                    <> M.keys (jinferredcommodities journal)
        dates = map tdate $ jtxns journal
        currentYear = (\(y, _, _) -> y) $ toGregorian $ utctDay currentTime
        minDate = case minimumMay dates of
            Just d -> d
            Nothing -> fromGregorian currentYear 1 1
        maxDate = case maximumMay dates of
            Just d -> d
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
    -> M.Map T.Text T.Text
    -- ^ Map of aliases to transform journal commodities
    -> Day
    -- ^ Start of Price Range
    -> Day
    -- ^ End of Price Range
    -> Bool
    -- ^ Rate Limit Requests
    -> IO [(CommoditySymbol, [(Day, Prices)])]
fetchPrices cfg symbols cryptoCurrencies aliases start end rateLimit = do
    let (stockSymbols, cryptoSymbols) =
            unaliasAndBucketCommodities symbols cryptoCurrencies aliases
        genericAction =
            map FetchStock stockSymbols <> map FetchCrypto cryptoSymbols
    if rateLimit
        then fmap catMaybes $ rateLimitActions $ map fetch genericAction
        else catMaybes <$> mapM fetch genericAction
  where
    fetch
        :: AlphaRequest -> IO (Maybe (CommoditySymbol, [(Day, Prices)]))
    fetch req = do
        (symbol, label, resp) <- case req of
            FetchStock symbol ->
                (symbol,"Stock",)
                    <$> try (getDailyPrices cfg symbol start end)
            FetchCrypto symbol ->
                (symbol,"Cryptocurrency",)
                    <$> try
                        ( getDailyCryptoPrices cfg symbol "USD" start end
                        )
        case resp of
            Left (e :: SomeException) -> do
                logError $
                    "Error Fetching Prices for "
                        <> label
                        <> "  `"
                        <> T.unpack symbol
                        <> "`:\n\t"
                        ++ displayException e
                        ++ "\n"
                return Nothing
            Right (ApiError note) -> do
                logError $
                    "Error Fetching Prices for "
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


-- | Given a list of commodities from a journal, a list a cryptocurrencies,
-- and a map of aliases, return the a list of AlphaVantage equities
-- & cryptocurencies.
unaliasAndBucketCommodities
    :: [CommoditySymbol]
    -- ^ Journal symbols
    -> [T.Text]
    -- ^ Cryptocurrency symbols
    -> M.Map T.Text T.Text
    -- ^ Aliases
    -> ([CommoditySymbol], [CommoditySymbol])
unaliasAndBucketCommodities symbols cryptoCurrencies aliases =
    L.partition (`notElem` cryptoCurrencies) $
        S.toList $
            S.fromList $
                map transformAliases symbols
  where
    transformAliases :: T.Text -> T.Text
    transformAliases original =
        fromMaybe original $ M.lookup original aliases


-- | Given a list of paired unaliased symbols, the original journal
-- commodities, and the map of aliases, generate a new list of paired
-- symbols that reflects the commodities in the original journal.
--
-- Pairs with symbols in the journal but not in the aliases will be
-- unaltered. Pairs with aliases only in the journal will return only alias
-- items. Pairs for multiple aliases with return a set of items for each
-- alias. Pairs with symbols and aliases in the journal will return both
-- sets of items.
reAliasCommodities
    :: [(CommoditySymbol, a)]
    -- ^ Unaliased pairs of symbols
    -> [CommoditySymbol]
    -- ^ Original symbols from the journal
    -> M.Map T.Text T.Text
    -- ^ Aliases
    -> [(CommoditySymbol, a)]
reAliasCommodities symbolPairs journalSymbols aliases =
    concatMap reAlias symbolPairs
  where
    reAlias :: (CommoditySymbol, a) -> [(CommoditySymbol, a)]
    reAlias s@(cs, a) = case M.lookup cs reverseAliases of
        Nothing ->
            [s]
        Just revAliases ->
            map (,a) $ filter (`elem` journalSymbols) $ NE.toList revAliases
    reverseAliases :: M.Map T.Text (NE.NonEmpty T.Text)
    reverseAliases =
        let journalSymbolPairs = map (\s -> (s, NE.singleton s)) journalSymbols
         in M.fromListWith (<>)
                . (<> journalSymbolPairs)
                . map (\(k, v) -> (v, NE.singleton k))
                $ M.assocs aliases


-- | Types of AlphaVantage requests we make. Unified under one type so we
-- write a generic fetching function that can be rate limited.
data AlphaRequest
    = FetchStock CommoditySymbol
    | FetchCrypto CommoditySymbol


-- | Perform the actions at a rate of 5 per minute, then return all the
-- results.
--
-- Note: Will log waiting times to stdout.
rateLimitActions :: [IO a] -> IO [a]
rateLimitActions a = case chunksOf 5 a of
    [first] -> sequence first
    first : rest -> do
        rest_ <- concat <$> mapM runAndDelay rest
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
    :: [(CommoditySymbol, [(Day, Prices)])] -> LBS.ByteString
makePriceDirectives = (<> "\n") . LBS.intercalate "\n\n" . map makeDirectives
  where
    makeDirectives
        :: (CommoditySymbol, [(Day, Prices)]) -> LBS.ByteString
    makeDirectives (symbol, prices) =
        LBS.intercalate "\n" $
            ("; " <> LBS.fromStrict (encodeUtf8 symbol))
                : map (makeDirective symbol) prices
    makeDirective :: CommoditySymbol -> (Day, Prices) -> LBS.ByteString
    makeDirective symbol (day, prices) =
        LBS.intercalate
            " "
            [ "P"
            , LC.pack $ formatTime defaultTimeLocale "%F" day
            , LBS.fromStrict $ encodeUtf8 symbol
            , "$" <> LC.pack (show $ pClose prices)
            ]
