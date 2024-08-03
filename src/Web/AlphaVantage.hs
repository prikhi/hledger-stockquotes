{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | A minimal client for the AlphaVantage API.

Currently only supports the @TIME_SERIES_DAILY@ & @DIGITAL_CURRENCY_DAILY@
endpoints.

-}
module Web.AlphaVantage
    ( Config(..)
    , AlphaVantageResponse(..)
    , Prices(..)
    , getDailyPrices
    , getDailyCryptoPrices
    ) where

import Control.Applicative                      ( (<|>)
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , Value(Object)
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Scientific                ( Scientific )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/~)
                                                , (=:)
                                                , GET(..)
                                                , NoReqBody(..)
                                                , defaultHttpConfig
                                                , https
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , runReq
                                                )
import           Text.Read                      ( readMaybe )

import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- | Configuration for the AlphaVantage API Client.
newtype Config =
    Config
        { cApiKey :: T.Text
        -- ^ Your API Key.
        } deriving (Show, Read, Eq,  Generic)

-- | Wrapper type enumerating between successful responses and error
-- responses with notes.
data AlphaVantageResponse a
    = ApiResponse a
    | ApiError T.Text
    deriving (Show, Read, Eq, Generic, Functor)

-- | Check for errors by attempting to parse a @Note@ or @Information@
-- field. If one does not exist, parse the inner type.
instance FromJSON a => FromJSON (AlphaVantageResponse a) where
    parseJSON = withObject "AlphaVantageResponse" $ \v -> do
        mbErrorNote <- v .:? "Note"
        mbErrorInfo <- v .:? "Information"
        case mbErrorNote <|> mbErrorInfo of
            Nothing   -> ApiResponse <$> parseJSON (Object v)
            Just note -> return $ ApiError note

-- | List of Daily Prices for a Stock.
newtype PriceList =
    PriceList
        { fromPriceList :: [(Day, Prices)]
        } deriving (Show, Read, Eq, Generic)

instance FromJSON PriceList where
    parseJSON = withObject "PriceList" $ \v -> do
        inner <- v .: "Time Series (Daily)"
        let daysAndPrices = HM.toList inner
        PriceList
            <$> mapM (\(d, ps) -> (,) <$> parseDay d <*> parseJSON ps)
                     daysAndPrices
        where parseDay = parseTimeM True defaultTimeLocale "%F"

-- | The Single-Day Price Quotes & Volume for a Stock,.
data Prices = Prices
    { pOpen          :: Scientific
    -- ^ Day's Opening Price
    , pHigh          :: Scientific
    -- ^ High Price of the Day
    , pLow           :: Scientific
    -- ^ Low Price of the Day
    , pClose         :: Scientific
    -- ^ Day's Closing Price
    , pVolume        :: Scientific
    -- ^ Trading Volume for the Day
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON Prices where
    parseJSON = withObject "Prices" $ \v -> do
        pOpen          <- parseScientific $ v .: "1. open"
        pHigh          <- parseScientific $ v .: "2. high"
        pLow           <- parseScientific $ v .: "3. low"
        pClose         <- parseScientific $ v .: "4. close"
        pVolume        <- parseScientific $ v .: "5. volume"
        return Prices { .. }


-- | List of Daily Prices for a Cryptocurrency.
newtype CryptoPriceList =
    CryptoPriceList
        { fromCryptoPriceList :: [(Day, Prices)]
        } deriving (Show, Read, Eq, Generic)

instance FromJSON CryptoPriceList where
    parseJSON = withObject "CryptoPriceList" $ \v -> do
        inner <- v .: "Time Series (Digital Currency Daily)"
        let daysAndPrices = HM.toList inner
        CryptoPriceList
            <$> mapM
                    (\(d, ps) -> (,) <$> parseAlphavantageDay d <*> parseJSON ps
                    )
                    daysAndPrices


parseAlphavantageDay :: String -> Parser Day
parseAlphavantageDay = parseTimeM True defaultTimeLocale "%F"

parseScientific :: MonadFail m => m String -> m Scientific
parseScientific parser = do
    val <- parser
    case readMaybe val of
        Just x  -> return x
        Nothing -> fail $ "Could not parse number: " ++ val


-- | Fetch the Daily Prices for a Stock, returning only the prices between
-- the two given dates.
getDailyPrices
    :: Config
    -> T.Text
    -> Day
    -> Day
    -> IO (AlphaVantageResponse [(Day, Prices)])
getDailyPrices cfg symbol startDay endDay = do
    resp <- runReq defaultHttpConfig $ req
        GET
        (https "www.alphavantage.co" /~ ("query" :: T.Text))
        NoReqBody
        jsonResponse
        (  ("function" =: ("TIME_SERIES_DAILY" :: T.Text))
        <> ("symbol" =: symbol)
        <> ("outputsize" =: ("full" :: T.Text))
        <> ("datatype" =: ("json" :: T.Text))
        <> ("apikey" =: cApiKey cfg)
        )
    return . fmap (filterByDate startDay endDay . fromPriceList) $ responseBody
        resp


-- | Fetch the Daily Prices for a Cryptocurrency, returning only the prices
-- between the two given dates.
getDailyCryptoPrices
    :: Config
    -> T.Text
    -> T.Text
    -> Day
    -> Day
    -> IO (AlphaVantageResponse [(Day, Prices)])
getDailyCryptoPrices cfg symbol market startDay endDay = do
    resp <- runReq defaultHttpConfig $ req
        GET
        (https "www.alphavantage.co" /~ ("query" :: T.Text))
        NoReqBody
        jsonResponse
        (  ("function" =: ("DIGITAL_CURRENCY_DAILY" :: T.Text))
        <> ("symbol" =: symbol)
        <> ("market" =: market)
        <> ("apikey" =: cApiKey cfg)
        )
    return
        . fmap (filterByDate startDay endDay . fromCryptoPriceList)
        $ responseBody resp

-- | Filter a list of prices to be within a range of two 'Day's.
filterByDate :: Day -> Day -> [(Day, a)] -> [(Day, a)]
filterByDate startDay endDay =
    takeWhile ((<= endDay) . fst)
        . dropWhile ((< startDay) . fst)
        . L.sortOn fst
