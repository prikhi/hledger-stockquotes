{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | A minimal client for the AlphaVantage API.

Currently only supports the @Daily Time Series@ endpoint.

-}
module Web.AlphaVantage
    ( Config(..)
    , Prices(..)
    , getDailyPrices
    )
where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Scientific                ( Scientific )
import           Data.Time                      ( Day
                                                , parseTimeM
                                                , defaultTimeLocale
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/~)
                                                , (=:)
                                                , GET(..)
                                                , NoReqBody(..)
                                                , runReq
                                                , req
                                                , defaultHttpConfig
                                                , https
                                                , jsonResponse
                                                , responseBody
                                                )
import           Text.Read                      ( readMaybe )

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L


-- | Configuration for the AlphaVantage API Client.
newtype Config =
    Config
        { cApiKey :: T.Text
        -- ^ Your API Key.
        } deriving (Show, Read, Eq,  Generic)


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
data Prices =
    Prices
        { pOpen :: Scientific
        , pHigh :: Scientific
        , pLow :: Scientific
        , pClose :: Scientific
        , pVolume :: Integer
        } deriving (Show, Read, Eq, Generic)

instance FromJSON Prices where
    parseJSON = withObject "Prices" $ \v -> do
        pOpen   <- parseScientific $ v .: "1. open"
        pHigh   <- parseScientific $ v .: "2. high"
        pLow    <- parseScientific $ v .: "3. low"
        pClose  <- parseScientific $ v .: "4. close"
        pVolume <- parseScientific $ v .: "5. volume"
        return Prices { .. }
      where
        parseScientific parser = do
            val <- parser
            case readMaybe val of
                Just x  -> return x
                Nothing -> fail $ "Could not read: " ++ val


-- | Fetch the Daily Prices for a Stock, returning only the prices between
-- the two given dates.
getDailyPrices :: Config -> T.Text -> Day -> Day -> IO [(Day, Prices)]
getDailyPrices cfg symbol startDay endDay = do
    resp <- runReq defaultHttpConfig $ req
        GET
        (https "www.alphavantage.co" /~ ("query" :: T.Text))
        NoReqBody
        jsonResponse
        (  "function"
        =: ("TIME_SERIES_DAILY" :: T.Text)
        <> "symbol"
        =: symbol
        <> "outputsize"
        =: ("full" :: T.Text)
        <> "datatype"
        =: ("json" :: T.Text)
        <> "apikey"
        =: cApiKey cfg
        )
    return
        . takeWhile ((<= endDay) . fst)
        . dropWhile ((< startDay) . fst)
        . L.sortOn fst
        . fromPriceList
        $ responseBody resp
