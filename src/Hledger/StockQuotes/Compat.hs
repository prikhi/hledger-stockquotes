{-# LANGUAGE CPP #-}
-- | Hledger-related functions that use CPP macros for multiple
module Hledger.StockQuotes.Compat
    ( allJournalCommodities
    ) where

import Hledger (CommoditySymbol, Journal (..))

import qualified Data.Map.Strict as M

-- | Get all declared & inferred commodities of a Journal.
allJournalCommodities :: Journal -> [CommoditySymbol]
allJournalCommodities journal =
-- TODO: remove < 1.41 support after we drop GHC 9.8
#if MIN_VERSION_hledger_lib(1, 41, 0)
    M.keys (jdeclaredcommodities journal)
        <> M.keys (jinferredcommoditystyles journal)
#else
    M.keys (jcommodities journal)
        <> M.keys (jinferredcommodities journal)
#endif
