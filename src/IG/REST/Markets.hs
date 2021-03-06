-- | Provides functions and datatypes for interacting with the /market endpoint
-- | IG Api.
{-# LANGUAGE DuplicateRecordFields  #-}

module IG.REST.Markets where

import Prelude

-- import Control.Lens hiding (from, to)
import Lens.Micro ((.~))
import Data.Aeson
import Data.Aeson.Types hiding (Options)
import Data.List as List
import Data.String.Conversions
import Data.Text as Text
import IG
import IG.REST
import IG.REST.Markets.Types
import Network.Wreq as NW
import Text.Show

marketUrl :: Bool -> Text
marketUrl isDemo =
  host isDemo </> restPathSegment 

-- | Get the subnodes of a given node in the market navigation hierarchy
navigateMarkets :: AuthHeaders -> Maybe Node -> IO (Either ApiError MarketData)
navigateMarkets a@(AuthHeaders _ _ _ isDemo) n = do
  let opts = v1 a
  let suffix = case n of
                    Nothing -> mempty
                    Just (Node id _) -> id
  let url = cs $ marketUrl isDemo </> "marketnavigation" </> suffix 
  apiRequest $ getWith opts url


-- | Download the details of a given set of markets. There is in fact a separate
-- endpoint that allows the user to download the details of a single market, 
-- but since that can be achieved here anyway it has not been included. 
markets :: AuthHeaders -> [Epic] -> IO (Either ApiError [MarketDetails])
markets a@(AuthHeaders _ _ _ isDemo) epics = do
  let epicsQuery = Text.concat . List.intersperse "," $ epics
  let opts = v2 a & param "epics" .~ [ epicsQuery]
  let url = cs $ marketUrl isDemo </> "markets" 
  response <- apiRequest $ getWith opts url :: IO (Either ApiError Value)
  case response of
       Left e -> return $ Left e
       Right r -> 
         let 
           decodeMarkets :: Value -> Parser [MarketDetails]
           decodeMarkets = withObject "markets" $ \o -> o .: "marketDetails" 
         in
           case parseEither decodeMarkets r of
                Left parseError -> return (Left . BadPayload . cs $ parseError)
                Right details -> return $ Right details


market :: AuthHeaders -> Epic -> IO (Either ApiError MarketDetails)
market a@(AuthHeaders _ _ _ isDemo) e = do
  let opts = v2 a
  let url = cs $ marketUrl isDemo </> "markets" </> e
  apiRequest $ getWith opts url 


historicalData :: AuthHeaders -> Epic -> HistoryOpts -> Maybe Int -> IO (Either ApiError HistoricalPrices)
historicalData a@(AuthHeaders _ _ _ isDemo) e hOpts page = do
  let opts = v3 a -? ("from", from hOpts) 
                  -? ("to", to hOpts) 
                  -? ("max", IG.REST.Markets.Types.max (hOpts :: HistoryOpts)) 
                  -? ("pageSize", pageSize (hOpts :: HistoryOpts))
                  -? ("page", page)
  let url = cs $ marketUrl isDemo </> "prices" </> e
  apiRequest $ getWith opts url


(-?) :: Show a => NW.Options -> (Text, Maybe a) -> NW.Options
(-?) base (k, val) = case val of
                      Nothing -> base
                      Just v  -> base & param k .~ [cs $ Text.Show.show v]
