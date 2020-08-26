{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Broker where

import Prelude

import Lib
import Security
import Text.Pretty.Simple (pPrint)
import Text.Show

import IG
import IG.REST
import IG.REST.Dealing
import IG.REST.Dealing.Types
import IG.REST.Login

import Control.Concurrent (threadDelay)
import Data.Hourglass
import Time.System (timeCurrent)
import Control.Concurrent.STM.TMVar  --using TMVars as threadsafe and STM-happy mutable vars, apparantly superior to MVars (may block more easily), and IORefs (single-thread only)
import qualified Control.Concurrent.STM.TMapMVar as TM

data TradingEnvironment = Demo | Live deriving (Show, Read)

createAuthHeaderTMVar :: IO (TMVar AuthHeaders)
createAuthHeaderTMVar = do
  authHeadersTMVar <- atomically $ newEmptyTMVar --create the TMVar
  return $ authHeadersTMVar                      --return the address, ie. TMVar so that it can be passed around the program

loginFirst :: TradingEnvironment -> Logger -> TMVar AuthHeaders -> IO ()
loginFirst tradingEnvironment logger tmAuthHeaders = do
  -- authHeaders <- atomically $ takeTMVar tmAuthHeaders
  resp1 <- case tradingEnvironment of
            Live -> login False "api-key-live" (LoginBody False "userid-live" "passw0rd")
            Demo -> login True  "api-key-demo" (LoginBody False "userid-demo" "passw0rd") 
  let (authHeaders, loginResponse) = fromRight undefined resp1  --needs a default; will error if secID elem  contains a left
  logInfo logger $ Prelude.show loginResponse
  _ <- atomically $ tryTakeTMVar tmAuthHeaders
  -- this returns a Maybe, emptying the TMVar if it was populated. I don't need the prev value (if it existed), and can just go ahead put the new auth headers in.
  atomically $ putTMVar  tmAuthHeaders authHeaders   --first time: put them in
  return ()

--This data type is accessbile in realtime, and contains just the fields needed, so in realtime.hs you don't have to muck about with accessing the fields provided by IG
data RealPosn = RealPosn { rpDealID :: Text
                         , rpEpic   :: Text
                         , rpSize   :: Double
                         , rpOpenLevel :: Double
                         , rpDir    :: Direction
                         , rpLastPrice :: Double
                         , rpStopLevel :: Maybe Double
                         , rpLotSize       :: Double
                         , rpScalingFactor :: Double
                         , rpCcy       :: Text
                         , rpMktState  :: Text
                         } deriving (Eq)
                         -- } deriving (Eq,Show)
instance Show RealPosn where
    show (RealPosn rpDealID rpEpic rpSize rpOpenLevel rpDir rpLastPrice rpStopLevel rpLotSize rpScalingFactor rpCcy rpMktState ) = Prelude.show rpEpic
          ++ " " ++ Prelude.show rpSize
          ++ " " ++ Prelude.show rpOpenLevel
          ++ " " ++ Prelude.show rpDir
          ++ " " ++ Prelude.show rpLastPrice
          ++ " " ++ Prelude.show rpLastPrice
          ++ " " ++ Prelude.show rpStopLevel
          ++ " " ++ Prelude.show rpLotSize
          ++ " " ++ Prelude.show rpScalingFactor
          ++ " " ++ Prelude.show rpCcy
          ++ " " ++ Prelude.show rpMktState

emptyRealPosn :: Text -> RealPosn
emptyRealPosn ep = RealPosn "empty" ep 0 0 BUY 0 (Just 0) 0 0 "" ""

getAllPositions :: TradingEnvironment -> TMVar AuthHeaders -> Logger -> IO [RealPosn]
getAllPositions tradingEnvironment tmAuthHeaders logger = do
  ah <- atomically $ readTMVar tmAuthHeaders
  resp2 <- allPositions ah
  case resp2 of
    Left e -> do logInfo logger $ Prelude.show e
                 case e of
                   InvalidClientToken -> do logInfo logger $ "Log in again, due to " <> Prelude.show e  --do call to login again, this will refresh the authHeaders, as they've expired
                                            loginFirst tradingEnvironment logger tmAuthHeaders
                   -- NoResponseDataReceived -> do 
                   _ -> do logInfo logger "Waiting 60s before trying again ; surely IG will have their systems back up by then"
                           threadDelay $ 60 * 1000000 --wait 60 seconds
                 getAllPositions tradingEnvironment tmAuthHeaders logger
    Right posns -> return $ convertPositionDataToRealPosn <$> (positions posns)

convertPositionDataToRealPosn :: PositionData -> RealPosn
convertPositionDataToRealPosn pd = RealPosn rpdid ep sz ol dir lp sl ls sf ccy ms
  where -- ep = IG.REST.Dealing.Types.epic $ market pd
        rpdid = dealId (position pd :: Position)
        ep  = epic (market pd :: Market)
        sz  = size (position pd :: Position)
        ol  = level (position pd :: Position)
        dir = direction (position pd :: Position)
        sl  = if isNaN stp then Nothing
                           else Just stp
        stp = stopLevel (position pd :: Position)  --intermediate step
        lp  = case dir of
                BUY -> ofr
                SELL-> bd
        bd  = bid (market pd :: Market)
        ofr = offer (market pd :: Market)
        sf  = IG.REST.Dealing.Types.scalingFactor (market pd :: Market)
        ls  = IG.REST.Dealing.Types.lotSize (market pd :: Market)
        ccy = Prelude.show $ IG.REST.Dealing.Types.currency (position pd :: Position)  --I don't want to have to import the IG types into realtime.hs
        ms  = Prelude.show $ marketStatus (market pd :: Market)                        --I don't want to have to import the IG types into realtime.hs
        -- secID = "L1:" ++ epicText
        -- lotSz   = lotSize (market pd :: Market)
        -- scalFct = scalingFactor (market pd :: Market)

-- Example: here are two open trades, returned by allPositions ; getAllTrades returns just the list (or retries if Left)
-- PositionsResponse {positions = [PositionData {position = Position {contractSize = 10000.0, controlledRisk = False, createdDateUTC = UTCDate 2019-11-13 14:19:48 UTC, currency = "USD", dealId = "DIAAAAC4PNG5MAU", dealReference = "ME4MFMMDYAM44S4", direction = BUY, level = 1.28319, limitLevel = Nothing, size = 1.0, stopLevel = NaN, trailingStep = Nothing, trailingStopDistance = Nothing}, market = Market {bid = 1.28319, delayTime = 0.0, epic = "CS.D.GBPUSD.MINI.IP", expiry = None, high = 1.2868, instrumentName = "GBP/USD Mini", instrumentType = CURRENCIES, lotSize = 1.0, low = 1.28218, marketStatus = TRADEABLE, netChange = -1.35e-3, offer = 1.28328, percentageChange = -0.11, scalingFactor = 10000.0, streamingPricesAvailable = True, updateTimeUTC = DealTime 1970-01-01 14:22:02 UTC}},PositionData {position = Position {contractSize = 10000.0, controlledRisk = False, createdDateUTC = UTCDate 2019-11-13 14:21:49 UTC, currency = "JPY", dealId = "DIAAAAC4PNG8EAU", dealReference = "VCSSZTG2SW244S4", direction = SELL, level = 108.776, limitLevel = Nothing, size = 1.0, stopLevel = NaN, trailingStep = Nothing, trailingStopDistance = Nothing}, market = Market {bid = 108.777, delayTime = 0.0, epic = "CS.D.USDJPY.MINI.IP", expiry = None, high = 109.154, instrumentName = "USD/JPY Mini", instrumentType = CURRENCIES, lotSize = 100.0, low = 108.747, marketStatus = TRADEABLE, netChange = -0.228, offer = 108.784, percentageChange = -0.21, scalingFactor = 100.0, streamingPricesAvailable = True, updateTimeUTC = DealTime 1970-01-01 14:21:53 UTC}}]}
-- Example: and here is the same request when there are no open positions: an empty list
-- PositionsResponse {positions = []}


dealAtMarket :: Text -> Direction -> Double -> Double -> Maybe Double -> PositionRequest
dealAtMarket ep dir sz lvl sl = PositionRequest { currencyCode = Security.currency ep
                                                , dealReference = Nothing
                                                , direction = dir
                                                , epic = ep
                                                , expiry = None
                                                , forceOpen = forceOpenFlag   --reqd True in order to set a stop level (specified below)
                                                , guaranteedStop = False
                                                , level = Nothing
                                                , limitDistance = Nothing
                                                , limitLevel = Nothing
                                                , orderType = MARKET
                                                , quoteId = Nothing
                                                , size = sz
                                                , stopLevel = stopLoss 
                                                , timeInForce = FILL_OR_KILL
                                                , trailingStop = False
                                                , trailingStopIncrement = Nothing
                                                }
    where stopLoss = case sl of
            Nothing  -> Nothing
            Just sl' -> case ep of  --scale the stop loss for currencies
                          "CS.D.GBPEUR.MINI.IP" -> Just ( lvl + stopLossIncr ( sl' / 100 ) )
                          "CS.D.USDJPY.MINI.IP" -> Just ( lvl + stopLossIncr ( sl' / 100 ) )
                          _                     -> Just ( lvl + stopLossIncr   sl' )
          stopLossIncr :: Double -> Double
          stopLossIncr x = case dir of
            BUY      -> 0 - x --ie. negative  (protects against a lower price)
            SELL     -> 0 + x --ie. positive  (protects against a higher price)
          forceOpenFlag  = case sl of
            Nothing  -> False
            Just _   -> True --need this set to allow a stop loss to be provided

emptyPR = PositionRequest "" Nothing BUY "" None False False Nothing Nothing Nothing MARKET Nothing 0 Nothing FILL_OR_KILL False Nothing 

getCurrentHour :: Elapsed -> String
getCurrentHour currentTime = drop 11 $ take 13 $ timePrint ISO8601_DateAndTime currentTime

waitIfBetween10pm11pm :: IO ()
waitIfBetween10pm11pm = do
  -- IG do maintenance at this time, there are loads of timeouts etc.  Don't even try logging in until after 11pm 
  currentTime <- timeCurrent
  let timeZoneOffset = 1*60*60  --1 hour offset from UTC during BST (daylight saving)
  let currentTime'   = currentTime + timeZoneOffset
  when ((getCurrentHour currentTime') == "22") $ do threadDelay $ 1000000 * 60  --wait 60 seconds
                                                    waitIfBetween10pm11pm


--copied from IG dealing spec
closeRequest :: AuthHeaders -> PositionData -> IO (Either ApiError DealReference)
closeRequest h pos = closePosition h $ toClosePositionRequest pos options
  where options = CloseOptions Nothing MARKET Nothing Nothing Nothing Nothing Nothing

placeTrade :: TMVar AuthHeaders -> PositionRequest -> IO (Either ApiError DealReference)
placeTrade tmAuthHeaders pr = do
    ah <- atomically $ readTMVar tmAuthHeaders
    dr <- createPosition ah pr
    pure dr

-- posnRequestIsNotTheSameAsLastRequest :: TM.TMapMVar Text PositionRequest -> PositionRequest -> Bool
-- posnRequestIsNotTheSameAsLastRequest lastPR pr = do
--   lastPR = getLastPosn

