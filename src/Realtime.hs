module Realtime where

import Prelude
import Lib
import Database  --my library
import Security  --my library
import Broker    --my library
import Settings  --my library
import OHLC      --my library
import Tui       --my library  (it's imported in libraries above, I guess, so not needed directly)
-- import Control.Concurrent  --has forkIO,threadDelay
-- import qualified Data.Sequence as S  --use sequence instead of list to speed up tick load
import Data.Sequence((<|), (|>), (><), Seq(..), ViewL(..), ViewR(..) )
import Text.Pretty.Simple (pPrint)
-- import Data.Hourglass
import System.ZMQ4.Monadic
-- import Control.Concurrent.STM  --looking for STM type
import qualified Data.ByteString.Char8 as CS
import qualified Control.Concurrent.STM.TMapMVar as TM
import Data.Text.Read (double,decimal)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S  --use sequence instead of list to speed up tick load
import Database.SQLite3 as SQL3
import qualified Control.Concurrent.STM.TBQueue  as TBQ
import Time.System
import Data.Hourglass  --contains fromElapsedTime
import qualified Text.Show as TS
import Data.Text (drop,dropEnd,unpack)
import Control.Concurrent.STM.TMVar  --using TMVars as threadsafe and STM-happy mutable vars, apparantly superior to MVars (may block more easily), and IORefs (single-thread only)
import Fmt

import IG (ApiError)
import IG.REST        --types for result of login
import IG.REST.Login  --types for positions
import IG.REST.Dealing.Types ( Direction (BUY,SELL) , DealReference , PositionRequest )  --types for trade direction, deal reference

newMVarByteString :: IO (MVar CS.ByteString)
newMVarByteString = Prelude.newEmptyMVar

demoOrLive :: Bool -> TradingEnvironment
demoOrLive flag = case flag of
                    True  -> Live
                    False -> Demo

--Receive prices via zmq, and call the function that processes them
runRealtime :: Bool-> Text -> Text -> Logger -> IO ()
runRealtime liveAccountFlag ipFilename startDateText logger = liftIO $ runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub "tcp://127.0.0.1:4998"
    connect sub "tcp://127.0.0.1:4999"
    -- newPFPriceMVar   <- liftIO newMVarByteString  --var for point and figure tracking
    -- newOHLCPriceMVar <- liftIO newMVarByteString  --var for OHLC tracking
    newPFPriceQueue   <- atomically $ TBQ.newTBQueue 100000  --surely it won't get more than a thousand ticks behind??
    newOHLCPriceQueue <- atomically $ TBQ.newTBQueue 100000  --surely it won't get more than a thousand ticks behind??

    let tradingEnvironment = demoOrLive liveAccountFlag  --convert the flag to the type that is expected
    _ <- liftIO $ forkIO $ initProcessPrices   logger ipFilename tradingEnvironment startDateText newPFPriceQueue newOHLCPriceQueue    --initialise PF and OHLC processing; load prior data from sqlite data file
    liftIO $ logInfo logger "Subscribed to zmq queues for tick data, looping...."
    forever $ do
      newPriceString <- receive sub  --recv from zmq
      atomically $ TBQ.writeTBQueue newOHLCPriceQueue newPriceString --push tick on the queue --convert STM to IO
      atomically $ TBQ.writeTBQueue newPFPriceQueue   newPriceString --push tick on the queue --convert STM to IO


type SecurityID = Text

data PaperTradeState = TradeShortProfitable
                     | TradeShortOpen
                     | TradeShortSignal2
                     | TradeShortSignal1
                     | TradeShortClosed
                     | NoTrade
                     | TradeLongClosed
                     | TradeLongSignal1
                     | TradeLongSignal2
                     | TradeLongOpen
                     deriving (Show,Eq,Ord)

data PaperTrade = PaperTrade { paperTradeState :: PaperTradeState
                             , size :: Double
                             , pnl :: Double
                             } deriving (Eq)
                             -- } deriving (Show,Eq)
instance Show PaperTrade where
    show (PaperTrade st sz pnl) = Prelude.show st ++ " " ++  Prelude.show sz ++ " " ++ Prelude.show pnl

emptyTrade :: PaperTrade
emptyTrade = PaperTrade NoTrade 0 0

isPaperTradeClosed :: PaperTrade -> Bool
isPaperTradeClosed pt = paperTradeState pt == TradeShortClosed
                     || paperTradeState pt == TradeLongClosed    --if short/long trade has been closed


--This routine is called by the zmq routine, so that the bind is done to start buffering prices there.  Next, the history/back-data is loaded.  Only then can
--prices be pulled off the live queues, to get the sequence right. It has to be called by the zmq biding routine to get the MVar address that it's
--sending the new prices from zmq to, internally
initProcessPrices:: Logger
              -> Text
              -> TradingEnvironment
              -> Text
              -> TBQ.TBQueue CS.ByteString
              -> TBQ.TBQueue CS.ByteString
              -> IO ()
initProcessPrices logger ipFilename tradingEnvironment startDateText pfPriceTBQ ohlcPriceTBQ = do
    secMap           <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the list of securities to be tracked
    --PF specific
    chartMapSV       <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of charts, to be updated
    chartMapMV       <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of charts, to be updated
    chartMapLV       <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of charts, to be updated
    signalMapSV      <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of signals, to be updated
    signalMapMV      <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of signals, to be updated
    signalMapLV      <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of signals, to be updated
    paperPosnMap     <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of positions, to be updated
    realPosnMap      <- atomically TM.newTMapMVar   --needs to be a map by secID; used to limit number of new trades that can be opened in this session
    lastPosnRequest  <- atomically TM.newTMapMVar   --needs to be a map by secID; used to check for duplicate trades being placed (since analysis is faster than execution)
    --OHLC specific
    ohlcDataMap      <- atomically TM.newTMapMVar   --initialise the map; move from STM to IO  --this is the map of charts, to be updated 

    pPrint "Starting up PF and OHLC"

    let secIDs = mapEpic <$>  [ -- "L1:CS.D.EURGBP.CFD.IP"
                              -- , "L1:CC.D.CL.UNC.IP"
                              -- , "L1:CC.D.LCO.UNC.IP"
                              -- , "L1:CS.D.GBPUSD.CFD.IP"
                              -- "L1:CS.D.USDJPY.CFD.IP"
                              -- , "L1:CS.D.EURUSD.CFD.IP"
                              "L1:IX.D.NASDAQ.IFS.IP"
                              ] :: [SecurityID]

    pPrint "Check for valid settings"
    logInfo logger "Check for valid settings"
    settings <- loadSettings $ "settings." <> show tradingEnvironment <> ".json"
    case settings of
      Nothing -> pPrint "No settings file found"
      Just s1 -> do let listOfSettingsBySecID = (selectSetting s1) <$> secIDs
                    if Prelude.any Prelude.isNothing listOfSettingsBySecID
                      then do pPrint "Problem wtih settings file ARGGG - missing or has duplicates"
                              pPrint s1
                              pPrint $ Prelude.filter (\(a,b) -> Prelude.isNothing b) $ zip secIDs listOfSettingsBySecID
                      else pPrint "Settings file contains all secIDs, and no duplicates: good to continue"

    pPrint $ "Process realtime prices...."
    -- let processPricesM _ = processPrices ah chartMap paperPosnMap realPosnMap  signalMap secIDs logger priceMVar  --cheeky mondic defn for the func I want to call with Control.Retry.recoverAll
    pPrint $ "createAuthHeaderTMVar"
    ahTMvar <- createAuthHeaderTMVar
    pPrint $ "loginFirst tradingEnvironment logger ahTMvar"
    loginFirst tradingEnvironment logger ahTMvar --Assume that error was the auth header tokens have expired, so get new ones

    startingTrades <- mapM (getRealPosnAndConvertToPaperPosn tradingEnvironment ahTMvar logger ) secIDs
    atomically $ mapM_ (initPaperPosnMap paperPosnMap) $ zip secIDs startingTrades
    pPrint         $ "Initialised maps for" <> show secIDs
    logInfo logger $ "Initialised maps for" <> show secIDs

    pPrint "Initialising graphs"

    --initialise the OHLC data map : need starting values against which to insert new mid values
    atomically $ mapM_ (initialiseOHLCDataMap ohlcDataMap ) secIDs

    pPrint $ "Loading " ++ (toString ipFilename) 
    -- -- _ <- forkIO $ the following??  -not really necessary, it only takes about 2 seconds to load and backtest 1 days worth of data (on a Monday!) [very much dependent on length/size of the db]
    mapM_ (loadBackdata ipFilename startDateText ohlcDataMap logger) secIDs   --without mapM, you get nesting of IO's in [], it's just messy (and won't build)


    logInfo logger $ "Starting TUI" <> "...." 
    _ <- forkIO $ runTui logger secIDs ohlcDataMap

    -- go into infinte loop, processing prices
    pPrint         $ "Process realtime prices for " <> Prelude.show secIDs <> "...." 
    logInfo logger $ "Process realtime prices for " <> Prelude.show secIDs <> "...." 
    _ <- forkIO $ processPricesOHLC tradingEnvironment ahTMvar ohlcDataMap secIDs logger ohlcPriceTBQ 100
    pPrint         $ "Initialisation complete" <> Prelude.show secIDs <> "...." 
    logInfo logger $ "Initialisation complete" <> Prelude.show secIDs <> "...." 

initPaperPosnMap :: TM.TMapMVar Text PaperTrade -> (Text , PaperTrade) -> STM ()
initPaperPosnMap !posnMap (secID , pt) =
  TM.insertForce posnMap secID pt 

getRealPosnAndConvertToPaperPosn :: TradingEnvironment
                                 -> TMVar AuthHeaders
                                 -> Logger
                                 -> Text
                                 -> IO PaperTrade
getRealPosnAndConvertToPaperPosn te ah logger secID = do
    rp <- liftIO $ getRealPosnFromBroker te ah logger secID
    logInfo logger $ "Starting position for " <> secID <> " " <> show rp
    -- pPrint rp
    --Take an updated posn from the broker if one exists
    logInfo logger $ "Which is empty:" <> show (rpDealID rp == "empty" )
    let pt = case (rpDealID rp == "empty" ) of  --this is the return value
                            True  -> emptyTrade      -- no broker posn, so set to no trade
                            False -> paperPosnFromReal  -- there is a broker posn, so use that regardless of the paper posn (this will re-execute trades not filled, or pick up manually opened trades)
          where paperPosnFromReal = PaperTrade pst sz 0   --PFNoSignal since trade was generated manually, not from this analysis
                pst = case rpDir rp of
                  SELL -> TradeShortOpen
                  BUY  -> TradeLongOpen
                sz  = rpSize rp
                ol  = rpOpenLevel rp
    pure pt  --push this value back as the resul of the function


loadBackdata :: Text
             -> Text
             -> TM.TMapMVar Text OHLCData 
             -> Logger
             -> SecurityID
             -> IO ()
loadBackdata !ipFilename !startDateText !ohlcDataMap !logger !secID = do
    conn <- SQL3.open ipFilename
    t <- timeCurrent
    -- let t = 1587600000 :: Elapsed --timestamp for 24 hours before 12:00am on 2020-07-23
    let oneDayAgo :: String = Prelude.take (Prelude.length (TS.show t) -1 ) $ show (timeGetElapsed t - (fromIntegral 1) * 80 * 60 * 60)  -- get timestamp for 80 hours, so can calc 4h ma/boll.s
    -- let oneDayAgo :: String = Prelude.take (Prelude.length (TS.show t) -1 ) $ show (timeGetElapsed t - (fromIntegral 1) * 40 * 60 * 60)  -- get timestamp for 80 hours, so can calc 4h ma/boll.s
    -- let oneDayAgo :: String = Prelude.take (Prelude.length (TS.show t) -1 ) $ show (timeGetElapsed t - (fromIntegral 1) * 5 * 60 * 60)  -- get timestamp for 12 hours before the current time
    tickQueue <- atomically $ TBQ.newTBQueue 20000000   --one per symbol : start with 1M bound  --initialise the queue; move from STM to IO
    tickMapSV   <- atomically $ TM.newTMapMVar
    tickMapMV   <- atomically $ TM.newTMapMVar
    tickMapLV   <- atomically $ TM.newTMapMVar
    logInfo logger $          "SELECT timestamp, bid, offer, state FROM ticks WHERE symbol = \'" <> (inverseMapEpic secID) <> "\'"
                              <> " AND timestamp >= " <> toText oneDayAgo
                              -- <> " AND timestamp >= " <> convertTimestampToString ( startTimestamp )
                              -- <> " LIMIT 5 "   --DEBUG only
                              <> " ; " 
    SQL3.execWithCallback conn ( "SELECT timestamp, bid, offer, state FROM ticks WHERE symbol = \'" <> (inverseMapEpic secID) <> "\'"
                              <> " AND timestamp >= " <> toText oneDayAgo
                              -- <> " AND timestamp >= " <> convertTimestampToString ( startTimestamp )
                              -- <> " LIMIT 50000 "   --DEBUG only
                              <> " ; " )
                          (quickTick tickQueue )  --1562893200 is 1am on Mon 12th July 2019
                          -- (quickMidInto secID ohlcDataMap)
    trace ("< Reached the end of the db read > for " ++ unpack secID)  $ SQL3.close conn
    -- SQL3.close conn

    pPrint         $ "Completed read of 1 days back data for " <> secID <> " from " <> startDateText
    logInfo logger $ "Completed read of 1 days back data for " <> secID <> " from " <> startDateText
--               -- This routine consumes the ticks loaded in the SQL statement below, and populates a chart from the backdata loaded
-- _  <- forkIO $ do atomically $ receiveTickList secID tickQueue tickMapSV S.empty  --unless you fork, you get an infinite wait, as the SQL is only executed after this forked block of code

    --Get all the ticks loaded from the db
    backTicks <- atomically $ TBQ.flushTBQueue tickQueue
    pPrint $ Prelude.show $ length backTicks
    -- pPrint $ Prelude.show $ backTicks 
    -- Determine all OHLC Data, by running over all the ticks
    let mids = S.fromList $ convertSpreadToMid . fromJust <$> backTicks
    -- let backOD = zeroOD  --debug ONL|Y
    -- let backOD = foldl' (flip updateOD) zeroOD mids   --build up the OHLCData by running over each and every tick in sequence
    let backOD = backDataIntoOD (mids)
    -- pPrint $ show $ listOD backOD
    atomically $ TM.insertForce ohlcDataMap secID backOD   --and store it for retrieval later
    pPrint "Finished OHLC back data udpate" 
    pPrint         $ "Completed processing of 1 days back data for " <> secID <> " from " <> startDateText
    pure ()

--this just feeds the new price into updates of OHLC data for each time period (1sec through to 4hr).  It is actually dealt with elsewhere (in the PF update routine)
processPricesOHLC :: TradingEnvironment
                  -> TMVar AuthHeaders
                  -> TM.TMapMVar Text OHLCData
                  -> [Text]
                  -> Logger
                  -> TBQ.TBQueue CS.ByteString
                  -> Int
                  -> IO ()
processPricesOHLC !te !ah !ohlcDataMap !secIDs !logger !ohlcPriceQueue loopCounter = do    --bang patterns to eschew laziness, and remove any potential for hangs
            -- logInfo logger "Read new mid price for OHLC"
            len <- atomically $ TBQ.lengthTBQueue ohlcPriceQueue
            -- logInfo logger $ "Queue length=" <> show len 
            newPriceString <- atomically $ TBQ.readTBQueue ohlcPriceQueue  --wait for new price
            -- logInfo logger $ "New string " <> show newPriceString
            let (secIDD,newMid) = convertTextToTickField newPriceString
            let secIDmapped = mapEpic secIDD
            case (secIDmapped `elem` secIDs) of --for now, only a subset of the securities being streamed
              True -> do
                -- logInfo logger $ (show loopCounter) <> " " -- <> show len
                -- when (loopCounter > 100) $ liftIO $ do displayText <- atomically $ fmtOD ohlcDataMap secIDmapped
                --                                       -- logInfo logger displayText
                --                                       pure ()  --Don't do this!  it returns from the entire fcn, meaning it no longer iterates --do block requires a statement
                -- -- when (loopCounter > 10) $ liftIO $ do _ <- forkIO $ writeChartPageToFileFromMap secIDmapped chartMapSV "SV"
                let newLoopCounter = if loopCounter > 100 then 0 else loopCounter + 1 --reset the counter above 11, else keep counting
                -- logInfo logger $ "update mids...." <> show newPriceString
                atomically $ pushMids secIDmapped ohlcDataMap newMid
                -- logInfo logger $ show newPriceString <> "....update complete for " <> show newPriceString
                -- atomically $ trimChartMap 150 secIDmapped chartMapLV  --keep 999 columns only, ensure filesize doens't grow too big (my algorithm can't write that big a file with laziness)
                -- logInfo logger $ "Loop"
                processPricesOHLC te ah ohlcDataMap secIDs logger ohlcPriceQueue newLoopCounter --then loop with the updated paper trade
              False  -> do let newLoopCounter = loopCounter
                           processPricesOHLC te ah ohlcDataMap secIDs logger ohlcPriceQueue newLoopCounter --else loop with no update on the the paper trade

getRealPosnFromBroker :: TradingEnvironment -> TMVar AuthHeaders -> Logger -> SecurityID -> IO RealPosn
getRealPosnFromBroker te ah logger secID = do
   realPosns <- getAllPositions te ah logger
   pure $ pick1stRP secID realPosns


pick1stRP :: SecurityID -> [RealPosn] -> RealPosn
-- pick1stRP secID realPosns = case (filter (ep == rpEpic) realPosns) of
pick1stRP secID realPosns = case (filter (matchEpic ep) realPosns) of
                              []     -> emptyRealPosn ep  --so populate with 'empty' if no position found in lookup
                              rp : _ -> rp
                            where ep = Data.Text.drop 3 secID  --drop "L1:"
                                  matchEpic :: Text -> RealPosn -> Bool
                                  matchEpic ep1 rp = ep1 == ep2
                                    where ep2 = rpEpic rp

convertTextToTickField :: CS.ByteString -> (Text , MidPriceField)
convertTextToTickField newTickStr = do
   let [timestamp,secID,bid,offer,mktState] = words $ decodeUtf8 newTickStr
   let Right (ts,_) = decimal timestamp
   let Right (b,_) = double bid
   let Right (o,_) = double offer
   let newMid = convertSpreadToMid $ TickField ts b o mktState
   (secID,newMid)

-- initialZeroPosnMap :: TM.TMapMVar Text Position -> Text -> Double -> Double -> STM ()
-- initialZeroPosnMap chartMap secID blockSize reversal = do
--   TM.insertForce chartMap secID (PFChart (PointAndFigureSettings blockSize reversal secID) S.empty 0 0 0 0 0)

updateRealPosnMap :: TM.TMapMVar Text RealPosn -> RealPosn -> SecurityID -> STM ()
updateRealPosnMap realPosnMap rp secID =
  TM.insertForce realPosnMap secID rp

getRealPosnFromMap :: TM.TMapMVar Text RealPosn -> SecurityID -> STM RealPosn
getRealPosnFromMap realPosnMap secID = do
  rp <- TM.tryObserve realPosnMap secID
  return $ fromMaybe (emptyRealPosn secID) rp  --this must be initialised ; it assumes initialisation has occured
