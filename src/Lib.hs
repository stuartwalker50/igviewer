-- {-# LANGUAGE Strict #-}   --this doens't seem to improve exeuction speed
{-# LANGUAGE StrictData #-}  --this nearly halves the execution time
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude
import Data.Time.Clock.POSIX
import Data.Hourglass
import System.Hourglass
import Data.Text (replace)
import System.IO
-- import Data.Maybe
import Control.Concurrent  --has forkIO
import qualified Control.Concurrent.STM.TBQueue  as TBQ
import Data.Text.IO (hPutStrLn)
import Text.Pretty.Simple (pPrint)

data TickField = TickField { timestamp1 :: Integer  --this is what Elapsed is based on, Integer
                           , bid :: Double
                           , offer :: Double
                           , mktstate :: Text 
                           } deriving (Show,Eq)

type Timestamp = Integer
data MidPriceField = MidPriceField { timestamp2 :: Timestamp
                                   , mid :: Double
                                   } deriving (Show,Eq)
-- note getters are functions, hence timestamp1 and ..2 since can't have same field getter across different records

-- sample record: TickField 1.5172959e9 108.729 108.736
-- returns the MidPriceField 1.5172959e9 108.7325 -- which is (108.729+108.736)/2
convertSpreadToMid :: TickField -> MidPriceField
convertSpreadToMid tf = MidPriceField timestamp midPrice
  where midPrice = (bid tf + offer tf)/2
        timestamp = timestamp1 tf

convertTimestampToDateTime :: Seconds -> DateTime
convertTimestampToDateTime t = timeConvert (Elapsed t) :: DateTime

convertDateTimeToTimestamp :: DateTime -> Elapsed
convertDateTimeToTimestamp dt = ((timeConvert dt) :: Elapsed )  --find the unix timestamp string

convertDateTimeToTimestampString :: DateTime -> Text
convertDateTimeToTimestampString dt = replace "s" "" $ show ((timeConvert dt) :: DateTime )  --find the uinx timestamp string

convertTimestampToString :: Elapsed -> Text
convertTimestampToString dt = replace "s" "" $ show ((timeConvert dt) :: Elapsed )  --find the uinx timestamp string

showTime :: Integer -> String
-- showTime ts = fromString $ timePrint ISO8601_DateAndTime ((fromInteger ts)::Integer)
showTime ts = timePrint ISO8601_DateAndTime $ Elapsed $ fromInteger ts

type Logger = TBQ.TBQueue (Maybe Text)

newLoggerQueue :: STM (TBQ.TBQueue (Maybe Text))
newLoggerQueue = TBQ.newTBQueue 500  --small buffer to hold items while they're written to disk sequentially

createLogger :: Text -> IO (TBQ.TBQueue (Maybe Text))
createLogger fname = do
  newLogger <- atomically $ newLoggerQueue
  logInfo newLogger $ "Initialised logger " <> fname
  _ <- forkIO $ printLog fname newLogger --in a separate process, write the value just set to file
  -- printLog fname newLogger  --try it once
  return newLogger                  --return the log queue address so that items can be put onto the queue and then written in sequence

--write items off the queue to disk one at a time, to ensure log is kept as up to date as possible, in case of failure due to exception.  Queue is useful when multi-threading
printLog :: Text -> (TBQ.TBQueue (Maybe Text)) -> IO ()
printLog fname logger = do item <- atomically $ TBQ.tryReadTBQueue logger  --doesn't block, read a maybe value off the queue, of a maybe value
                           case item of
                             Nothing -> do threadDelay 20000  --0.2s wait before the next check
                                           printLog fname logger
                             Just x  -> do currentTime <- timeCurrent
                                           logFile <- openFile (toString fname) AppendMode
                                           let timeStr = toText $ timePrint ISO8601_DateAndTime currentTime
                                           case x of
                                             Just it -> do Data.Text.IO.hPutStrLn logFile $ timeStr <> " " <> it
                                                           hClose logFile
                                                           printLog fname logger  --loop until termination signalled
                                             Nothing -> do Data.Text.IO.hPutStrLn logFile $ timeStr <> " Close logger"
                                                           hClose logFile
                                                           return ()  --exit the inifite loop when a close-the-logger signal is received

logInfo :: (TBQ.TBQueue (Maybe Text)) -> Text -> IO ()
logInfo logger item = --do Prelude.putStrLn "Pushing value on the logger queue"
                         atomically $ TBQ.writeTBQueue logger (Just item)
                         -- return ()

closeLog :: (TBQ.TBQueue (Maybe Text)) -> IO ()
closeLog logger = do atomically $ TBQ.writeTBQueue logger (Nothing)
                     threadDelay 10000  --should be long enough for the final write to disk?
                     return ()
