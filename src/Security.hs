{-# LANGUAGE OverloadedStrings #-}

module Security where

import Prelude

import Lib

import Data.Time
-- import Data.Time.Zones
import Data.Time.Clock
import Data.Time.Clock.POSIX
-- import Data.Hourglass  --has elapsed time


slippage ep = case ep of   --for use in backtesting, want to set a high bar to see where real profits are being achieved that may be achieved furreal. Make it a penalty
          "L1:CS.D.EURGBP.MINI.IP"  -> 0.00009
          "L1:CC.D.CL.UMP.IP"      -> 2.8
          "L1:CC.D.LCO.UMP.IP"     -> 2.8
          "L1:CS.D.GBPUSD.MINI.IP" -> 0.0002
          "L1:CS.D.USDJPY.MINI.IP" -> 0.001     --it used to appear to be 0.007, however on 5th Mar '20 it spent more time at 0.01, often-times switching within each second
          "L1:CS.D.EURUSD.MINI.IP" -> 0.0001    --it's ranging between 0.0006 and 0.0009 on 10th Mar 2020
          "L1:KB.D.MID250.IFM.IP"  -> 44        --it changes between 43 and 44 at 10:18 on 17-Dec-19
          "L1:IX.D.NASDAQ.IFS.IP"   -> 2.5       --spread on 17:50 on Tues 7 April
          _                        -> 10

spread ep = case ep of
          "L1:CS.D.EURGBP.MINI.IP"  -> 0.00009
          "L1:CC.D.CL.UMP.IP"      -> 2.8
          "L1:CC.D.LCO.UMP.IP"     -> 2.8
          "L1:CS.D.GBPUSD.MINI.IP" -> 0.0002
          "L1:CS.D.USDJPY.MINI.IP" -> 0.010     --it used to appear to be 0.007, however on 5th Mar '20 it spent more time at 0.01, often-times switching within each second
          "L1:CS.D.EURUSD.MINI.IP" -> 0.00005    --it's ranging between 0.0006 and 0.0009 on 10th Mar 2020  --5e-5: going for finer resolution on signals
          "KB.D.MID250.IFM.IP"     -> 44        --it changes between 43 and 44 at 10:18 on 17-Dec-19
          "L1:IX.D.NASDAQ.IFS.IP"  -> 2.5       --spread on 17:50 on Tues 7 April
          _                        -> 10

multiplier ep = case ep of
          "L1:CS.D.EURGBP.MINI.IP" -> 10000
          "L1:CS.D.GBPUSD.MINI.IP" -> 10000
          "L1:CS.D.USDJPY.MINI.IP" -> 100
          "L1:CS.D.EURUSD.MINI.IP" -> 10000
          "KB.D.MID250.IFM.IP"     -> 2        --has a lotSize of 2, and a scaling factor of 1
          "L1:IX.D.NASDAQ.IFS.IP"  -> 1        --This is US Tech 100 Cash (£1)
          _                        -> 1

currency ep = case ep of
          "CS.D.EURGBP.MINI.IP" -> "GBP"
          "CC.D.CL.UMP.IP"      -> "GBP"
          "CC.D.LCO.UMP.IP"     -> "GBP"
          "CS.D.GBPUSD.MINI.IP" -> "USD"
          "CS.D.USDJPY.MINI.IP" -> "JPY"
          "CS.D.EURUSD.MINI.IP" -> "USD"
          "KB.D.MID250.IFM.IP"  -> "GBP"
          "IX.D.NASDAQ.IFS.IP"  -> "GBP"        --This is US Tech 100 Cash (£1)
          _                     -> "NoCCY"  --this is obviously wrong


  -- no longer used by Realtime.hs
lookupTradeSize ep = case ep of
          "L1:CS.D.EURGBP.MINI.IP" -> 1
          "L1:CS.D.GBPUSD.MINI.IP" -> 1
          "L1:CS.D.USDJPY.MINI.IP" -> 1
          "L1:CS.D.EURUSD.MINI.IP" -> 1
          "KB.D.MID250.IFM.IP"     -> 1        --has a lotSize of 2, and a scaling factor of 1
          "L1:IX.D.NASDAQ.IFS.IP"  -> 0.5      --This is US Tech 100 Cash (£1)
          _                        -> 1

-- [RealPosn {rpDealID = "DIAAAAC72EBVBAN", rpEpic = "KB.D.MID250.IFM.IP", rpSize = 1.0, rpOpenLevel = 21599.0, rpDir = SELL, rpLastPrice = 21593.0, rpStopLevel = Just 21749.0, rpLotSize = 2.0, rpScalingFactor = 1.0, rpCcy = "\"GBP\"", rpMktState = "TRADEABLE"}]

mapEpic :: Text -> Text
mapEpic ipEpic = strEpic
  where strEpic = epic
        epic = case ipEpic of
          "L1:CS.D.EURGBP.CFD.IP" -> "L1:CS.D.EURGBP.MINI.IP" 
          "L1:CC.D.CL.UNC.IP"     -> "L1:CC.D.CL.UMP.IP"  --map the $10 contract epic to the GBP1 contract epic
          "L1:CC.D.LCO.UNC.IP"    -> "L1:CC.D.LCO.UMP.IP"
          "L1:CS.D.GBPUSD.CFD.IP" -> "L1:CS.D.GBPUSD.MINI.IP"
          "L1:CS.D.USDJPY.CFD.IP" -> "L1:CS.D.USDJPY.MINI.IP"
          "L1:CS.D.EURUSD.CFD.IP" -> "L1:CS.D.EURUSD.MINI.IP"
          -- "CS.D.EURUSD.CFD.IP"       "CS.D.EURUSD.MINI.IP"
          "L1:KB.D.MID250.CASH.IP"-> "KB.D.MID250.IFM.IP"  --map the £10 to the £2
          "L1:IX.D.NASDAQ.IFS.IP" -> "L1:IX.D.NASDAQ.IFS.IP" --The same in the db and the smaller contract to trade
          _ -> ipEpic <> "argggg"

--Needed where you are trading a smaller contract than has been written to the db
inverseMapEpic :: Text -> Text
inverseMapEpic ipEpic = epic
  where strEpic = ipEpic
        epic = case strEpic of
          "L1:CS.D.EURGBP.MINI.IP" -> "L1:CS.D.EURGBP.CFD.IP"
          "L1:CC.D.CL.UMP.IP"      -> "L1:CC.D.CL.UNC.IP"      --un-map the $10 contract epic to the GBP1 contract epic 
          "L1:CC.D.LCO.UMP.IP"     -> "L1:CC.D.LCO.UNC.IP"
          "L1:CS.D.GBPUSD.MINI.IP" -> "L1:CS.D.GBPUSD.CFD.IP"
          "L1:CS.D.USDJPY.MINI.IP" -> "L1:CS.D.USDJPY.CFD.IP"
          "L1:CS.D.EURUSD.MINI.IP" -> "L1:CS.D.EURUSD.CFD.IP"
          "KB.D.MID250.IFM.IP"     -> "L1:KB.D.MID250.CASH.IP"
          "L1:IX.D.NASDAQ.IFS.IP"  -> "L1:IX.D.NASDAQ.IFS.IP" --The same in the db and the smaller contract to trade
          _ -> strEpic <> "argggg"


lookupBlockSize ep = case ep of
          "L1:CS.D.EURGBP.MINI.IP"  -> 0.000001  -- 0.00001  --0.00009
          "L1:CC.D.CL.UMP.IP"      -> 2.5 -- 2.8
          "L1:CC.D.LCO.UMP.IP"     -> 2.5 -- 2.8
          "L1:CS.D.GBPUSD.MINI.IP" -> 0.0002 -- 0.0002  -- 1x the tightest spread
          -- "L1:CS.D.USDJPY.MINI.IP" -> 0.01 -- 0.007
          "L1:CS.D.USDJPY.MINI.IP" -> 0.005 -- 0.007
          "L1:CS.D.EURUSD.MINI.IP" -> 0.00002  -- was 5 --spread is 0.00006 - 9
          "KB.D.MID250.IFM.IP"     -> 5  --just larger than the intraday spread
          -- "KB.D.MID250.IFM.IP"     -> 10  --just larger than the intraday spread
          -- "KB.D.MID250.IFM.IP"     -> 50  --just larger than the intraday spread
          "L1:IX.D.NASDAQ.IFS.IP"  -> 0.1 -- was 1  -- US Tech 100 Cash (£1)
          _                        -> 10  --unknown, for now
-- L1:KB.D.MID250.CASH.IP

-- getLocalTimeOffsetForSecurity :: Text -> Int -> IO Int
-- getLocalTimeOffsetForSecurity secID t = do 
--         tz <- loadTZFromDB (toString location)
--         pure $ diffForPOSIX tz (fromIntegral t)
--   where location = case secID of
--           "L1:CS.D.EURGBP.MINI.IP" -> "Europe/London"
--           "L1:CC.D.CL.UMP.IP"      -> "America/New_York"
--           "L1:CC.D.LCO.UMP.IP"     -> "Europe/London"
--           "L1:CS.D.GBPUSD.MINI.IP" -> "America/New_York"
--           "L1:CS.D.USDJPY.MINI.IP" -> "Asia/Tokyo"
--           "L1:CS.D.EURUSD.MINI.IP" -> "America/New_York"
--           "KB.D.MID250.IFM.IP"     -> "Europe/London"  --just larger than the intraday spread 
-- -- L1:KB.D.MID250.CASH.IP
--           "L1:IX.D.NASDAQ.IFS.IP"  -> "America/New_York" -- US Tech 100 Cash (£1)
--           _ -> secID <> "argggg"

--   --Don't need this one below as can't get out of Elapsed in the calling function, instead need to pull in the Int into Elapsed
-- getLocalTimeForSecurity :: Text -> Int -> IO Int
-- getLocalTimeForSecurity secID t = do 
--         tz <- loadTZFromDB (toString location)
--         -- pure $ diffForPOSIX tz (fromIntegral t)
--         -- pure $ diffForPOSIX tz (timeFromElapsed t)
--         -- pure $ diffForPOSIX tz (floor $ utcTimeToPOSIXSeconds t)  --works with UTCTime
--         pure $ diffForPOSIX tz $ fromIntegral t

--         -- pure $ utcToLocalTimeTZ tz (timeFromElapsed t)
--   where location = case secID of
--           "L1:CS.D.EURGBP.MINI.IP" -> "Europe/London"
--           "L1:CC.D.CL.UMP.IP"      -> "America/New_York"
--           "L1:CC.D.LCO.UMP.IP"     -> "Europe/London"
--           "L1:CS.D.GBPUSD.MINI.IP" -> "America/New_York"
--           "L1:CS.D.USDJPY.MINI.IP" -> "Asia/Tokyo"
--           "L1:CS.D.EURUSD.MINI.IP"  -> "America/New_York"
--           "KB.D.MID250.IFM.IP"     -> "Europe/London"  --just larger than the intraday spread 
-- -- L1:KB.D.MID250.CASH.IP
--           _ -> secID <> "argggg"

-- local2utcNaive :: LocalTime -> UTCTime
-- local2utcNaive (LocalTime day tod) = UTCTime day (timeOfDayToTime tod)

-- utc2int64Naive :: UTCTime -> Int64
-- utc2int64Naive ut = floor $ utcTimeToPOSIXSeconds ut


--Securites being tracked:
-- L1:CS.D.CFDSILVER.CFM.IP
-- L1:IX.D.NASDAQ.IFD.IP
-- L1:IX.D.SPTRD.IFD.IP
-- L1:CS.D.EURGBP.CFD.IP
-- L1:CS.D.CFDGOLD.CFM.IP
-- L1:CS.D.USDJPY.CFD.IP
-- L1:CS.D.GBPZAR.CFD.IP
-- L1:KB.D.MID250.CASH.IP
-- L1:IX.D.DAX.IFD.IP
-- L1:CS.D.USDCHF.CFD.IP
-- L1:CS.D.AUDUSD.CFD.IP
-- L1:IX.D.FTSE.CFD.IP
-- L1:CC.D.CL.UNC.IP
-- L1:CC.D.LCO.UNC.IP
-- L1:CS.D.EURUSD.CFD.IP
-- L1:CS.D.GBPUSD.CFD.IP
                    
