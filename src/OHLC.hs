module OHLC where

import qualified Prelude
import Prelude hiding (getLast,atomically)

-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromRow 
import qualified Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
--import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as S
import Data.Sequence((<|), (|>), (><), Seq(..), ViewL(..), ViewR(..) )
import qualified Data.Foldable as Foldable
import Text.Show.Functions
import Data.Time.Clock.POSIX
import Data.Int
import Data.Ord
import Data.Hourglass
import Data.List
import Data.Maybe  --contains fromMaybe
-- import Safe        --contains fromJustDef

import qualified Data.ByteString.Char8 as CS
-- import qualified Data.Text as T  --dupl. above
import qualified Data.Text.Read as TR
import qualified Control.Concurrent.STM.TMapMVar as TM
import Control.Concurrent  --has MVar
import Control.Concurrent.STM  --looking for STM type
import Control.Monad  --contains forM

-- import LibSettings
-- import Dhall as Dl
import qualified Control.Concurrent.STM.TBQueue  as TBQ

import Lib 
import Fmt
import Text.Show(show)

-- This module takes streaming price data throgh the following steps:
--
-- TickField - loaded from SQLite file
-- MidPriceField: by convertSpreadToMidField <*>
-- pairTimeOHLC - also convert to boxes here, ie. to minutes to pair with OHLC

tickTradeable :: TickField -> Bool
tickTradeable tick = mktstate tick == "TRADEABLE"

filterTickTradeable :: [TickField] -> [TickField]
filterTickTradeable tick = filter tickTradeable tick


-- type Timestamp = Integer
-- data MidPriceField = MidPriceField { timestamp2 :: Timestamp
--                                    , mid :: Double
--                                    } deriving (Show,Eq)
-- -- note getters are functions, hence timestamp1 and ..2 since can't have same field getter across different records

-- -- sample record: TickField 1.5172959e9 108.729 108.736
-- -- returns the MidPriceField 1.5172959e9 108.7325 -- which is (108.729+108.736)/2
-- convertSpreadToMid :: TickField -> MidPriceField
-- convertSpreadToMid tf = MidPriceField timestamp midPrice
--   where midPrice = (bid tf + offer tf)/2
--         timestamp = round $ timestamp1 tf


  -- tickStringToMid "1545156106 L1:CS.D.USDJPY.CFD.IP 112.584 112.591 TRADEABLE"
  -- @?= (" L1:CS.D.USDJPY.CFD.IP", MidPriceField 1545156106 (112.5875) )

-- tickStringToMid :: CS.ByteString -> (String, MidPriceField)
-- tickStringToMid newTickStr = (tick,newMid) where
--    [timestamp,tick,bid,offer,state] = words $ CS.unpack newTickStr
--    ts                                = read timestamp
--    newTick                           = TickField ts (read bid) (read offer) (T.pack state)
--    newMid                            = convertSpreadToMid newTick
--    -- tickk = drop 3 tick


data OHLC = OHLC {openPrice :: Double
                 ,highPrice :: Double
                 ,lowPrice :: Double
                 ,closePrice :: Double
                 } deriving (Show, Eq) --open/high/low/close

zeroOhlc = OHLC 0 0 0 0

updateOHLC :: MidPriceField -> OHLC -> OHLC
updateOHLC ipMid ipOhlc = OHLC o h l c
  where h = max midPrice (highPrice ipOhlc)
        l = if lo > 0
             then min midPrice lo
             else midPrice
        c = mid ipMid  --ie the last price we have is the close, until another is provided
        o = if op > 0
             then op
             else mid ipMid
        op = openPrice ipOhlc
        -- add if empty above, setting to zeroOHLC?
        lo = lowPrice ipOhlc
        midPrice = mid ipMid

-- work on the convention that ip2 has the midPrice as openPrice, and ip1 is a fully fledged OHLC
updateOHLCOHLC :: OHLC -> OHLC -> OHLC
updateOHLCOHLC ip1 ip2 = updateOHLC ip2open ip1
  where ip2open = MidPriceField 0 $ openPrice ip2  --the timestampe is immaterial here
--working on assumption that pre-existing OHLC will be the left arg, and new one will be the second argument

--take a timestamp and give me an integer repr. that increases by 1 for each minute
convertTimestampToIndex :: Integer -> Integer -> Integer -> Integer
convertTimestampToIndex periodSec tsStart tsNow = (diff `div` periodSec)  --integer div gives the rounding down
  where diff = (tsNow - tsStart)

restoreTimestampFromIndex :: Integer -> Integer -> Seconds
restoreTimestampFromIndex k tindx = (fromInteger (fromIntegral k * tindx)) :: Seconds
--restoreTimestampFromIndex k tindx = (Data.Hourglass.Elapsed ((1.0::Float)*(fromIntegral k) * tindx)) :: Elapsed
--restoreTimestampFromIndex k tindx = Elapsed $ toSeconds (k*tindx)

-- convertTimestampToDateTime :: Seconds -> DateTime
-- convertTimestampToDateTime t = timeConvert (Elapsed t) :: DateTime

convertTimestampToDate :: Seconds -> Date
convertTimestampToDate t = timeConvert (Elapsed t) :: Date

convertIndexToDateTime :: Integer -> Integer -> DateTime
convertIndexToDateTime k ti = convertTimestampToDateTime $ restoreTimestampFromIndex k ti

tZero1970 = DateTime (Date 1970 January 1) (TimeOfDay 0 0 0 0)
tZero = DateTime (Date 2016 January 1) (TimeOfDay 0 0 0 0)
tZero17 = DateTime (Date 2017 January 1) (TimeOfDay 0 0 0 0)
tZero18 = DateTime (Date 2018 January 1) (TimeOfDay 0 0 0 0)


-- instance Prelude.Show TimeOHLC where
--   show (TimeOHLC t o) = timePrint ISO8601_DateAndTime (convertTimestampToDateTime (fromInteger t)) ++ " " ++ Prelude.show o

data TimeOHLC = TimeOHLC { timestamp3 :: Timestamp, ohlc :: OHLC } deriving (Eq,Show) 
type SeqTimeOHLC = S.Seq TimeOHLC

zeroTimeOHLC :: TimeOHLC
zeroTimeOHLC = TimeOHLC 0 zeroOhlc

zeroSeqTimeOHLC :: SeqTimeOHLC
zeroSeqTimeOHLC = S.fromList [TimeOHLC 0 zeroOhlc]

--Build out the sequence of TimeOHLCs:
--1. In same timestamp bucket, update the OHLC
--2. Pad out the timestamps with prior OHLCs if a few are skipped
--3. Start a new bucket
insertMids :: Integer -> SeqTimeOHLC ->  MidPriceField -> SeqTimeOHLC
insertMids k acc newMid
    |    newTimestamp == lastTimestamp = xs |> updateInPlace
    | newTimestamp > expectedTimestamp = insertMids k (acc |> padTOHLC) newMid  -- new timestamp, same old OHLC
    |                        otherwise = acc |> newTOHLC--plus a new one
    where xs :> x = S.viewr acc  --eff. take the tail of the seq into x
          newTimestamp  = k * convertTimestampToIndex k 0 ( timestamp2 newMid)
          lastTimestamp = if timestamp3 x == 0
                           then newTimestamp  --if it's a zero as 'empty' to start the seq, then overwrite it (otherwise it fills in millions of minutes from 1970); need to convert it in case it's a nonzero start to the sequence, which can happen if not building from zeroOHLC
                           else k * convertTimestampToIndex k 0 ( timestamp3 x)
          expectedTimestamp = lastTimestamp + k
          lastOHLC = ohlc x
          updateInPlace = TimeOHLC lastTimestamp (updateOHLC newMid lastOHLC)
          padTOHLC = TimeOHLC expectedTimestamp lastOHLC
          newTOHLC = TimeOHLC newTimestamp (updateOHLC newMid zeroOhlc)

-------------
resample :: Integer -> SeqTimeOHLC -> SeqTimeOHLC
resample k timeFreqShorterTimeBase = foldl (insertMids k ) (S.fromList [zeroTimeOHLC]) (convertTOHLCToMids timeFreqShorterTimeBase)
  -- take eg a 1min series, and resample it to eg. 5mins
  where
    convertTOHLCToMids :: SeqTimeOHLC -> [MidPriceField]
    convertTOHLCToMids sqnc
      | S.length sqnc == 0 =  []
      | S.length sqnc >= 1 =  MidPriceField timestamp (openPrice x) :
                              MidPriceField timestamp (highPrice x) :
                              MidPriceField timestamp (lowPrice x) :
                              MidPriceField timestamp (closePrice x) :
                              convertTOHLCToMids xs where
                                x' :<| xs = sqnc  --eff. take the first element of the seq into x'
                                x = ohlc x'
                                timestamp = timestamp3 x'

joinPriceSeqs :: SeqTimeOHLC -> SeqTimeOHLC -> SeqTimeOHLC
joinPriceSeqs baseSeq newSeq
 | (S.length baseSeq == 0) && (S.length newSeq == 0) = S.empty
 | (S.length baseSeq == 0) && (S.length newSeq  > 0) = newSeq
 | (S.length baseSeq  > 0) && (S.length newSeq == 0) = baseSeq
 --can assume both seq's have at least one element
 | (timestamp3 baseLastElem) == ( timestamp3 newFirstElem) = baseFirstBit >< combine baseLastElem newFirstElem >< restNewSeq   -- if single overlap, combine that ohlc
 | (timestamp3 baseLastElem)  < ( timestamp3 newFirstElem) = baseSeq >< newSeq                                        --if no overlap then you're good, just join them
 | (timestamp3 baseLastElem)  > ( timestamp3 newFirstElem) = joinPriceSeqs baseSeq restNewSeq  --discard elems of newseq that are before baseseq; need them to overlap
 -- if S.length newSeq < 2 --need to split into 2 elems
  --   then baseSeq  --not enough input data to update
  --   else case compare baseSeqLastTimestamp newSeqTimestamp of
  --           LT -> 
  --           GT -> 
  --           EQ -> baseFirstBit >< combine baseLastElem newFirstElem >< restNewSeq
  where baseFirstBit :|> baseLastElem = baseSeq  --eff. take the first element of the seq into x'
        newFirstElem :<| restNewSeq   = newSeq   --eff. take the first element of the seq into x'
        -- baseSeqLastTimestamp = timestamp3 $ baseLastElem

combine :: TimeOHLC -> TimeOHLC -> SeqTimeOHLC
combine s1 s2 = S.fromList [TimeOHLC (timestamp3 s1) $
  foldl (flip updateOHLC) (ohlc s1) [ MidPriceField s2Timestamp (openPrice  s2')
                                    , MidPriceField s2Timestamp (highPrice  s2')
                                    , MidPriceField s2Timestamp (lowPrice   s2')
                                    , MidPriceField s2Timestamp (closePrice s2')
                                    ]
                          ]
    where s2'          = ohlc s2
          s2Timestamp  = timestamp3 $ s2


-- updateOHLC ipMid ipOhlc = OHLC o h l c

inSamePeriod :: SeqTimeOHLC -> SeqTimeOHLC -> Bool
inSamePeriod sShort sLong =
  timestamp3 firstElemShort <= timestamp3 firstElemLong
  where firstElemShort :<| _ = sShort
        firstElemLong  :<| _ = sLong

inNextPeriod :: Integer -> SeqTimeOHLC -> SeqTimeOHLC -> Bool
inNextPeriod k sShort sLong =
  timestamp3 firstElemShort < k + timestamp3 firstElemLong &&  --mustn't be more than 1 bar/candle handle
  timestamp3 firstElemShort >= timestamp3 firstElemLong        --must also be equal or ahead of the longer one
  where firstElemShort :<| _ = sShort
        firstElemLong  :<| _ = sLong


--intended to bring seqShort up to the same date as seqLong, since it will always start earlier as uses fewer elems to calc
bringStartInLineWith :: SeqTimeOHLC -> SeqTimeOHLC -> SeqTimeOHLC
bringStartInLineWith s1 s2 = if startDateEarlier s1 s2 then bringStartInLineWith (S.drop 1 s1) s2
                                                       else s1

nthInNextPeriod :: Int -> Integer -> SeqTimeOHLC -> SeqTimeOHLC -> Bool
nthInNextPeriod n k sShort sLong = if S.length sShort >= (n-1) && S.length sLong >= (n-1)     --(n-1) as S.drop is zero-based ie. first element is 0th
                                    then timestamp3 nthElemShort - (k-1)*60 > timestamp3 nthElemLong   --more than 1 bar/candle handle ahead
                                    -- then timestamp3 nthElemShort >= (k-1) + timestamp3 nthElemLong   --more than 1 bar/candle handle ahead
                                    else False  --not enough data left?
  -- timestamp3 nthElemShort >= k + timestamp3 nthElemLong   --more than 1 bar/candle handle ahead
  -- timestamp3 nthElemShort >= timestamp3 nthElemLong        --must also be equal or ahead of the longer one
  where nthElemShort = sShort `S.index` (n-1)
        nthElemLong  = sLong `S.index` (n-1)

startDateEarlier s1 s2 = timestamp3 firstElemShort < timestamp3 firstElemLong
  where firstElemShort :<| _ = s1
        firstElemLong  :<| _ = s2

--intended to bring seqShort up to the same date as seqLong, since it will always start earlier as uses fewer elems to calc
bringStartInLineWithOffset :: Integer -> SeqTimeOHLC -> SeqTimeOHLC -> SeqTimeOHLC
bringStartInLineWithOffset k s1 s2 = S.drop adjRequired s1 
  where targetDate s = timestamp3 $ s `S.index` fromIntegral (k - 1)  --40 elements hence
        diff = targetDate s2 - targetDate s1
        adjRequired :: Int
        adjRequired = if diff > 0 then fromIntegral diff::Int else 0
-------------

closingPrices :: Seq TimeOHLC -> Seq TimeVal
closingPrices ohlcs = S.zipWith TimeVal (timestamp3 <$> ohlcs) (closePrice . ohlc <$> ohlcs)

openingPrices :: Seq TimeOHLC -> Seq TimeVal
openingPrices ohlcs = S.zipWith TimeVal (timestamp3 <$> ohlcs) (openPrice . ohlc <$> ohlcs)


-- processFile :: String -> Integer -> IO ()
-- processFile fname k = do
-- -- k : bucket size
-- -- tickCount : how many records to retrieve from the database 
--   ticks <- readDB fname
--   -- if length ticks > 6600 
--   --  then let ticks = drop ((length ticks) - 6000) ticks  --just keep the most recent 6000 ticks
--   let tradeableTicks = filterTickTradeable ticks
--   let mids = convertSpreadToMid <$> tradeableTicks
--   let ohlcs = foldl (insertMids k) (S.fromList [zeroTimeOHLC]) mids
--   putStr $ "Number of ticks : " 
--   putStrLn $ show $ length mids
--   print "Last 5 min prices"
--   print $ S.drop ((length ohlcs) - 5) ohlcs
--   --let closePrices = S.zipWith TimeVal (timestamp3 <$> ohlcs) (closePrice <$> ohlc <$> ohlcs)
--   let (bollLo,bollMid,bollUp) = bollingerBands 20 2 $ closingPrices ohlcs
--   print "Last 5 min bollLower"
--   print $ S.drop ((length ohlcs) - 5) bollLo
--   print "Last 5 min bollMid"
--   print $ S.drop ((length ohlcs) - 5) bollMid
--   print "Last 5 min bollUpper"
--   print $ S.drop ((length ohlcs) - 5) bollUp



-- --  foldl over mids, appending to the data seq from nil, but updateOHLC the last timestamp if it is the same
-- --  let ohlcs = convertMidToOHLCpair k <$> mids
-- ----  let ohlcs2 = foldIntoOHLConPeriod ohlcs
-- ----  let str = IntMap.foldlWithKey' (convertMTOtoStr k) "" ohlcs2
-- --  putStrLn str
-- --  now what?  map or vector.  Just stick to lists

-- --  let ohlcs = listToSTUArray ticks

-- --  convertSpreadToMid <$> xs >>= head >>= putStr
--   putStrLn "Done."


  --- ********** ---
  --- Indicators ---
  --- ********** ---

data TimeVal = TimeVal { timestamp4 :: Integer, val :: Double } deriving (Eq,Ord,Show) 
type SeqTimeVal = S.Seq TimeVal


-- instance Show TimeVal where
--   show (TimeVal t v) = timePrint ISO8601_DateAndTime (convertTimestampToDateTime (fromInteger t)) ++ "," ++ show v

-- mean :: [Float] -> Float

-- sumSeq :: SeqTimeVal -> Float
-- sumSeq ip = foldl (+) 0.0 (fmap val ip)

mean ip = (foldl (+) 0 ip) / (realToFrac . length) ip

-- simple moving average, working right to left  (easier to zero-pad the front this way)
sma :: Int -> SeqTimeVal -> SeqTimeVal
sma _ S.Empty = S.Empty   -- never hits this line
sma n ip = if length ip >= n
            then sma n remainingVals :|> newSmaVal     --insert a new TimeVal at the end of the seq with :|>
            else zeroVals  --termination point
            -- else S.replicate (n-1) 0.0   --terminate recursion with zero-padded list of n-1 elements 
   where window = S.drop ((length ip) - n) ip  --select (take) just the vals used in the calc for this iteration
         windowVals = fmap val window  --extract the val field from the TimeSeq
         windowTimes = fmap timestamp4 window  --extract the timestamp field from the TimeSeq 
         newSmaVal = TimeVal (S.index windowTimes (n-1) ) (mean windowVals)
         zeroVals = S.zipWith TimeVal windowTimes (S.replicate n 0.0)  --zero-pad the beginning of the sequence, where no values can be calculated 
         remainingVals :|> _ = ip  -- take all but the rightmost value
  
--use in a loop to add the diffs to create stdDev
squareDiff :: Double -> Double -> Double
squareDiff avg x = (avg - x) ^ 2


-- round function for use in the test module, spec:
roundToDecPlaces n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)


roundTVToDecPlaces :: Int -> TimeVal -> TimeVal
roundTVToDecPlaces n tv = tv {val = roundToDecPlaces n (val tv)}

roundToDecPlaceSeq3Ways :: Int -> (SeqTimeVal, SeqTimeVal, SeqTimeVal) -> (SeqTimeVal, SeqTimeVal, SeqTimeVal)
roundToDecPlaceSeq3Ways n (s1,s2,s3) = (roundTVToDecPlaces n <$> s1, roundTVToDecPlaces n <$> s2, roundTVToDecPlaces n <$> s3 )

roundToDecPlaceSeq7Ways :: Int -> (SeqTimeVal, SeqTimeVal, SeqTimeVal,SeqTimeVal,SeqTimeVal, SeqTimeVal, SeqTimeVal) -> (SeqTimeVal, SeqTimeVal, SeqTimeVal,SeqTimeVal,SeqTimeVal, SeqTimeVal, SeqTimeVal)
roundToDecPlaceSeq7Ways n (s1,s2,s3,s4,s5,s6,s7) = ( roundTVToDecPlaces n <$> s1, roundTVToDecPlaces n <$> s2, roundTVToDecPlaces n <$> s3
                                                   , roundTVToDecPlaces n <$> s4, roundTVToDecPlaces n <$> s5, roundTVToDecPlaces n <$> s6
                                                   , roundTVToDecPlaces n <$> s7 )

-- Return a 3-tuple with (bollLower,bollMid,bollUpper)
bollingerBands :: Int -> Int -> SeqTimeVal -> (SeqTimeVal, SeqTimeVal, SeqTimeVal) 
bollingerBands smaK bandMult ip = if length ip >= smaK  
  then (seqLo :|> newLo, seqMid :|> newMid, seqHi :|> newHi)
  else (zeroVals,zeroVals,zeroVals)  --zero-pad the beginning  [termination clause for the recursive defn]
  where 
    --iterate for earlier vals in the sequence
    (seqLo,seqMid,seqHi) = bollingerBands smaK bandMult remainingVals 
    --find the window of vals for this iteration
    window = S.drop ((length ip) - smaK) ip  --select (take) just the vals used in the calc for this iteration
    windowVals = fmap val window  --extract the val field from the TimeSeq
    windowTimes = fmap timestamp4 window  --extract the timestamp field from the TimeSeq 
    _ :|> timeInThisIteration = windowTimes --take most recent timestamp
    --and for the next iteration, or terminating the iteration
    remainingVals :|> rightMostIPval = ip  -- take all but the rightmost value 
    zeroVals = S.zipWith TimeVal windowTimes (S.replicate smaK 0.0)  --zero-pad the beginning of the sequence, where no values can be calculated
    --now calc the values this time round
    newMidVal = val $ S.index (sma smaK window) (smaK - 1)  --take the very last value in the sequence, it's the one we need for this iteration  ##opportunity for optimisation here?, or will the compiler find it?
    stdDev = sqrt $ summedElements / fromIntegral smaK
    avg = mean windowVals
    summedElements = foldl (+) 0.0 (fmap (squareDiff avg) windowVals)
    newMid = TimeVal timeInThisIteration newMidVal
    newLo = TimeVal timeInThisIteration (newMidVal - 2 * stdDev)
    newHi = TimeVal timeInThisIteration (newMidVal + 2 * stdDev)
    -- newMid = TimeVal timeInThisIteration $ roundToDecPlaces dp newMidVal
    -- newLo = TimeVal timeInThisIteration $ roundToDecPlaces dp (newMidVal - 2 * stdDev)
    -- newHi = TimeVal timeInThisIteration $ roundToDecPlaces dp (newMidVal + 2 * stdDev)

getLast :: Seq b -> (b, b)   --it's for any sequence, not just SeqTOHLC, but also SeqTimeVal 
getLast ipSeq = (ipNm1,ipN) where
  remainingIPvals :|> ipNm1 :|> ipN = ipSeq

getLastTriple :: Seq b -> (b, b, b)   --it's for any sequence, not just SeqTOHLC, but also SeqTimeVal 
getLastTriple ipSeq = (ipNm2,ipNm1,ipN) where
  remainingIPvals :|> ipNm2 :|> ipNm1 :|> ipN = ipSeq

getLastQuad :: Seq b -> (b,b, b, b)   --it's for any sequence, not just SeqTOHLC, but also SeqTimeVal 
getLastQuad ipSeq = (ipNm3,ipNm2,ipNm1,ipN) where
  remainingIPvals :|> ipNm3 :|> ipNm2 :|> ipNm1 :|> ipN = ipSeq

-- getLast3 :: (Seq b0, Seq b1, Seq b2) -> ((b0,b0), (b1,b1), (b2,b2))   --it's for any sequence, not just SeqTOHLC, but also SeqTimeVal 
getLast3 (ipSeqA,ipSeqB,ipSeqC) = (getLast ipSeqA, getLast ipSeqB, getLast ipSeqC)
-- getLast3 ipSeqA ipSeqB ipSeqC = ((ipNm1a,ipNa),(ipNm1a,ipNa),(ipNm1a,ipNa)) where
--   remainingIPvalsA :|> ipNm1a :|> ipNa = ipSeqA
--   remainingIPvalsB :|> ipNm1b :|> ipNb = ipSeqB
--   remainingIPvalsC :|> ipNm1c :|> ipNc = ipSeqC

getLast3Triple (ipSeqA,ipSeqB,ipSeqC) = (getLastTriple ipSeqA, getLastTriple ipSeqB, getLastTriple ipSeqC)

-- type SecMapData = (Int,SeqTimeOHLC)

processNewPrice :: Integer -> Text -> MidPriceField -> TM.TMapMVar Text SeqTimeOHLC -> STM ()
processNewPrice k !secID !newMid !secMap = do
    lastValue <- TM.tryLookup secMap secID  --returns STM
    case lastValue of  --note, tryLookup deletes the key-value if successful
      Nothing -> TM.insert secMap secID $ insertMids k zeroSeqTimeOHLC newMid
      Just existingTimeSeq -> TM.insert secMap secID $ insertMids k existingTimeSeq newMid  --insert with x when map reverts to seqTOHLC
    return ()

showTOHLCLength :: Text -> TM.TMapMVar Text SeqTimeOHLC -> STM Text
showTOHLCLength !secID !secMap = do
    lastValue <- TM.tryObserve secMap secID  --returns STM
    case lastValue of  --note, tryLookup deletes the key-value if successful
      Nothing -> return $ "Nothing for " <> secID
      Just existingTimeSeq -> return $ ""+|S.length existingTimeSeq|+""
    --return ()

showFirstAndLastDate :: Text -> TM.TMapMVar Text SeqTimeOHLC -> STM Text
showFirstAndLastDate !secID !secMap = do
    maybeTimeSeq <- TM.tryObserve secMap secID  --returns STM
    case maybeTimeSeq of  --note, tryLookup deletes the key-value if successful
      Nothing -> return $ "Nothing for " <> secID
      Just timeSeq -> do let firstElem :<| _ = timeSeq
                         let _ :|>  lastElem  = timeSeq
                         return $ ""+|secID|+" "+|Text.Show.show firstElem|+"  -  "+|Text.Show.show lastElem|+""

showLatest :: Text -> TM.TMapMVar Text SeqTimeOHLC -> STM Text
showLatest !secID !secMap = do
    maybeTimeSeq <- TM.tryObserve secMap secID  --returns STM
    case maybeTimeSeq of  --note, tryLookup deletes the key-value if successful
      Nothing -> return $ "Nothing for " <> secID
      Just timeSeq -> if S.length timeSeq >= 40 then do let len = S.length timeSeq
                                                        let timeSeqCrop = S.drop (len-40) timeSeq
                                                        let (secondLastPrice, lastPrice) = getLast timeSeq
                                                        let ((bollLo2last,bollLoLast), (bollMid2last,bollMidLast), (bollHi2last,bollHiLast)) = getLast3 $ bollingerBands 20 2 $ closingPrices timeSeqCrop
                                                        return $ ""+|secID|+" " +|Text.Show.show secondLastPrice|+
                                                                          " 2BollL/M/H " +|Text.Show.show bollLo2last|+" ; " 
                                                                           +|Text.Show.show bollMid2last|+" ; " 
                                                                           +|Text.Show.show bollHi2last |+"\n"
                                                else return $ ""+|secID|+" not long enough: "+|(S.length timeSeq)|+""


findEndOfPeriodIndex :: Int -> Int -> SeqTimeOHLC -> Int
findEndOfPeriodIndex k count ipSeq =
  if (timestamp3 (S.index ipSeq 1)) `mod` (fromIntegral k) == 0
   then count
   else if S.length ipSeq > 20
           then findEndOfPeriodIndex k (count + 1) (S.drop 1 ipSeq)
           else count  --leave at least 20 elements for last section

-- splitIntoTimePeriods :: [Int] -> SeqTimeOHLC -> [SeqTimeOHLC]
-- splitIntoTimePeriods periods ipSeq
--   | periods == []        = []
--   | otherwise            = (S.drop (S.length ipSeq - (head periods)) ipSeq) : splitIntoTimePeriods (tail periods) ipSeq
--   -- forM_ periods $ \k -> do
--   --   if S.length ipSeq < k then [ipSeq]
--   --                         else thisSeq : splitIntoRegularTimePeriods k nextSeq where
--   --                           (thisSeq :: SeqTimeOHLC , nextSeq) = S.splitAt (fromIntegral k) ipSeq

splitIntoRegularTimePeriods :: Int -> SeqTimeOHLC -> [SeqTimeOHLC]
splitIntoRegularTimePeriods k ipSeq =
  if S.length ipSeq < k then [ipSeq]
                        else thisSeq : splitIntoRegularTimePeriods k nextSeq where
                          (thisSeq :: SeqTimeOHLC , nextSeq) = S.splitAt (fromIntegral k) ipSeq

chunkUp :: Int -> SeqTimeOHLC -> [SeqTimeOHLC]
chunkUp period ipSeq = listOfTimeSlices  where
  n = findEndOfPeriodIndex period 0 ipSeq
  (thisSeq , restSeq) = S.splitAt (fromIntegral n) ipSeq
  listOfTimeSlices = thisSeq : splitIntoRegularTimePeriods period restSeq


---------------------------------------------------
-- | Functions to manage OHLC data for realtime.hs
---------------------------------------------------

data OHLCDataPoint = OHLCDataPoint { odpSecs   :: Integer          --no of secs in each ohlc bar
                                   , odpSeq    :: SeqTimeOHLC  --series of 20 (21?) bars to permit ma and sdev to be calculated
                                   , odpLastP  :: Double       --last price
                                   , odpBollLo :: Double       --last BollLo (completed, ie. not the currently forming bar)
                                   , odpMA     :: Double       --last MA     (completed, ie. not the currently forming bar)
                                   , odpBollUp :: Double       --last BollHi (completed, ie. not the currently forming bar)
                                   , odpMAgrad :: Double       --last MA     (completed, ie. not the currently forming bar)
                               , timestamp5:: Timestamp    --timestamp values were last updated at
                                   } deriving (Eq)
instance Show OHLCDataPoint where
  show (OHLCDataPoint k _ lastP bLo bMid bUp bpMidgrad ts) = ""+|timePrint ISO8601_DateAndTime (convertTimestampToDateTime (fromInteger ts))|+""
instance Buildable OHLCDataPoint where
  build (OHLCDataPoint k _ lastP bLo bMid bUp bMidgrad ts) = "" +|fixedF 1 lastP|+
                                                    " "+|fixedF 1 bLo  |+
                                                    " "+|fixedF 1 bMid |+
                                                    " "+|fixedF 1 bMidgrad |+
                                                    " "+|fixedF 1 bUp  |+
                                                    ""
-- zeroOHLCDataPoint = OHLCDataPoint 
data OHLCData = OHLCData { od1s    :: OHLCDataPoint
                         , od2s    :: OHLCDataPoint
                         , od3s    :: OHLCDataPoint
                         , od10s   :: OHLCDataPoint
                         , od30s   :: OHLCDataPoint
                         , od1m    :: OHLCDataPoint
                         , od2p5m  :: OHLCDataPoint  --2.5m, ie. two Point five
                         , od5m    :: OHLCDataPoint
                         , od10m   :: OHLCDataPoint
                         , od15m   :: OHLCDataPoint
                         , od30m   :: OHLCDataPoint
                         , od1h    :: OHLCDataPoint
                         , od2h    :: OHLCDataPoint
                         , od4h    :: OHLCDataPoint
                         } deriving (Eq,Show)

zeroOD :: OHLCData
zeroOD = OHLCData (OHLCDataPoint 1     zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --1 sec
                  (OHLCDataPoint 2     zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --3s
                  (OHLCDataPoint 3     zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --3s
                  (OHLCDataPoint 10    zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --10s
                  (OHLCDataPoint 30    zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --30s
                  (OHLCDataPoint 60    zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --1m
                  (OHLCDataPoint 150   zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --2.5m
                  (OHLCDataPoint 300   zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --5m
                  (OHLCDataPoint 600   zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --10m
                  (OHLCDataPoint 900   zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --15m
                  (OHLCDataPoint 1800  zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --30m
                  (OHLCDataPoint 3600  zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --1h
                  (OHLCDataPoint 7200  zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --2h
                  (OHLCDataPoint 14400 zeroSeqTimeOHLC 0.0 0.0 0.0 0.0 0 0)  --4h 

initialiseOHLCDataMap :: TM.TMapMVar Text OHLCData -> Text -> STM ()
initialiseOHLCDataMap !ohlcDataMap secID = do
  let ohlcData = zeroOD
  TM.insertForce ohlcDataMap secID ohlcData

pushMids :: Text -> TM.TMapMVar Text OHLCData -> MidPriceField -> STM ()
pushMids !secID !ohlcDataMap !newMid = do 
  od <- TM.lookup ohlcDataMap secID  --returns STM  ;  Blocks, and observe *doesn't* delete upon looking it up
  let newOD = updateOD newMid od 
  TM.insertForce ohlcDataMap secID newOD

updateOD :: MidPriceField -> OHLCData -> OHLCData
updateOD !newMid !od = od { od1s    = updateODPoint (od1s   od) newMid 
                          , od2s    = updateODPoint (od2s   od) newMid 
                          , od3s    = updateODPoint (od3s   od) newMid 
                          , od10s   = updateODPoint (od10s  od) newMid 
                          , od30s   = updateODPoint (od30s  od) newMid 
                          , od1m    = updateODPoint (od1m   od) newMid 
                          , od2p5m  = updateODPoint (od2p5m od) newMid 
                          , od5m    = updateODPoint (od5m   od) newMid 
                          , od10m   = updateODPoint (od10m  od) newMid 
                          , od15m   = updateODPoint (od15m  od) newMid 
                          , od30m   = updateODPoint (od30m  od) newMid 
                          , od1h    = updateODPoint (od1h   od) newMid 
                          , od2h    = updateODPoint (od2h   od) newMid 
                          , od4h    = updateODPoint (od4h   od) newMid 
                          } 

--take a list of backdata mids and ram them into an initial OD without calc'ing the intermediate mas/bollingers the whole way through - reduce time to load
backDataIntoOD :: S.Seq MidPriceField -> OHLCData
backDataIntoOD (mids :|> lastMid) = newOD where
  lastTS = timestamp2 lastMid
  -- find the first timestamp reqd to prepopulate the TimeOHLC series  (21 OHLC bars, to permit 20 bar calc of ma's and bollingers)
  k1s = (odpSecs $ od1s zeroOD)
  k2s = (odpSecs $ od2s zeroOD) 
  k3s = (odpSecs $ od3s zeroOD)
  k10s  = (odpSecs $ od10s zeroOD)
  k30s  = (odpSecs $ od30s zeroOD)
  k1m   = (odpSecs $ od1m  zeroOD)
  k2p5m = (odpSecs $ od2p5m zeroOD) 
  k5m   = (odpSecs $ od5m  zeroOD)
  k10m  = (odpSecs $ od10m zeroOD)
  k15m  = (odpSecs $ od15m  zeroOD)
  k30m  = (odpSecs $ od30m  zeroOD) 
  k1h   = (odpSecs $ od1h   zeroOD)
  k2h   = (odpSecs $ od2h   zeroOD)
  k4h   = (odpSecs $ od4h   zeroOD)
  firstTSod1s   = lastTS - 21 * k1s 
  firstTSod2s   = lastTS - 21 * k2s
  firstTSod3s   = lastTS - 21 * k3s  
  firstTSod10s  = lastTS - 21 * k10s  
  firstTSod30s  = lastTS - 21 * k30s  
  firstTSod1m   = lastTS - 21 * k1m  
  firstTSod2p5m = lastTS - 21 * k2p5m  
  firstTSod5m   = lastTS - 21 * k5m 
  firstTSod10m  = lastTS - 21 * k10m 
  firstTSod15m  = lastTS - 21 * k15m 
  firstTSod30m  = lastTS - 21 * k30m 
  firstTSod1h   = lastTS - 21 * k1h 
  firstTSod2h   = lastTS - 21 * k2h
  firstTSod4h   = lastTS - 21 * k4h 
  os1s = foldl' (insertMids k1s) zeroSeqTimeOHLC (S.filter (afterTS firstTSod1s) mids) 
  os2s = foldl' (insertMids k2s) zeroSeqTimeOHLC (S.filter (afterTS firstTSod2s) mids) 
  os3s = foldl' (insertMids k3s) zeroSeqTimeOHLC (S.filter (afterTS firstTSod3s) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os10s = foldl' (insertMids k10s) zeroSeqTimeOHLC (S.filter (afterTS firstTSod10s) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os30s = foldl' (insertMids k30s) zeroSeqTimeOHLC (S.filter (afterTS firstTSod30s) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os1m  = foldl' (insertMids k1m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod1m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os2p5m = foldl' (insertMids k2p5m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod2p5m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os5m  = foldl' (insertMids k5m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod5m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os10m = foldl' (insertMids k10m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod10m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os15m = foldl' (insertMids k15m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod15m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os30m = foldl' (insertMids k30m) zeroSeqTimeOHLC (S.filter (afterTS firstTSod30m) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os1h = foldl' (insertMids k1h) zeroSeqTimeOHLC (S.filter (afterTS firstTSod1h) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os2h = foldl' (insertMids k2h) zeroSeqTimeOHLC (S.filter (afterTS firstTSod2h) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  os4h = foldl' (insertMids k4h) zeroSeqTimeOHLC (S.filter (afterTS firstTSod4h) mids)   --by converting the OHLC bars back to mids, there are many, many fewer to process
  -- os4h = insertMids k4h zeroSeqTimeOHLC <$> ( (S.filter (betweenTS firstTSod4h firstTSod2h) mids) >< S.fromList ( convertTOHLCToMids os2h ) )   --by converting the OHLC bars back to mids, there are many, many fewer to process
  newOD = updateOD lastMid $ zeroOD { od1s    = OHLCDataPoint k1s os1s   0 0 0 0 0 0   --inserting the very last mid will cause the ma's and bollingers to be calc'd across all time frames
                                    , od2s    = OHLCDataPoint k2s os2s   0 0 0 0 0 0
                                    , od3s    = OHLCDataPoint k3s os3s   0 0 0 0 0 0 
                                    , od10s   = OHLCDataPoint k10s os10s 0 0 0 0 0 0
                                    , od30s   = OHLCDataPoint k30s os30s 0 0 0 0 0 0
                                    , od1m    = OHLCDataPoint k1m  os1m  0 0 0 0 0 0
                                    , od2p5m  = OHLCDataPoint k2p5m  os2p5m  0 0 0 0 0 0
                                    , od5m    = OHLCDataPoint k5m    os5m    0 0 0 0 0 0
                                    , od10m   = OHLCDataPoint k10m   os10m   0 0 0 0 0 0
                                    , od15m   = OHLCDataPoint k15m   os15m   0 0 0 0 0 0
                                    , od30m   = OHLCDataPoint k30m   os30m   0 0 0 0 0 0
                                    , od1h    = OHLCDataPoint k1h    os1h    0 0 0 0 0 0
                                    , od2h    = OHLCDataPoint k2h    os2h    0 0 0 0 0 0
                                    , od4h    = OHLCDataPoint k4h    os4h    0 0 0 0 0 0
                                    }


  afterTS :: Timestamp -> MidPriceField -> Bool
  afterTS ts m = ts <= timestamp2 m
  betweenTS :: Timestamp -> Timestamp -> MidPriceField -> Bool
  betweenTS ts1 ts2 m = ts1 <= timestamp2 m
                     && ts2 >= timestamp2 m

      -- Just od -> do let newOD = od { od1s    = updateODPoint (od1s   od) newMid 
      --                              , od10s   = updateODPoint (od10s  od) newMid 
      --                              , od30s   = updateODPoint (od30s  od) newMid 
      --                              , od1m    = updateODPoint (od1m   od) newMid 
      --                              , od2p5m  = updateODPoint (od2p5m od) newMid 
      --                              , od5m    = updateODPoint (od5m   od) newMid 
      --                              , od10m   = updateODPoint (od10m  od) newMid 
      --                              , od15m   = updateODPoint (od15m  od) newMid 
      --                              , od30m   = updateODPoint (od30m  od) newMid 
      --                              , od1h    = updateODPoint (od1h   od) newMid 
      --                              , od2h    = updateODPoint (od2h   od) newMid 
      --                              , od4h    = updateODPoint (od4h   od) newMid 
      --                              } 
      --               TM.insertForce ohlcDataMap secID newOD

--  od1s
--     ,  od10s   
--     ,  od30s   
--     ,  od1m    
--     ,  od2p5m  
--     ,  od5m    
--     ,  od10m   
--     ,  od15m   
--     ,  od30m   
--     ,  od1h    
--     ,  od2h    
--     ,  od4h    

fmtOD :: TM.TMapMVar Text OHLCData -> Text -> STM Text
fmtOD !ohlcDataMap !secID = do
  od <- TM.lookup ohlcDataMap secID  --returns STM
  TM.insertForce ohlcDataMap secID od   --need to reinsert it unchanged, as TM.lookup deletes the key-value pair
  pure $ fmt "" +|secID|+ " 1s: " +|od1s od|+ 
                          " 10s: "+|od10s od|+ 
                          -- " 30s: "+|od30s od|+ 
                          " 1m: "+|od1m od|+ 
                          ""
  -- pure "########"

  -- odM <- TM.tryLookup ohlcDataMap secID  --returns STM
  -- case odM of  --note, tryLookup deletes the key-value if successful
  --   Nothing -> pure $ "Unexpected ohlcDataMap show lookup for "<> secID  --as the map has been initialised, this should never occur
  --   Just od -> do TM.insertForce ohlcDataMap secID od   --trying inserting it back to remove an STM blockage somewhere
  --                 pure $ fmt "" +|secID|+
  --                         " 1s: " +|od1s od|+ 
  --                         " 10s: "+|od10s od|+ 
  --                         " 30s: "+|od30s od|+ 
  --                         ""
  --       --  " 1s:"+|roundToNDecPlaces 1 (od1s od)|+

-- listOD :: OHLCData -> [String]  --reading for display by brick, which expects String
-- listOD od = reverse . fst <$> (sortOn snd $ dataList) where   --need to reverse the list so the biggest number is at the top of the screen
listOD :: OHLCData -> [(String,Double)]  --reading for display by brick, which expects String
listOD od = reverse $ (sortOn snd $ dataList) where   --need to reverse the list so the biggest number is at the top of the screen
    dataList = [ pairUp lastP "price"  --dataList is a list of ("desc",val), so that it can be sorted on the value, but display the 
               , pairUp x1sMA "1s ma"
               , pairUp x2sMA "2s ma"
               , pairUp x3sMA "3s ma"
               , pairUp x10sMA "10s ma"
               , pairUp x30sMA "30s ma"
               , pairUp x1mMA "1m ma"
               , pairUp x2p5mMA "2.5m ma"
               , pairUp x5mMA "5m ma"
               , pairUp x10mMA "10m ma"
               , pairUp x15mMA "15m ma"
               , pairUp x30mMA "30m ma"
               , pairUp x1hMA "1h ma"
               , pairUp x2hMA "2h ma"
               , pairUp x4hMA "4h ma"

               , pairUp x1sBL "1s bl"
               , pairUp x2sBL "2s bl"
               , pairUp x3sBL "3s bl"
               , pairUp x10sBL "10s bl"
               , pairUp x30sBL "30s bl"
               , pairUp x1mBL "1m bl"
               , pairUp x2p5mBL "2.5m bl"
               , pairUp x5mBL "5m bl"
               , pairUp x10mBL "10m bl"
               , pairUp x15mBL "15m bl"
               , pairUp x30mBL "30m bl"
               , pairUp x1hBL "1h bl"
               , pairUp x2hBL "2h bl"
               , pairUp x4hBL "4h bl"

               , pairUp x1sBU "1s bu"
               , pairUp x2sBU "2s bu"
               , pairUp x3sBU "3s bu"
               , pairUp x10sBU "10s bu"
               , pairUp x30sBU "30s bu"
               , pairUp x1mBU "1m bu"
               , pairUp x2p5mBU "2.5m bu"
               , pairUp x5mBU "5m bu"
               , pairUp x10mBU "10m bu"
               , pairUp x15mBU "15m bu"
               , pairUp x30mBU "30m bu"
               , pairUp x1hBU "1h bu"
               , pairUp x2hBU "2h bu"
               , pairUp x4hBU "4h bu"

               ]
    pairUp :: Double -> Text -> (String,Double)
    pairUp x desc = ("" +|fixedF 1 x|+" "+|desc|+""  :: String , x)
    lastP   =  (odpLastP $ od1s od)    --this provides the type to all the other rows, removing ambiguity
    x1sMA   =  (odpMA $ od1s od   )
    x2sMA   =  (odpMA $ od2s od   )
    x3sMA   =  (odpMA $ od3s od   )
    x10sMA  =  (odpMA $ od10s od  )
    x30sMA  =  (odpMA $ od30s od  )
    x1mMA   =  (odpMA $ od1m od   )
    x2p5mMA =  (odpMA $ od2p5m od )
    x5mMA   =  (odpMA $ od5m od   )
    x10mMA  =  (odpMA $ od10m od  )
    x15mMA  =  (odpMA $ od15m od  )
    x30mMA  =  (odpMA $ od30m od  )
    x1hMA   =  (odpMA $ od1h od   )
    x2hMA   =  (odpMA $ od2h od   )
    x4hMA   =  (odpMA $ od4h od   )
    x1sBL   =  (odpBollLo $ od1s od   )
    x2sBL   =  (odpBollLo $ od2s od   )
    x3sBL   =  (odpBollLo $ od3s od   )
    x10sBL  =  (odpBollLo $ od10s od  )
    x30sBL  =  (odpBollLo $ od30s od  )
    x1mBL   =  (odpBollLo $ od1m od   )
    x2p5mBL =  (odpBollLo $ od2p5m od )
    x5mBL   =  (odpBollLo $ od5m od   )
    x10mBL  =  (odpBollLo $ od10m od  )
    x15mBL  =  (odpBollLo $ od15m od  )
    x30mBL  =  (odpBollLo $ od30m od  )
    x1hBL   =  (odpBollLo $ od1h od   )
    x2hBL   =  (odpBollLo $ od2h od   )
    x4hBL   =  (odpBollLo $ od4h od   ) 
    x1sBU   =  (odpBollUp $ od1s od   )
    x2sBU   =  (odpBollUp $ od2s od   )
    x3sBU   =  (odpBollUp $ od3s od   )
    x10sBU  =  (odpBollUp $ od10s od  )
    x30sBU  =  (odpBollUp $ od30s od  )
    x1mBU   =  (odpBollUp $ od1m od   )
    x2p5mBU =  (odpBollUp $ od2p5m od )
    x5mBU   =  (odpBollUp $ od5m od   )
    x10mBU  =  (odpBollUp $ od10m od  )
    x15mBU  =  (odpBollUp $ od15m od  )
    x30mBU  =  (odpBollUp $ od30m od  )
    x1hBU   =  (odpBollUp $ od1h od   )
    x2hBU   =  (odpBollUp $ od2h od   )
    x4hBU   =  (odpBollUp $ od4h od   )


getOD :: TM.TMapMVar Text OHLCData -> Text -> STM OHLCData
getOD !ohlcDataMap !secID = do
  od <- TM.lookup ohlcDataMap secID  --returns STM
  TM.insertForce ohlcDataMap secID od   --need to reinsert it unchanged, as TM.lookup deletes the key-value pair
  pure od  

updateODPoint :: OHLCDataPoint -> MidPriceField -> OHLCDataPoint
updateODPoint ipODP newMid = odp where 
  odp    = OHLCDataPoint k os_trimmed lastPrice bollLo ma bollHi maGrad ts 
  os     = insertMids k prevOS newMid 
  prevOS = (odpSeq ipODP)
  os_trimmed = S.drop (S.length os - 21) os  --keep last 21 elements, since the latest is still forming
  k      = odpSecs ipODP
  lastPrice = mid newMid
  maGrad = ma - ma2
  ztv = TimeVal 0 0 
  ts        = timestamp2 newMid
  ( (_, TimeVal _ bollLo, _) , (TimeVal _ ma2 , TimeVal _ ma, _) , (_, TimeVal _ bollHi, _) ) = 
    if S.length os < 21 
      then ((ztv,ztv,ztv),(ztv,ztv,ztv),(ztv,ztv,ztv))
      else getLast3Triple $ bollingerBands 20 2 $ closingPrices os   --getLast3 is more triple; it returns the most recent two readings, discard the most recent which is still forming
  -- ( (TimeVal _ bollLo, _) , (TimeVal _ ma, _) , (TimeVal _ bollHi, _) ) = 
  --   if S.length os < 21 
  --     then ((TimeVal 0 0,TimeVal 0 0),(TimeVal 0 0,TimeVal 0 0),(TimeVal 0 0,TimeVal 0 0))
  --     else getLast3 $ bollingerBands 20 2 $ closingPrices os   --getLast3 is more triple; it returns the most recent two readings, discard the most recent which is still forming


--read the tick's received off the TBQueue and update the OHLC data 
receiveTickListIntoOHLC :: Text -> TBQ.TBQueue (Maybe TickField) -> TM.TMapMVar Text OHLCData -> STM ()
receiveTickListIntoOHLC secID tickQueue ohlcDataMap = do  --allow these variables to be lazy (no !bang symbol) --od is the starting point, eg all zeros for initial call
  newTickM <- TBQ.readTBQueue tickQueue  --read just one value
  case newTickM of 
    Just t  -> do pushMids secID ohlcDataMap $ trace (Prelude.show t) (convertSpreadToMid t)  --update the od variable and iterate again
                  receiveTickListIntoOHLC secID tickQueue ohlcDataMap  --recursive call, building the list of ticks until a Nothing is received
    -- Just t  -> do trace (Prelude.show t) $ receiveTickListIntoOHLC secID tickQueue ohlcDataMap  --recursive call, building the list of ticks until a Nothing is received
    -- Nothing -> pure () --we're done here
    Nothing -> pure $ trace ("Received Nothing from tickQueue in the receive function") () --we're done here
