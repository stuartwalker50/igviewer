{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}  --this nearly halves the execution time

module Database where

import Prelude

import Database.SQLite3 as SQL3
import Lib
import OHLC (OHLCData,pushMids)
import qualified Relude.Unsafe as Unsafe
import qualified Control.Concurrent.STM.TMapMVar as TM
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Data.Sequence as S  --use sequence instead of list to speed up tick load
import Data.Sequence((<|), (|>), (><), Seq(..), ViewL(..), ViewR(..) )
import qualified Text.Read as TR  --I only need the Text read from this, which is totally unsafe
-- import Data.Text
import Data.Text.Read as TR

--Callback used by readDBsecID below
-- {-# INLINE quickTick #-}
quickTick :: TBQ.TBQueue (Maybe TickField) -> SQL3.ColumnCount -> [Text] -> [Maybe Text] -> IO ()
quickTick tickQueue colCount colNames colValues = do
  let [ts,bid,offer,mktState] = colValues
  case TR.decimal (Unsafe.fromJust ts) of
    Left errMsg -> putStrLn $ "Couldn't convert tick from db - timestamp - " ++ errMsg
    Right (ts',_) ->
      case double (Unsafe.fromJust bid) of
                  Left errMsg -> putStrLn $ "Couldn't convert tick from db - bid - " ++ errMsg
                  Right (bid',_) -> case double (Unsafe.fromJust offer) of
                                  Left errMsg -> putStrLn $ "Couldn't convert tick from db - offer - " ++ errMsg
                                  Right (offer',_) -> case mktState of
                                    Nothing -> putStrLn $ "Couldn't read market state for timestamp" ++ show ts
                                    Just mktState' -> when (mktState' == "TRADEABLE") $  --only add tradeable ticks to the OHLC mix
                                                        -- do putStrLn $ show $ TickField ts' bid' offer' mktState'
                                                        --push tick on the queue --convert STM to IO
                                                        atomically $ TBQ.writeTBQueue tickQueue (Just $ TickField ts' bid' offer' mktState') --push tick on the queue --convert STM to IO
                                                        -- atomically $ trace ("Read " ++ Prelude.show ts' ++ " " ++ Prelude.show bid' ++ " " ++ Prelude.show offer')
                                                        --              TBQ.writeTBQueue tickQueue (Just $ TickField ts' bid' offer' mktState') --push tick on the queue --convert STM to IO

--read the tick's received off the TBQueue
receiveTickList :: Text -> TBQ.TBQueue (Maybe TickField) -> TM.TMapMVar Text (S.Seq TickField) -> S.Seq TickField -> STM ()
-- receiveTickList !secID !tickQueue !tickMap !tickSeq = do  --allow these variables to be lazy (no !bang symbol)
receiveTickList secID tickQueue tickMap tickSeq = do  --allow these variables to be lazy (no !bang symbol)
  !newTickM <- TBQ.readTBQueue tickQueue  --read just one value
  case newTickM of
    Just t  -> receiveTickList secID tickQueue tickMap (tickSeq |> t)  --recursive call, building the list of ticks until a Nothing is received
    Nothing -> TM.insertForce tickMap secID tickSeq  --write the list to the tickMap, we're done here.  The queue should be empty, having read off all values
