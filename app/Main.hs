module Main where

import Prelude
import Lib       --my library
import Realtime  --my library
import Settings  --my library
import Options.Applicative
import Data.Semigroup ((<>))
-- import Data.Time
import Data.Fixed
import Data.Hourglass
import Data.Text (replace)
import qualified Control.Concurrent.STM.TBQueue  as TBQ
import Text.Pretty.Simple (pPrint)
import Data.Maybe (fromJust)

data CliOptions = CliOptions {
  realtimeFlag :: Bool
, liveAccountFlag :: Bool
, backtestFlag :: Bool
, serverFlag :: Bool
, preloadFlag :: Bool
, secID      :: Text
, inputFile  :: Maybe Text
, startDate  :: Maybe Text
, endDate    :: Maybe Text
, outputFile :: Maybe Text
} deriving (Eq, Show)

data Command = Backtest | Realtime deriving (Eq, Show)

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
   <$> switch (long "realtime" <> help "Set flag to trade realtime")
   <*> switch (long "live" <> help "Set flag to trade on live account (defaults to demo)")
   <*> switch (long "backtest" <> help "Set flag to backtest between start and end date")
   <*> switch (long "server" <> help "Test server alone")
   <*> switch (long "preLoadBackdata" <> help "Test server alone")
   <*> (strOption $ long "securityID" <> short 's' <> help "security to trade/backtest")
   <*> (optional $ strOption $ long "InputFile"
          <> short 'i'
          <> help "Input tick database file" )
   <*> (optional $ strOption $ long "BeginDate"
          <> short 'b'
          <> help "YYYY-MM-DD - starts from 00:00 on this date" )
   <*> (optional $ strOption $ long "EndDate"
          <> short 'e'
          <> help "YYYY-MM-DD - ends at 23:59 on this date" )
   <*> (optional $ strOption $ long "OutputFile"
          <> short 'o'
          <> help "Output CSV file" )

defineCliOptions :: ParserInfo CliOptions
defineCliOptions =
   info (cliOptionsParser <**> helper) $
   header "Backtest - generate and evaluate paper trades" <>
   progDesc "Read from tick database, apply rules to get paper trades, and summarise profitability characteristics"

main :: IO ()
main = do
  cliOptions <- execParser defineCliOptions
  putStrLn $ show $ cliOptions
  logger <- createLogger "./trader.log"
  logInfo logger "Logger created"
  logInfo logger "Ready to branch based on cli options"

  case (realtimeFlag cliOptions , backtestFlag cliOptions , serverFlag cliOptions , serverFlag cliOptions) of
    (False , False , False , False) -> putStrLn ("You must set at least one flag for backtest and/or realtime trading")
    -- (False , False , True , False ) -> runSpockServer 
    (False , False , _ , False) -> putStrLn ("You must set at least one flag for backtest and/or realtime trading")
    (True  , False , _ , False) -> do putStrLn ("Realtime flag set")
                                      callRealtime cliOptions logger
                                      -- runRealtime (liveAccountFlag cliOptions) (secID cliOptions) (inputFile cliOptions) 
    (False , True , _ , False) -> do putStrLn ("Backtest flag set")
                                     callBacktest cliOptions logger
    (True  , True , _ , False) -> do putStrLn ("Realtime & Backtest flag set - not implemented")
    (False , True , _ , False) -> do putStrLn ("Load Backdata flag set")
                                     callPreloadBackdata cliOptions logger
                          -- runRealtime (liveAccountFlag cliOptions) (inputFile cliOptions)

  logInfo logger "Execution complete"
  closeLog logger  --without this call, you run the risk of the main thread terminating, before the child thread is finished - it will be prematurely terminated.
  -- l <- atomically $ TBQ.lengthTBQueue logger
  -- pPrint l

  -- allOfThem <- atomically $ TBQ.flushTBQueue logger
  -- pPrint allOfThem



 -- Define calls to functionality --

callRealtime :: CliOptions -> Logger -> IO ()
callRealtime cliOptions logger = do
  if (any isNothing [ startDate cliOptions
                    , inputFile cliOptions
                    ])
     then error "You must specify start and end dates, and the input file to load backdata from."
     else do let sDateText  = fromJust $ startDate cliOptions
             let ipFilename = fromJust $ inputFile cliOptions
             -- let sd = timeParse ISO8601_Date (toString sDateText)
             pPrint  $ "Starting realtime with " <> ipFilename <> " " <> sDateText
             logInfo logger $ "Starting realtime with " <> ipFilename <> " " <> sDateText
             -- runMultiRealtime (liveAccountFlag cliOptions)
             runRealtime (liveAccountFlag cliOptions)
                         (ipFilename)
                         (sDateText)
                         logger

callBacktest :: CliOptions -> Logger -> IO ()
callBacktest cliOptions logger = do
  if (any isNothing [ startDate cliOptions
                    , endDate   cliOptions
                    , inputFile cliOptions
                    -- , secID     cliOptions
                    ])
     then error "You must specify start and end dates, and the input file to load backdata from."
     else do let sDateText  = fromJust $ startDate cliOptions
             let eDateText  = fromJust $ endDate   cliOptions
             let ipFilename = fromJust $ inputFile cliOptions
             let sd = timeParse ISO8601_Date (toString sDateText)
             let ed = timeParse ISO8601_Date (toString eDateText)
             let sid = secID cliOptions
             pPrint  $ "Starting backtest with " <> ipFilename <> " " <> sDateText <> " " <> eDateText
             logInfo logger $ "Starting backtest with " <> ipFilename <> " " <> sDateText <> " " <> eDateText
             when (any isNothing [sd , ed]) $ do
                pPrint  $ "Unrecognised date format, try YYYY-mm-dd"
                logInfo logger $ "Unrecognised date format, try YYYY-mm-dd"
             pPrint  $         "Backtest not available"
             logInfo logger  $ "Backtest not available"

callPreloadBackdata :: CliOptions -> Logger -> IO ()
callPreloadBackdata cliOptions logger = do
  if (any isNothing [ inputFile cliOptions
                    ])
     then error "You must specify one security, and the input file to load backdata from."
     else do let sid = secID cliOptions
             let ipFilename = fromJust $ inputFile cliOptions
             pPrint  $ "Starting load (and thin) of back data with " <> ipFilename <> " for " <> sid
             logInfo logger $ "Starting load (and thin) of back data with " <> ipFilename <> " for " <> sid 
             pPrint  $ "Preload now included automatically - please do not specify"