module Tui where
    --ie. TUI, text user interface
    --this proviced a fast view of price action with panels for
    -- Point and Figure charts
    -- MA summary view, showing current price vs. MA's and Bollinger levels, with lovely colours
    -- horiz log panel across the bottom showing messages like orders filled, perhaps errors, and alerts

import Prelude hiding (on)
import Lib
-- import PointNFigure
-- import Database  --my library
-- import Security  --my library
-- import Broker    --my library
-- import Settings  --my library
import OHLC      --my library
-- import Text.Pretty.Simple (pPrint)
-- -- import Control.Concurrent.STM  --looking for STM type
-- import qualified Data.ByteString.Char8 as CS
import qualified Control.Concurrent.STM.TMapMVar as TM
-- -- import Control.Concurrent  --has forkIO,threadDelay
import Control.Concurrent (threadDelay, forkIO)
-- import Control.Monad (when)
-- import Data.Maybe (fromJust)
-- import qualified Data.Sequence as S  --use sequence instead of list to speed up tick load
-- import Data.Sequence((<|), (|>), (><), Seq(..), ViewL(..), ViewR(..) )
-- import Database.SQLite3 as SQL3
-- import qualified Control.Concurrent.STM.TBQueue  as TBQ
-- import Time.System
-- import Data.Hourglass  --contains fromElapsedTime
-- import qualified Text.Show as TS
-- import Data.Text (drop,dropEnd,unpack)
-- import Control.Concurrent.STM.TMVar  --using TMVars as threadsafe and STM-happy mutable vars, apparantly superior to MVars (may block more easily), and IORefs (single-thread only)
-- import Fmt

import Brick
-- import Brick.Widgets.Center
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import Graphics.Vty (black , red , green , yellow , blue , magenta , cyan , white , brightBlack , brightRed , brightGreen , brightYellow , brightBlue , brightMagenta , brightCyan , brightWhite )
import Brick.BChan
import Lens.Micro
import Lens.Micro.TH
import Data.Text ( unpack )
import Data.List ( isSuffixOf , isInfixOf )

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  , Next
  , EventM
  , BrickEvent(..)
  )
import Brick.AttrMap
  ( attrMap
  , AttrMap
  )
import Brick.Util (on, fg) 
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , (<=>)
  , str
  )

data St =
    St { _secID :: Text  --this is the secID which should be displayed, ie. used in lookups against the map
    --    , _ohlcDataMap :: TM.TMapMVar Text OHLCData
       , _ohlcData :: OHLCData
       , _stLastBrickEvent :: Maybe (BrickEvent () DataUpdateEvent)
       }
-- makeLenses ''St 

-- data DataUpdateEvent = DataUpdate ( TM.TMapMVar Text OHLCData )
data DataUpdateEvent = DataUpdate ( OHLCData )

---------------------------------------
-- This is the main entry point      --
---------------------------------------

runTui :: Logger -> [Text] -> TM.TMapMVar Text OHLCData -> IO ()
runTui logger secIDs ohlcDataMap = do 
    logInfo logger $ "Started TUI" <> "...." 
 
    chan <- newBChan 10   --create the channel :: Brick.BChan (ie. the bounded channel that brick is expecting, of it's own type)
    let secID = "L1:IX.D.NASDAQ.IFS.IP"  --OF COURSE< you'll iterate over all the secIDs, updating each map in turn
    void $ forkIO $ forever $ do  --set up infinite loop reading latest OHLC datamapas from STM and piping into Brick through it's expected chan
      latestOHLCData <- atomically $ getOD ohlcDataMap secID 
      writeBChan chan $ DataUpdate latestOHLCData  --write the event to the channel, that brick will pass through it's event handler
      threadDelay 100000  --0.1s iteration

    latestOHLCData <- atomically $ getOD ohlcDataMap secID 
    let initialState = St "L1:IX.D.NASDAQ.IFS.IP" latestOHLCData Nothing
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ M.customMain initialVty buildVty (Just chan) app initialState

data Name = VP1
          | VP2
          | VP3
          deriving (Ord, Show, Eq)

drawUi :: St -> [Widget Name]
drawUi st = [ui]
    where
        -- ui = C.center $ B.border $ hLimit 150 $ vLimit 81 $
        ui = B.border $ -- hLimit 180 $ vLimit 81 $
              -- vBox [ pair, B.hBorder, singleton ]
              vBox [ pair ]
        pair = hBox [ viewport VP1 Vertical $
                        hLimit 150 $ vLimit 120 $
                        vBox $ str "Press up and down arrow keys" :
                              str "to scroll this viewport." :
                              (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                    , B.vBorder
                    -- , viewport VP2 Horizontal $
                      -- str "Press left and right arrow keys to scroll this viewport."
                    , viewport VP2 Vertical $
                      hLimit 30 $ vLimit 120 $
                      vBox $ str "Press up and down arrow keys to scroll this viewport."
                           : (attrForMAs <$> listOD (_ohlcData st))
                    ]
        singleton = viewport VP3 Both $
                      hLimit 180 $ vLimit 30 $
                    vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                          : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])

attrForMAs :: (String , Double) -> Widget Name
attrForMAs ip = colourCode ip where
  colourCode :: (String , Double) -> Widget Name
  -- colourCode (fullStr,desc,lvl) = if x == "price" then (withAttr "price_colour") $ str x
  --                                else (withAttr "ma_colour")    $ str x
  colourCode (fullStr,val) | "price" `isSuffixOf` fullStr = (withAttr "price_colour")     $ str fullStr 
                           | "ma" `isSuffixOf` fullStr    = (withAttr "ma_colour")        $ str fullStr
                           | "ma" `isSuffixOf` fullStr && "m" `isInfixOf` fullStr  
                                                          = (withAttr "ma_minute_colour") $ str fullStr
                           | "ma" `isSuffixOf` fullStr && "h" `isInfixOf` fullStr  
                                                          = (withAttr "ma_hour_colour")   $ str fullStr
                           | "bu" `isSuffixOf` fullStr    = (withAttr "bu_colour")        $ str fullStr
                           | "bl" `isSuffixOf` fullStr    = (withAttr "bl_colour")        $ str fullStr
                           | otherwise                    = (withAttr "plain")            $ str fullStr

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

vp3Scroll :: M.ViewportScroll Name
vp3Scroll = M.viewportScroll VP3

appEvent :: St -> T.BrickEvent Name DataUpdateEvent -> T.EventM Name (T.Next St)
appEvent st e =
    case e of
        T.VtyEvent (V.EvKey V.KDown  [V.MCtrl]) -> M.vScrollBy vp3Scroll 1 >> M.continue st
        T.VtyEvent (V.EvKey V.KUp    [V.MCtrl]) -> M.vScrollBy vp3Scroll (-1) >> M.continue st
        T.VtyEvent (V.EvKey V.KRight [V.MCtrl]) -> M.hScrollBy vp3Scroll 1 >> M.continue st
        T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl]) -> M.hScrollBy vp3Scroll (-1) >> M.continue st
        T.VtyEvent (V.EvKey V.KDown [])  -> M.vScrollBy vp1Scroll 1 >> M.continue st
        T.VtyEvent (V.EvKey V.KUp [])    -> M.vScrollBy vp1Scroll (-1) >> M.continue st
        T.VtyEvent (V.EvKey V.KRight []) -> M.hScrollBy vp2Scroll 1 >> M.continue st
        T.VtyEvent (V.EvKey V.KLeft [])  -> M.hScrollBy vp2Scroll (-1) >> M.continue st 
        T.VtyEvent (V.EvKey V.KEsc [])   -> M.halt st
        AppEvent   (DataUpdate (ohlcData) ) -> M.continue $ st {_ohlcData = ohlcData} 
        _ -> M.continue st

globalDefault :: V.Attr
globalDefault = V.white `on` black

theMap :: AttrMap
theMap = attrMap globalDefault
  [ ("plain"    , white   `on` black)
  , ("ma_colour", black   `on` white )
  , ("ma_minute_colour", brightBlack   `on` white )
  , ("ma_hour_colour", brightBlack   `on` brightWhite )
  , ("price_colour", white   `on` cyan  )  --price
  , ("bu_colour", yellow  `on` black )  --boll Upper
  , ("bl_colour", magenta `on` black )  --boll Lower
  ]

app :: M.App St DataUpdateEvent Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          -- , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appAttrMap = const $ theMap
          , M.appChooseCursor = M.neverShowCursor
          }
