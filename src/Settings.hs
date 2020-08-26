{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Prelude
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B  --what Aeson works with
import Data.Text as DT
import Data.Maybe (fromJust,isNothing)
import Control.Monad (when)
import Text.Pretty.Simple (pPrint)
import qualified Relude.Unsafe as Unsafe

import Lib
import Security  --my library
-- import PointNFigure
-- import Database  --my library


data SecuritySetting =
  SecuritySetting { ssID :: !Text
                  --need to put in start and end times, outside of which no new trades may be opened
                  , ssLongSize  :: Double
                  , ssShortSize :: Double
                  , ssLongOpenLevel  :: Double    --level beyond which permitted to open long trades
                  , ssShortOpenLevel :: Double    --level beyond which permitted to open short trades
                  , ssLongCloseLevel  :: Double    --level beyond which permitted to close long trades
                  , ssShortCloseLevel :: Double    --level beyond which permitted to close short trades
                  , ssMaxLevelLong  :: Int
                  , ssMaxLevelShort :: Int}
  deriving (Generic,Show)
instance FromJSON SecuritySetting
instance ToJSON SecuritySetting where 
  toEncoding = genericToEncoding defaultOptions

removeWhitespace :: B.ByteString -> B.ByteString
removeWhitespace tb = encodeUtf8 t3 where
  t3 = DT.replace " " "" t2
  t2 = DT.strip t1
  t1 = decodeUtf8 tb

loadSettings :: FilePath -> IO (Maybe [SecuritySetting])
loadSettings fname = do  -- "demo.settings.json"
  j <- B.readFile fname  -- Aeson's decode expects a bystestring 
  pure $ decode (removeWhitespace j)

selectSetting :: [SecuritySetting] -> Text -> Maybe SecuritySetting
selectSetting settingsList secID = case (Prelude.length res) of
    0 -> Nothing
    1 -> Just (Unsafe.head res)
    _ -> trace ("Duplicate settings for " ++ unpack secID)
               Nothing 
  where
  res = Prelude.filter (\x -> ssID x == secID) settingsList


--Sample json to decode    ####KEeping this for posterity, but I've changed the schema
-- { "ssID" : "L1:IX.D.NASDAQ.IFS.IP"
-- , "ssLongSize"      : "true"
-- , "ssShortSize"     : "true"
-- , "ssShortLevel" : 9580
-- , "ssLongLevel"  : 0
-- , "ssMaxLevelLong"  : 5
-- , "ssMaxLevelShort" : 3}
--

-- *Settings> ss = SecuritySetting {ssID = "nasdaq" , ssLongEnabled = True , ssShortEnabled = False , ssTradeSize = 1 , ssMaxLevel = 3}
-- SecuritySetting {ssID = "nasdaq", ssLongEnabled = True, ssShortEnabled = False, ssTradeSize = 1.0, ssMaxLevel = 3}
-- *Settings> encode ss
-- "{\"ssID\":\"nasdaq\",\"ssLongEnabled\":true,\"ssShortEnabled\":false,\"ssTradeSize\":1.0,\"ssMaxLevel\":3}"
-- *Settings> ssJSON = encode ss
-- *Settings> ssJSON
-- "{\"ssID\":\"nasdaq\",\"ssLongEnabled\":true,\"ssShortEnabled\":false,\"ssTradeSize\":1.0,\"ssMaxLevel\":3}"
-- *Settings> decode ssJSON :: Maybe SecuritySetting
-- Just (SecuritySetting {ssID = "nasdaq", ssLongEnabled = True, ssShortEnabled = False, ssTradeSize = 1.0, ssMaxLevel = 3})
