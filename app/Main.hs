module Main where

import Control.Monad
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Csv
import Data.Text (Text)
import Data.Vector as V
import Happstack.Server (ServerPart, dir, look, nullConf, ok, simpleHTTP, toResponse)

csvPart :: ServerPart String
csvPart = do
  csvStr <- look "csvdata"
  let csvBytes = toLazyByteString $ stringUtf8 csvStr
      -- TODO: header included? (for table)
      parsed = decode NoHeader csvBytes :: Either String (V.Vector Record)
  case parsed of
    Left _ -> mzero
    Right stuff -> ok $ show stuff

main :: IO ()
main =
  simpleHTTP nullConf $ dir "render" csvPart
