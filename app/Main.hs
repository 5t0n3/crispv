{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Happstack.Server (Response, ServerPart, dir, look, nullConf, ok, simpleHTTP, toResponse)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H

type TextRecord = Vector Text

widthReducer :: (Bool, Int) -> TextRecord -> (Bool, Int)
widthReducer (allSame, prevLength) record = (allSame && consistent, len)
  where
    len = V.length record
    consistent = len == prevLength

getTableWidth :: Vector TextRecord -> Either String Int
getTableWidth csvRecords = if allSameLength then Right length else Left "not all same length"
  where
    firstRec = (V.!?) csvRecords 0
    initialAcc = case firstRec of
      Just vec -> (True, V.length vec)
      Nothing -> (False, 0)
    (allSameLength, length) = V.foldl' widthReducer initialAcc csvRecords

-- Converts a record into a row (<tr>), where each field is placed in a <td> element
recordToRow :: TextRecord -> H.Html
recordToRow = H.tr . mapM_ (H.td . H.toHtml)

csvToTable :: Vector TextRecord -> Either String H.Html
csvToTable records = do
  width <- getTableWidth records
  return $ H.table $ forM_ records recordToRow

csvPart :: ServerPart Response
csvPart = do
  csvStr <- look "csvdata"
  let csvBytes = toLazyByteString $ stringUtf8 csvStr
      -- TODO: header included? (for table)
      parsed = decode NoHeader csvBytes
  -- TODO: monadic way to do this?
  case parsed >>= csvToTable of
    Left _ -> mzero
    Right table -> ok $ toResponse table

main :: IO ()
main =
  simpleHTTP nullConf $ dir "render" csvPart
