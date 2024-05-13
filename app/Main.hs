module Main where

import Control.Monad
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Happstack.Server (Response, ServerPart, badRequest, dir, look, notFound, nullConf, ok, simpleHTTP, toResponse)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H

type TextRecord = Vector Text

-- Reducing function for consistentLength
widthReducer :: (Bool, Int) -> TextRecord -> (Bool, Int)
widthReducer (consistent, prevLength) record = (consistent && len == prevLength, len)
  where
    len = V.length record

-- Ensures all CSV records in the given record are of the same length
consistentLength :: Vector TextRecord -> Bool
consistentLength csvRecords = allSameLength
  where
    firstRec = (V.!?) csvRecords 0
    initialAcc = case firstRec of
      Just vec -> (True, V.length vec)
      -- if there aren't any records, treat that as a bad request
      Nothing -> (False, 0)
    (allSameLength, length) = V.foldl' widthReducer initialAcc csvRecords

-- Converts a record into a <tr>, where each field is wrapped with a <td> element
recordToRow :: TextRecord -> H.Html
recordToRow = H.tr . mapM_ (H.td . H.toHtml)

-- Renders CSV data into a table, ensuring it is nonempty & each row has the same length
csvToTable :: Vector TextRecord -> Either String H.Html
csvToTable records
  | consistentLength records = Right renderedTable
  | otherwise = Left "bad records"
  where
    -- TODO: header (index + V.slice?)
    renderedTable = H.table $ forM_ records recordToRow

-- Handler for /render route, which does the HTML rendering of CSV
csvPart :: ServerPart Response
csvPart = do
  csvStr <- look "csvdata"
  -- decode takes a ByteString so we have to do the conversion manually
  let csvBytes = toLazyByteString $ stringUtf8 csvStr
      -- TODO: header included? (for table)
      parsed = decode NoHeader csvBytes
  -- TODO: monadic way to do this?
  case parsed >>= csvToTable of
    Left _ -> mzero
    Right table -> ok $ toResponse table

main :: IO ()
main =
  simpleHTTP nullConf $
    msum
      [ dir "render" csvPart,
        -- if previous route failed, then something went wrong when rendering the CSV
        dir "render" $ badRequest $ toResponse "invalid CSV",
        -- default to not found for non-render routes
        notFound $ toResponse "not found"
      ]
