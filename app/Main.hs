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

-- Ensures all CSV records in the given record are of the same length
-- There must also be at least one record
consistentLength :: Vector TextRecord -> Bool
consistentLength csvRecords = not (V.null csvRecords) && all ((== firstLen) . V.length) csvRecords
  where
    -- This is only evaluated if V.null returns false, since (&&) is lazy :)
    firstLen = V.length $ (V.!) csvRecords 0

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
