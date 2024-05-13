module Main where

import Control.Monad (forM_, mapM_, msum, mzero)
import Data.Csv (HasHeader (NoHeader), decode)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Happstack.Server (Method (POST), Response, ServerPart, askRq, badRequest, dir, look, method, notFound, nullConf, ok, simpleHTTP, takeRequestBody, toResponse, unBody)
-- import Text.Blaze ((!))
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
renderPart :: ServerPart Response
renderPart = do
  req <- askRq
  rawCsv <- takeRequestBody req
  -- TODO: header included? (for table)
  -- yay monad conversions :P
  let csvEither = case rawCsv of
        Just body -> Right $ unBody body
        Nothing -> Left ""
  case csvEither >>= decode NoHeader >>= csvToTable of
    Left _ -> mzero
    Right table -> ok $ toResponse table

main :: IO ()
main =
  simpleHTTP nullConf $
    msum
      [ method POST >> dir "render" renderPart,
        -- if previous route failed, then something went wrong when rendering the CSV (or wrong method/etc.)
        dir "render" $ badRequest $ toResponse "bad request",
        -- default to not found for non-render routes
        notFound $ toResponse "not found"
      ]
