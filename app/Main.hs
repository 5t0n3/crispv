{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, mapM_, msum, mzero)
import Data.Csv (HasHeader (NoHeader), decode)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Happstack.Server (Method (POST), Response, ServerPart, askRq, badRequest, dir, look, method, notFound, nullConf, ok, simpleHTTP, takeRequestBody, toResponse, unBody)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_)

type TextRecord = Vector Text

-- Convenience function for converting a Maybe to an Either String
maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just val) = Right val
maybeToEither msg Nothing = Left msg

-- Ensures all CSV records in the given record are of the same length
consistentLength :: Vector TextRecord -> Bool
consistentLength = all ((== firstLen) . V.length)
  where
    -- This is only evaluated if V.null returns false, since (&&) is lazy :)
    firstLen = V.length $ V.head csvRecords

renderTable :: (TextRecord, Vector TextRecord) -> H.Html
renderTable (header, tbody) = H.table ! class_ "crispv-table" $ toRow H.th header >> forM_ tbody (toRow H.td)
  where
    toRow :: (H.Html -> H.Html) -> TextRecord -> H.Html
    toRow innerEl = H.tr . mapM_ (innerEl . H.toHtml)

-- Renders CSV data into a table, ensuring it is nonempty & each row has the same length
csvToTable :: Vector TextRecord -> Either String H.Html
csvToTable records
  | enoughRecords && consistentLength records = renderTable <$> splitRecords
  | otherwise = Left "bad records"
  where
    enoughRecords = V.length records >= 2
    splitRecords = maybeToEither "not enough records" $ V.uncons records

-- Handler for /render route, which does the HTML rendering of CSV
renderPart :: ServerPart Response
renderPart = do
  req <- askRq
  rawCsv <- takeRequestBody req
  -- yay monad conversions :P
  let csvEither = unBody <$> maybeToEither "body not present" rawCsv
  case csvEither >>= decode NoHeader >>= csvToTable of
    Left _ -> mzero
    Right table -> ok $ toResponse table

main :: IO ()
main =
  simpleHTTP nullConf $
    msum
      [ method POST >> dir "render" renderPart,
        -- if previous route failed, then something went wrong when rendering the CSV (or wrong method/etc.)
        dir "render" $ badRequest $ toResponse ("bad request" :: Text),
        -- default to not found for non-render routes
        notFound $ toResponse ("not found" :: Text)
      ]
