module Lib (
    app,
    storageMiddleware,
) where

import Data.List (uncons, unsnoc)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status
import Network.Mime (MimeType, defaultMimeMap, defaultMimeType)
import Network.Wai

app :: Application
app req respond =
    let path = pathInfo req
     in respond $ responseFile status200 [(hContentType, getContentType path)] (T.unpack $ T.intercalate "/" path) Nothing

type Storage = Map.Map T.Text T.Text

storageMiddleware :: T.Text -> Storage -> Middleware
storageMiddleware def stor next req resp =
    -- remove empty parts
    let base = filter (\t -> (T.length t) > 0) $ T.splitOn "/" def
     in next
            req
                { pathInfo = case uncons (pathInfo req) of
                    Just (key, t) -> case Map.lookup key stor of
                        Just v -> v : t
                        Nothing -> base ++ key : t
                    Nothing -> base
                }
            resp

getContentType :: [T.Text] -> MimeType
getContentType path = case unsnoc path of
    Nothing -> defaultMimeType
    Just (_, f) -> case unsnoc $ T.splitOn "." f of
        Nothing -> defaultMimeType
        Just (_, ext) -> case Map.lookup ext defaultMimeMap of
            Nothing -> defaultMimeType
            Just t -> t
