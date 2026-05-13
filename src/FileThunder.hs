module FileThunder (
    app,
    storageMiddleware,
) where

import Data.List (uncons, unsnoc)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status
import Network.Mime (MimeType, defaultMimeLookup, defaultMimeType)
import Network.Wai

app :: Application
app req respond = handle (pathInfo req) respond

handle :: [T.Text] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handle path respond
    | (T.length p) == 0 = respond $ responseLBS status404 [] "not found"
    | otherwise =
        respond $
            responseFile
                status200
                [(hContentType, getContentType path)]
                (T.unpack $ T.intercalate "/" path)
                Nothing
  where
    p = case unsnoc path of
        Nothing -> ""
        Just (_, v) -> v

type Storage = Map.Map T.Text T.Text

storageMiddleware :: T.Text -> Storage -> Middleware
storageMiddleware def stor next req resp =
    -- remove empty parts
    let base = (filter (\t -> (T.length t) > 0) $ T.splitOn "/" def)
     in next
            req
                { pathInfo = case uncons (pathInfo req) of
                    Just (key, t) -> case Map.lookup key stor of
                        Just v -> v : t
                        Nothing -> base ++ key : t
                    Nothing -> base ++ [""] -- normalize with trailing slash
                }
            resp

getContentType :: [T.Text] -> MimeType
getContentType path = case unsnoc path of
    Nothing -> defaultMimeType
    Just (_, f) -> if T.any (\c -> c == '.') f then defaultMimeLookup f else "text/plain"
