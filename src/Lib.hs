module Lib (
    app,
    storageMiddleware,
) where

import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai

app :: Application
app req respond = do
    print $ pathInfo req
    respond $ responseLBS status200 [] "uwu"

storageMiddleware :: Text -> Map.Map Text Text -> Middleware
storageMiddleware def stor next req resp =
    next
        req
            { pathInfo = case uncons (pathInfo req) of
                Just (key, t) -> case Map.lookup key stor of
                    Just v -> v : t
                    Nothing -> def : key : t
                Nothing -> [def]
            }
        resp
