module FileThunder (
    app,
    realPath,
    Storage,
) where

import Data.List (uncons, unsnoc)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import FileThunder.Content (ContentInfo (..), indexHtml)
import Lucid (renderBS)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status
import Network.Mime (MimeType, defaultMimeLookup, defaultMimeType)
import Network.Wai
import System.Directory (doesFileExist, doesPathExist, listDirectory)

app :: (Request -> [T.Text]) -> Application
app stor req respond = do
    let p = stor req
    let fullPath = T.unpack $ T.intercalate "/" p
    valid <- doesPathExist fullPath
    if valid
        then do
            exists <- doesFileExist fullPath
            let handle = if exists then handleFile else handleDir
            handle req p fullPath >>= respond
        else
            respond $ responseLBS status404 [] "not found"

type ReqHandler = Request -> [T.Text] -> FilePath -> IO Response

handleFile :: ReqHandler
handleFile _req p joined =
    pure $
        responseFile
            status200
            [(hContentType, getContentType p)]
            joined
            Nothing

handleDir :: ReqHandler
handleDir req _p fullPath = do
    contents <- listDirectory fullPath >>= (generateContentInfo [])
    pure $ responseLBS status200 [] (renderBS $ indexHtml req contents)

generateContentInfo :: [ContentInfo] -> [FilePath] -> IO [ContentInfo]
generateContentInfo acc ps = case ps of
    p : t ->
        doesFileExist p >>= \b -> generateContentInfo (ContentInfo{path = p, directory = not b} : acc) t
    [] -> pure acc

type Uri = T.Text
type Path = T.Text

type Storage = Map.Map Uri Path

realPath :: T.Text -> Storage -> Request -> [T.Text]
realPath def stor req =
    -- remove empty parts
    let base = (filter (\t -> (T.length t) > 0) (T.splitOn "/" def))
     in case uncons (pathInfo req) of
            Just (key, t) -> case Map.lookup key stor of
                Just v -> v : t
                Nothing -> base ++ key : t
            Nothing -> base ++ [""] -- normalize with trailing slash

getContentType :: [T.Text] -> MimeType
getContentType p = case unsnoc p of
    Nothing -> defaultMimeType
    Just (_, f) -> if T.any (\c -> c == '.') f then defaultMimeLookup f else "text/plain"
