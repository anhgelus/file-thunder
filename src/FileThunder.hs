module FileThunder (
    app,
    storageMiddleware,
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

app :: Application
app req respond = do
    let p = pathInfo req
    let fullPath = T.unpack $ T.intercalate "/" p
    exists <- doesFileExist fullPath
    let handle = if exists then handleFile else handleDir
    handle p fullPath >>= respond

handleFile :: [T.Text] -> FilePath -> IO Response
handleFile p joined =
    pure $
        responseFile
            status200
            [(hContentType, getContentType p)]
            joined
            Nothing

handleDir :: [T.Text] -> FilePath -> IO Response
handleDir p fullPath = do
    contents <- listDirectory fullPath >>= (generateContentInfo [])
    let name = case unsnoc p of
            Just (_, v) -> if (T.length v) == 0 then "Home" else v
            Nothing -> "Home"
    pure $ responseLBS status200 [] (renderBS $ indexHtml name contents)

generateContentInfo :: [ContentInfo] -> [FilePath] -> IO [ContentInfo]
generateContentInfo acc ps = case ps of
    p : t ->
        doesFileExist p >>= \a ->
            doesPathExist p >>= \b ->
                if b
                    then
                        generateContentInfo (ContentInfo{path = p, directory = not a} : acc) t
                    else error $ p <> " is not a directory"
    [] -> pure acc

type Uri = T.Text
type Path = T.Text

type Storage = Map.Map Uri Path

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
getContentType p = case unsnoc p of
    Nothing -> defaultMimeType
    Just (_, f) -> if T.any (\c -> c == '.') f then defaultMimeLookup f else "text/plain"
