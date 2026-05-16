module FileThunder (
    app,
    middlewareStorage,
) where

import FileThunder.Content (ContentInfo (..), indexHtml)
import FileThunder.Storage (Storage, realPath)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import Lucid (renderBS)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status
import Network.Mime (MimeType, defaultMimeLookup, defaultMimeType)
import Network.Wai
import System.Directory (doesFileExist, doesPathExist, listDirectory)

app :: V.Key FilePath -> Application
app k req respond = do
    let p = V.lookup k (vault req)
    case p of
        Nothing -> respond $ responseLBS status404 [] "not found"
        Just full -> do
            exists <- doesFileExist full
            let handle = if exists then handleFile else handleDir
            handle req full >>= respond

type ReqHandler = Request -> FilePath -> IO Response

handleFile :: ReqHandler
handleFile _req p =
    pure $
        responseFile
            status200
            [(hContentType, getContentType p)]
            p
            Nothing

handleDir :: ReqHandler
handleDir req fullPath = do
    contents <- listDirectory fullPath >>= (generateContentInfo [])
    pure $ responseLBS status200 [] (renderBS $ indexHtml req contents)

generateContentInfo :: [ContentInfo] -> [FilePath] -> IO [ContentInfo]
generateContentInfo acc ps = case ps of
    p : t ->
        doesFileExist p >>= \b -> generateContentInfo (ContentInfo{path = p, directory = not b} : acc) t
    [] -> pure acc

getContentType :: String -> MimeType
getContentType p = case L.unsnoc $ T.splitOn "/" (T.pack p) of
    Nothing -> defaultMimeType
    Just (_, f) -> if T.any (\c -> c == '.') f then defaultMimeLookup f else "text/plain"

middlewareStorage :: V.Key FilePath -> T.Text -> Storage -> Middleware
middlewareStorage k root stor next req resp = do
    let v = (vault req)
    let rp = T.unpack $ T.intercalate "/" (realPath root stor req)
    putStrLn rp
    exists <- doesPathExist rp
    next req{vault = if exists then V.insert k rp v else v} resp
