module FileThunder (
    app,
    middlewareStorage,
    middlewareAuth,
) where

import FileThunder.Auth
import FileThunder.Content (ContentInfo (..), indexHtml)
import FileThunder.Storage (Storage, fileKey, permissionsInSpace, placeFromReq, realPath)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import Lucid (renderBS)
import Network.HTTP.Types (Query, hContentType)
import Network.HTTP.Types.Status
import Network.Mime (MimeType, defaultMimeLookup, defaultMimeType)
import Network.Wai
import System.Directory (doesFileExist, doesPathExist, listDirectory)

app :: V.Key FilePath -> V.Key Permission -> Application
app k kPerm req respond = do
    let v = vault req
    let p = V.lookup k v
    case p of
        Nothing -> respond notFound
        Just full ->
            let perm = case V.lookup kPerm v of
                    Just pe -> pe
                    Nothing -> error "Impossible state: cannot get permissions"
             in handleFound perm req full >>= respond

type ReqHandler = Request -> FilePath -> IO Response

handleFound :: Permission -> ReqHandler
handleFound perm req p = do
    exists <- doesFileExist p
    if canRead perm
        then (if exists then handleFile else handleDir) req p
        else
            if canGet perm && exists
                then handleGet (queryString req) handleFile req p
                else pure notFound

handleGet :: Query -> ReqHandler -> ReqHandler
handleGet q handler req p = do
    key <- T.pack <$> fileKey p
    case q of
        (k, Just v) : t ->
            if k == "k"
                then if key == T.decodeUtf8 v then handler req p else pure notFound
                else handleGet t handler req p
        _ : t -> handleGet t handler req p
        [] -> pure notFound

notFound :: Response
notFound = responseLBS status404 [] "not found"

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
    contents <- listDirectory fullPath >>= generateContentInfo []
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

middlewareStorage ::
    V.Key Account ->
    V.Key FilePath ->
    V.Key Permission ->
    T.Text ->
    Storage ->
    Permission ->
    Middleware
middlewareStorage kAcc k kPerm root stor def next req resp = do
    let v = (vault req)
    let rp = T.unpack $ T.intercalate "/" (realPath root stor req)
    exists <- doesPathExist rp
    if not exists
        then next req resp
        else
            let perm = case placeFromReq stor req of
                    Just p -> permissionsInSpace p $ case V.lookup kAcc v of
                        Nothing -> notConnected
                        Just a -> a
                    Nothing -> def
             in next req{vault = V.insert kPerm perm $ V.insert k rp v} resp

middlewareAuth :: (Auth a) => V.Key Account -> a -> Middleware
middlewareAuth k auth next req resp = do
    acc <- login auth req
    case acc of
        Nothing -> next req resp
        Just c -> next req{vault = V.insert k c (vault req)} resp
