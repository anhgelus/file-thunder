module FileThunder.Storage (
    Storage,
    realPath,
    Place (uri, path, permissions),
    accountCanGet,
    accountCanRead,
    accountCanWrite,
    createPlace,
    createPrivatePlace,
    placeFromReq,
    permissionsInSpace,
    fileKey,
) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as B
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Text as T
import FileThunder.Auth
import Network.Wai (Request (pathInfo))

type Uri = T.Text

type Storage = M.Map Uri Place

data Place = Place {uri :: T.Text, path :: T.Text, permissions :: Permissions}

createPlace :: Uri -> T.Text -> Permissions -> Place
createPlace u p perms = Place{uri = u, path = p, permissions = perms}

createPrivatePlace :: Uri -> T.Text -> Account -> Place
createPrivatePlace u p acc = Place{uri = u, path = p, permissions = privatePermissions acc}

realPath :: T.Text -> Storage -> Request -> [T.Text]
realPath def stor req =
    -- remove empty parts
    let base = (filter (\t -> (T.length t) > 0) (T.splitOn "/" def))
     in case uncons (pathInfo req) of
            Just (key, t) -> case M.lookup key stor of
                Just v -> (path v) : t
                Nothing -> base ++ key : t
            Nothing -> base ++ [""] -- normalize with trailing slash

placeFromReq :: Storage -> Request -> Maybe Place
placeFromReq stor req = case uncons (pathInfo req) of
    Just (key, _) -> M.lookup key stor
    Nothing -> Nothing

permissionsInSpace :: Place -> Account -> Permission
permissionsInSpace place acc = permissionsOf acc (permissions place)

accountCanGet :: Place -> Account -> Bool
accountCanGet = accountCan canGet

accountCanRead :: Place -> Account -> Bool
accountCanRead = accountCan canRead

accountCanWrite :: Place -> Account -> Bool
accountCanWrite = accountCan canWrite

accountCan :: (Permission -> Bool) -> Place -> Account -> Bool
accountCan get place acc = get $ permissionsInSpace place acc

fileKey :: FilePath -> IO String
fileKey p = do
    content <- B.readFile p
    let (c, _) = splitAt 10 $ show (hash $ content :: Digest SHA256)
    pure c
