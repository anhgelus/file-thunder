module FileThunder.Auth (
    Permissions,
    Account (..),
    Permission (..),
    Auth (login),
    connect,
    notConnected,
    defaultPermissions,
    privatePermissions,
    permissionsOf,
) where

import qualified Data.Map as M
import Data.Text (Text)
import Network.Wai (Request)

class Auth kind where
    login :: kind -> Request -> IO (Maybe Account)

data Account = Account {name :: Text} deriving (Show, Eq, Ord)

type Permissions = M.Map Account Permission

data Permission = Permission {canGet :: Bool, canRead :: Bool, canWrite :: Bool} deriving (Show, Eq)

connect :: (Auth a) => a -> Request -> IO (Maybe Account)
connect auth = login auth

notConnected :: Account
notConnected = Account{name = ""}

defaultPermissions :: Permissions
defaultPermissions = M.fromList [(notConnected, Permission{canGet = True, canRead = True, canWrite = False})]

privatePermissions :: Account -> Permissions
privatePermissions acc =
    M.fromList
        [ (acc, Permission{canGet = True, canRead = True, canWrite = True})
        , (notConnected, Permission{canGet = True, canRead = False, canWrite = False})
        ]

permissionsOf :: Account -> Permissions -> Permission
permissionsOf acc perms = case M.lookup acc perms of
    Just p -> p
    Nothing -> case M.lookup notConnected perms of
        Just p -> p
        Nothing -> Permission{canGet = False, canRead = False, canWrite = False}
