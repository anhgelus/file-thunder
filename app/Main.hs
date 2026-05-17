{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Main (main) where

import FileThunder
import FileThunder.Auth (Account (..), Permission (..), Permissions, notConnected)
import FileThunder.Auth.Jwt (WithJwt (WithJwt))
import qualified FileThunder.Storage as S

import Control.Exception (throw)
import Data.List (intercalate, uncons)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)
import qualified Toml
import qualified Toml.Schema as Schema
import Prelude hiding (read)

data Config = Config {root :: T.Text, jwtSecretKey :: T.Text, defaultPermissions :: CfgPermission, storages :: Maybe [CfgStorage]}
    deriving (Eq, Show, Generic)
    deriving (Schema.FromValue) via Schema.GenericTomlTable Config

data CfgStorage = CfgStorage {uri :: T.Text, path :: T.Text, permissions :: Maybe (Map.Map T.Text CfgPermission)}
    deriving (Eq, Show, Generic)
    deriving (Schema.FromValue) via Schema.GenericTomlTable CfgStorage

data CfgPermission = CfgPermission {get :: Maybe Bool, read :: Maybe Bool, write :: Maybe Bool}
    deriving (Eq, Show, Generic)
    deriving (Schema.ToValue, Schema.ToTable, Schema.FromValue) via Schema.GenericTomlTable CfgPermission

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ case uncons args of
        Just (f, _) -> f
        Nothing -> "/etc/thunderd.toml"
    let res = loadConfig file
    case warn res of
        Just w -> putStrLn $ intercalate "\n" w
        Nothing -> putStrLn "Config loaded"
    let cfg = config res
    realPathKey <- V.newKey
    accountKey <- V.newKey
    run 8000 $
        middlewareStorage realPathKey (root cfg) (loadStorages (defaultPermissions cfg) (storages cfg)) $
            middlewareAuth accountKey (WithJwt $ jwtSecretKey cfg) $
                app realPathKey

data ConfigRes = ConfigRes {config :: Config, warn :: Maybe [String]}

loadConfig :: FilePath -> ConfigRes
loadConfig file = case Toml.decode $ T.pack file of
    Schema.Success [] v -> ConfigRes{config = v, warn = Nothing}
    Schema.Success w v -> ConfigRes{config = v, warn = Just w}
    Schema.Failure err -> throw $ userError $ intercalate "\n" err

cfgPermToAuth :: CfgPermission -> CfgPermission -> Permission
cfgPermToAuth def perm = Permission{canGet = maybeToDef get def perm, canRead = maybeToDef read def perm, canWrite = maybeToDef write def perm}

maybeToDef :: (CfgPermission -> Maybe Bool) -> CfgPermission -> CfgPermission -> Bool
maybeToDef cv def perm = case cv perm of
    Just b -> b
    Nothing -> case cv def of
        Just b -> b
        Nothing -> False

loadCfgPerm :: CfgPermission -> CfgStorage -> Permissions
loadCfgPerm def stor =
    let defaultName = name notConnected
     in Map.fromList $
            (notConnected, cfgPermToAuth def def)
                : ( map
                        ( \(k, v) ->
                            (Account{name = if k /= "default" then k else defaultName}, cfgPermToAuth def v)
                        )
                        $ Map.toList
                        $ case permissions stor of
                            Nothing -> Map.empty
                            Just m -> m
                  )

loadStorages :: CfgPermission -> Maybe [CfgStorage] -> S.Storage
loadStorages cfg stors =
    case stors of
        Just storage -> Map.fromList $ map (\s -> (uri s, S.createPlace (uri s) (path s) (loadCfgPerm cfg s))) storage
        Nothing -> Map.empty
