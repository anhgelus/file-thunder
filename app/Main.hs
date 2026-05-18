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

data Config = Config
    { root :: T.Text
    , jwtSecretKey :: T.Text
    , defaultPermissions :: CfgPermission
    , storages :: Maybe [CfgStorage]
    }
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
    let rawDef = defaultPermissions cfg
    let conv = \v -> case v of
            Just g -> g
            Nothing -> False
    let def = Permission{canGet = conv $ get rawDef, canRead = conv $ read rawDef, canWrite = conv $ write rawDef}
    realPathKey <- V.newKey
    accountKey <- V.newKey
    permKey <- V.newKey
    run 8000
        $ middlewareAuth accountKey (WithJwt $ jwtSecretKey cfg)
        $ middlewareStorage
            accountKey
            realPathKey
            permKey
            (root cfg)
            (loadStorages def (storages cfg))
            def
        $ app realPathKey permKey

data ConfigRes = ConfigRes {config :: Config, warn :: Maybe [String]}

loadConfig :: FilePath -> ConfigRes
loadConfig file = case Toml.decode $ T.pack file of
    Schema.Success [] v -> ConfigRes{config = v, warn = Nothing}
    Schema.Success w v -> ConfigRes{config = v, warn = Just w}
    Schema.Failure err -> throw $ userError $ intercalate "\n" err

cfgPermToAuth :: Permission -> CfgPermission -> Permission
cfgPermToAuth def perm =
    Permission
        { canGet = maybeToDef get canGet def perm
        , canRead = maybeToDef read canRead def perm
        , canWrite = maybeToDef write canWrite def perm
        }

maybeToDef :: (CfgPermission -> Maybe Bool) -> (Permission -> Bool) -> Permission -> CfgPermission -> Bool
maybeToDef cv cvd def perm = case cv perm of
    Just b -> b
    Nothing -> cvd def

loadCfgPerm :: Permission -> CfgStorage -> Permissions
loadCfgPerm def stor =
    let defaultName = name notConnected
     in Map.fromList $
            (notConnected, def)
                : ( map
                        ( \(k, v) ->
                            (Account{name = if k /= "default" then k else defaultName}, cfgPermToAuth def v)
                        )
                        $ Map.toList
                        $ case permissions stor of
                            Nothing -> Map.empty
                            Just m -> m
                  )

loadStorages :: Permission -> Maybe [CfgStorage] -> S.Storage
loadStorages cfg stors =
    case stors of
        Just storage -> Map.fromList $ map (\s -> (uri s, S.createPlace (uri s) (path s) (loadCfgPerm cfg s))) storage
        Nothing -> Map.empty
