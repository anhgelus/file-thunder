{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Main (main) where

import Lib

import Control.Exception (throw)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import qualified Toml
import Toml.Schema (
    FromValue,
    GenericTomlTable (..),
    Result (Failure, Success),
    ToTable,
    ToValue,
 )

data Config = Config {root :: Text, storages :: Maybe [Storage]}
    deriving (Eq, Show, Generic)
    deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

data Storage = Storage {uri :: Text, path :: Text}
    deriving (Eq, Show, Generic)
    deriving (ToTable, ToValue, FromValue) via GenericTomlTable Storage

main :: IO ()
main = do
    raw <- readFile "config.toml"
    let res = loadConfig $ pack raw
    case warn res of
        Just w -> putStrLn $ intercalate "\n" w
        Nothing -> putStrLn "Config loaded"
    let cfg = config res
    run 8000 $ storageMiddleware (root cfg) (loadStorages $ storages cfg) app

data ConfigRes = ConfigRes {config :: Config, warn :: Maybe [String]}

loadConfig :: Text -> ConfigRes
loadConfig p = case Toml.decode p of
    Success [] v -> ConfigRes{config = v, warn = Nothing}
    Success w v -> ConfigRes{config = v, warn = Just w}
    Failure err -> throw $ userError $ intercalate "\n" err

loadStorages :: Maybe [Storage] -> (Map.Map Text Text)
loadStorages stors = case stors of
    Just storage -> Map.fromList $ map (\s -> ((uri s), (path s))) storage
    Nothing -> Map.empty
