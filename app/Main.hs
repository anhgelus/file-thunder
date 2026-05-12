{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Main (main) where

import Lib

import Control.Exception (throw)
import Data.List (intercalate, uncons)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)
import qualified Toml
import Toml.Schema (
    FromValue,
    GenericTomlTable (..),
    Result (Failure, Success),
    ToTable,
    ToValue,
 )

data Config = Config {root :: T.Text, storages :: Maybe [Storage]}
    deriving (Eq, Show, Generic)
    deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

data Storage = Storage {uri :: T.Text, path :: T.Text}
    deriving (Eq, Show, Generic)
    deriving (ToTable, ToValue, FromValue) via GenericTomlTable Storage

main :: IO ()
main = do
    args <- getArgs
    raw <- readFile $ case uncons args of
        Just (f, _) -> f
        Nothing -> "/etc/thunderd.toml"
    let res = loadConfig $ T.pack raw
    case warn res of
        Just w -> putStrLn $ intercalate "\n" w
        Nothing -> putStrLn "Config loaded"
    let cfg = config res
    run 8000 $ storageMiddleware (root cfg) (loadStorages $ storages cfg) app

data ConfigRes = ConfigRes {config :: Config, warn :: Maybe [String]}

loadConfig :: T.Text -> ConfigRes
loadConfig p = case Toml.decode p of
    Success [] v -> ConfigRes{config = v, warn = Nothing}
    Success w v -> ConfigRes{config = v, warn = Just w}
    Failure err -> throw $ userError $ intercalate "\n" err

loadStorages :: Maybe [Storage] -> (Map.Map T.Text T.Text)
loadStorages stors = case stors of
    Just storage -> Map.fromList $ map (\s -> ((uri s), (path s))) storage
    Nothing -> Map.empty
