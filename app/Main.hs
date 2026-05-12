module Main (main) where

import Lib

import qualified Data.Map.Strict as Map
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    run 8000 $ storageMiddleware "/var/www/public" (Map.fromList [("private", "/var/www/private")]) app
