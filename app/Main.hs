module Main (main) where

import Lib

import Network.Wai.Handler.Warp

main :: IO ()
main = do
    run 8000 app
