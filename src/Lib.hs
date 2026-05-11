{-# LANGUAGE OverloadedStrings #-}

module Lib (
    someFunc,
    app,
) where

import Network.HTTP.Types.Status
import Network.Wai

someFunc :: IO ()
someFunc = putStrLn "someFunc"

app :: Application
app req respond = do
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS status200 [] "Hello World")
