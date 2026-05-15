module FileThunder.Auth.Password (WithPassword (..)) where

import FileThunder.Auth

import Crypto.KDF.BCrypt (validatePassword)
import Data.ByteString (ByteString)
import Data.List (uncons)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Query, hContentType, parseQuery)
import Network.Wai (Request (requestHeaders), getRequestBodyChunk)

data WithPassword = WithPassword {user :: Text, pass :: ByteString}

instance Auth WithPassword where
    login p req = case uncons $ filter (\(h, _) -> h == hContentType) $ requestHeaders req of
        Just ((_, "application/x-www-form-urlencoded"), []) -> processForm p req
        _ -> pure Nothing

processForm :: WithPassword -> Request -> IO (Maybe Account)
processForm p req =
    (\q -> if validateForm (False, False) p q then Just Account{name = user p} else Nothing)
        <$> parseQuery
        <$> getRequestBodyChunk req

validateForm :: (Bool, Bool) -> WithPassword -> Query -> Bool
validateForm (u, pw) p q = case q of
    (k, Just v) : t -> case k of
        "user" -> if v == encodeUtf8 (user p) then validateForm (True, pw) p t else False
        "password" -> if validatePassword v (pass p) then validateForm (u, True) p t else False
        -- missing CSRF token
        _ -> False
    [] -> u == pw && u
    _ -> False
