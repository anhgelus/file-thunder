module FileThunder.Cookie (parseCookies) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (hCookie)
import Network.Wai (Request (requestHeaders))

type Cookies = M.Map T.Text T.Text

parseCookies :: Request -> Cookies
parseCookies req = case parseCookieHeaders M.empty cookieH of
    Left _ -> M.empty
    Right v -> v
  where
    cookieH = map (\(_, v) -> decodeUtf8 v) $ filter (\(h, _) -> h == hCookie) $ requestHeaders req

data ParseCookieError = NotACookie T.Text deriving (Show)

parseCookieHeaders :: Cookies -> [T.Text] -> Either ParseCookieError Cookies
parseCookieHeaders acc hs = case hs of
    v : t -> case parseCookieHeader M.empty (map (\val -> T.strip val) $ T.splitOn ";" v) of
        Left err -> Left err
        Right val -> parseCookieHeaders (M.union acc val) t
    [] -> Right acc

parseCookieHeader :: Cookies -> [T.Text] -> Either ParseCookieError Cookies
parseCookieHeader acc cs = case cs of
    v : t -> case T.breakOn "=" v of
        (_, "") -> Left $ NotACookie v
        ("", _) -> Left $ NotACookie v
        (key, val) -> parseCookieHeader (M.insert key val acc) t
    [] -> Right acc
