{-# LANGUAGE DeriveGeneric #-}

module FileThunder.Auth.Jwt (
    encodeJwt,
    Jwt (header, payload),
    JwtHeader (typ),
    JwtPayload (iat, exp, user),
    WithJwt (..),
    createJwt,
    decodeJwt,
) where

import FileThunder.Auth (Account (..), Auth (login))
import FileThunder.Cookie (parseCookies)

import Crypto.Hash (Digest, SHA3_512, digestFromByteString)
import Crypto.MAC.HMAC (HMAC (HMAC, hmacGetDigest), hmac)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Base64.Types (extractBase64)
import qualified Data.ByteString as BSL
import Data.ByteString.Base64.URL
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime)
import GHC.Generics (Generic)
import Prelude hiding (exp)

data JwtHeader = JwtHeader {typ :: String} deriving (Show, Generic)
instance ToJSON JwtHeader
instance FromJSON JwtHeader

data JwtPayload = JwtPayload {iat :: Int64, exp :: Int64, user :: T.Text} deriving (Show, Generic)
instance ToJSON JwtPayload
instance FromJSON JwtPayload

data Jwt = Jwt {header :: JwtHeader, payload :: JwtPayload} deriving (Show)

data WithJwt = WithJwt {secret :: T.Text}

instance Auth WithJwt where
    login w req = case M.lookup "auth" (parseCookies req) of
        Nothing -> pure Nothing
        Just raw ->
            (\j -> j >>= (\jwt -> Just Account{name = user $ payload jwt}))
                <$> decodeJwt (secret w) raw

createJwt :: T.Text -> Int64 -> IO Jwt
createJwt u days = do
    let h = JwtHeader{typ = "jwt"}
    now <- systemSeconds <$> getSystemTime
    let expire = now + days * 1
    let p = JwtPayload{user = u, iat = now, exp = expire}
    pure Jwt{header = h, payload = p}

encode64 :: BS.ByteString -> T.Text
encode64 v = extractBase64 $ encodeBase64Unpadded v

encodePart :: (ToJSON a) => a -> T.Text
encodePart v = encode64 . BS.toStrict $ encode v

encodeJwtNoSign :: Jwt -> T.Text
encodeJwtNoSign jwt = (encodePart $ header jwt) <> "." <> (encodePart $ payload jwt)

encodeJwt :: T.Text -> Jwt -> T.Text
encodeJwt sec jwt = base <> "." <> s
  where
    base = encodeJwtNoSign jwt
    s = encode64 . BS.pack . show . hmacGetDigest $ signJwt sec (Left base)

decodeJwt :: T.Text -> T.Text -> IO (Maybe Jwt)
decodeJwt sec raw =
    case uncons sp of
        Nothing -> pure Nothing
        Just (hder, rest) -> case uncons rest of
            Just (pload, s : []) -> parseJwt sec hder pload s
            _ -> pure Nothing
  where
    sp = T.split (\c -> c == '.') raw

parseJwt :: T.Text -> T.Text -> T.Text -> T.Text -> IO (Maybe Jwt)
parseJwt sec hder pload s =
    case decodedHeader of
        Nothing -> pure Nothing
        Just h -> case decodedPayload of
            Nothing -> pure Nothing
            Just p -> case decodedSign of
                Nothing -> pure Nothing
                Just v ->
                    let jwt = Jwt{header = h, payload = p}
                     in do
                            ver <- verifyJwt sec jwt (HMAC v)
                            if ver then pure $ Just jwt else pure Nothing
  where
    decodedHeader = decode64 hder >>= decodePart :: Maybe JwtHeader
    decodedPayload = decode64 pload >>= decodePart :: Maybe JwtPayload
    decodedSign = decode64 s >>= digestFromByteString :: Maybe (Digest SHA3_512)

decode64 :: T.Text -> Maybe BS.ByteString
decode64 raw = case decodeBase64UnpaddedUntyped $ encodeUtf8 raw of
    Left _ -> Nothing
    Right v -> Just v

decodePart :: (FromJSON a) => BS.ByteString -> Maybe a
decodePart raw = decode $ BSL.fromStrict raw

signJwt :: T.Text -> Either T.Text Jwt -> HMAC SHA3_512
signJwt sec content = case content of
    Left c ->
        hmac ((encodeUtf8 sec) :: BS.ByteString) ((encodeUtf8 c) :: BS.ByteString)
    Right jwt -> signJwt sec (Left $ encodeJwtNoSign jwt)

verifyJwt :: T.Text -> Jwt -> HMAC SHA3_512 -> IO Bool
verifyJwt sec jwt s =
    if (signJwt sec (Right jwt)) == s
        then systemSeconds <$> getSystemTime >>= (\v -> pure $ v > (exp $ payload jwt))
        else pure False
