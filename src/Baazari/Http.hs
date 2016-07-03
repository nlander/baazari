module Baazari.Http where

import Baazari.Types
import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import Data.Digest.OpenSSL.HMAC
import Network.HTTP.Simple
import Data.ByteString.Builder

accessKeyId :: AccessKeyId

marketplaceIds :: [MarketplaceId]
marketplaceIds = [ "A2EUQ1WTGCTBG2"
                 , "ATVPDKIKX0DER"
                 , "A1AM78C64UM0Y8" ]

sellerId :: SellerId

secretKey :: SecretKey

toParam :: ByteString
        -> ByteString
        -> (ByteString, Maybe ByteString)
toParam name value = (name, Just value)

renderEndpoint :: Endpoint -> ByteString
renderEndpoint NorthAmerica =
  "mws.amazonservices.com"
renderEndpoint Europe =
  "mws-eu.amazonservices.com"
renderEndpoint India =
  "mws.amazonservices.in"
renderEndpoint China =
  "mws.amazonservices.com.cn"
renderEndpoint Japan =
  "mws.amazonservices.jp"

makeQuery :: Endpoint -> Request
makeQuery host = 
    setRequestMethod "POST"
  $ setRequestSecure True
  $ setRequestHost (renderEndpoint host)
  $ defaultRequest
--          -> AccessKeyId
--          -> Action
--          -> Parameters
--          -> AuthToken
--          -> Maybe MarketpleceIdList
--          -> SellerId
--          -> SignatureMethod
--          -> Timestamp

--getEligibleShippingServices :: Endpoint
--                            -> AccessKeyId
--                            -> ShipmentRequestDetails
--                            -> [MarketplaceId]
--                            -> SellerId
--                            -> SecretKey
--                            -> Request
--                            -> IO (Response ByteString)
--getEligibleShippingServices ep aId params token mIds sId key req = undefined
--  do
--    time <- renderTime <$> getCurrentTime
--    signature <- fmap B64.encode $ hmac sha256 key $ unsigned time
--  where unsigned time =
--       "POST\n"
--    <> renderEndpoint ep
--    <> "\n/\n"
--    <> renderAccessKeyId aId
--    <> "Action=GetEligibleShippingServices&"
--    <> renderShipmentRequestDetails params
--    <> renderAuthToken token
--    <> renderMarketplaceIds mIds
--    <> renderSellerId sId
--    <> "SignatureMethod=HmacSHA256&"
--    <> "SignatureVersion=2&"
--    <> "Timestamp=" <> time <> "&"
--    <> "Version=2015-10-01"
