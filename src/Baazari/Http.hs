module Baazari.Http where

import Baazari.Types
import Data.ByteString
import Network.HTTP.Simple
import Data.ByteString.Builder

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

