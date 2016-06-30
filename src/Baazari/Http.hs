module Baazari.Http where

import Baazari.Types
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Builder

renderEndpoint :: Endpoint -> Builder
renderEndpoint NorthAmerica =
  byteString "mws.amazonservices.com"
renderEndpoint Europe =
  byteString "mws-eu.amazonservices.com"
renderEndpoint India =
  byteString "mws.amazonservices.in"
renderEndpoint China =
  byteString "mws.amazonservices.com.cn"
renderEndpoint Japan =
  byteString "mws.amazonservices.jp"

--makeQuery :: HttpAction
--          -> Endpoint
--          -> AccessKeyId
--          -> Action
--          -> Parameters
--          -> AuthToken
--          -> Maybe MarketpleceIdList
--          -> SellerId
--          -> SignatureMethod
--          -> Timestamp
