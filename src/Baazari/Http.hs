module Baazari.Http where

import Baazari.Types
import Data.CountryCodes
import Text.Email.Validate
import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import Data.Digest.OpenSSL.HMAC
import Network.HTTP.Simple
import Data.ByteString.Builder
import Data.ByteString.Lazy

accessKeyId :: AccessKeyId

marketplaceIds :: [MarketplaceId]
marketplaceIds = [ "A2EUQ1WTGCTBG2"
                 , "ATVPDKIKX0DER"
                 , "A1AM78C64UM0Y8" ]

sellerId :: SellerId

secretKey :: SecretKey

apiVersion :: Version
apiVersion = "2015-06-01"

toParam :: ByteString
        -> Maybe ByteString
        -> (ByteString, Maybe ByteString)
toParam name value = (name, value)

fromParam :: (ByteString, Maybe ByteString)
          -> ByteString
fromParam (name, value) =
  if value == Just val
  then name <> "=" <> val <> "&"
  else ""

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

renderAmazonOrderId ::
     AmazonOrderId
  -> ByteString
renderAmazonOrderId i =
  encodeUtf8 . unAmazonOrderId $ i

renderSellerOrderId ::
     Maybe SellerOrderId
  -> Maybe ByteString
renderSellerOrderId i =
  fmap (encodeUtf8 . unSellerOrderId) i

renderOrderItemId ::
     OrderItemId
  -> ByteString
renderOrderItemId i =
  encodeUtf8 . unOrderItemId $ i

renderQuantity :: Int -> ByteString
renderQuantity n =
  toStrict . toLazyByteString . intDec $ n

itemToParams ::
     Int
  -> Item
  -> [(ByteString, Maybe ByteString)]
itemToParams itemNumber item =
  [ ( itemParam <> "OrderItemId"
    , Just $ renderOrderItemId . orderItemId $ item )
  , ( itemParam <> "Quantity"
    , Just $ renderQuantity . quantity $ item ) ]
  where
    itemParam =
         "Item."
      <> renderQuantity itemNumber
      <> "."

itemsToParams ::
     [Item]
  -> [(ByteString, Maybe ByteString)]
itemsToParams items = mconcat $
  zipWith itemToParams [1..] items

addressToParams ::
     Address
  -> [(ByteString, Maybe ByteString)]
addressToParams address =
  [ toParam "Address.Name"
      (Just . renderAddressName (name address))
  , toParam "Address.AddressLine1"
      (Just . renderAddressLine (addressLine1 address))
  , toParam "Address.AddressLine2"
      (renderSecondaryAddressLine (addressLine2 address))
  , toParam "Address.AddressLine3"
      (renderSecondaryAddressLine (addressLine3 address))
  , toParam "Address.DistrictOrCounty"
      (renderCounty (districtOrCounty address))
  , toParam "Address.Email"
      (Just . toByteString (email address))
  , toParam "Address.City"
      (Just . renderCity (city address))
  , toParam "Address.StateOrProvinceCode"
      (renderState (stateOrProvinceCode address))
  , toParam "Address.PostalCode"
      (Just . renderPostalCode (postalCode address))
  , toParam "Address.CountryCode"
      (Just . renderCountryCode (countryCode address))
  , toParam "Address.Phone"
      (Just . renderPhoneNumber (phone address)) ]

renderAddressName ::
     AddressName
  -> ByteString
renderAddressName a =
  encodeUtf8 . unAddressName $ a

renderAddressLine ::
     AddressLine
  -> ByteString
renderAddressName a =
  encodeUtf8 . unAddressName $ a

renderSecondaryAddressLine ::
     Maybe SecondaryAddressLine
  -> Maybe ByteString
renderSecondaryAddressLine a =
  fmap (encodeUtf8 . unSecondaryAddressName) a

renderCounty ::
     Maybe County
  -> Maybe ByteString
renderCounty c =
  fmap (encodeUtf8 . unCounty) c

renderCity ::
     City
  -> ByteString
renderCity c =
  encodeUtf8 . unCity $ c

renderState ::
     Maybe State
  -> Maybe ByteString
renderState s =
  fmap (encodeUtf8 . unState) s

renderPostalCode ::
     PostalCode
  -> ByteString
renderPostalCode pc =
  encodeUtf8 . unPostalCode $ pc

renderCountryCode ::
     CountryCode
  -> ByteString
renderCountryCode c =
  encodeUtf8 . toText $ c

renderPhoneNumber ::
     PhoneNumber
  -> ByteString
renderPhoneNumber n =
  encodeUtf8 . unPhoneNumber $ n

makeQuery :: Endpoint -> Request
makeQuery host = 
    setRequestMethod "POST"
  $ setRequestSecure True
  $ setRequestHost (renderEndpoint host)
  $ defaultRequest
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
unsignedGetEligibleShippingServices ::
      Endpoint
   -> ShipmentRequestDetails
   -> UTCTime
   -> ByteString
unsignedGetEligibleShippingServices ep deets time =
     genericQueryStringStart ep
  <> mconcat . fmap fromParam . sort $
       genericParams time ++
       idListToParams marketplaceIds ++
       shipmentRequestDetailsToParams deets

genericQueryStringStart ::
     Endpoint
  -> ByteString
genericQueryStringStart ep =
     "POST\n"
  <> renderEndpoint ep
  <> "\n/\n"

genericParams :: UTCTime
              -> [ByteString, Maybe ByteString)]
genericParams time =
  [ ("AWSAccessKeyId", Just accessKeyId)
  , ("Action", Just "GetEligibleShippingServices")
  , ("SellerId", Just sellerId)
  , ("SignatureMethod", Just "HmacSHA256")
  , ("SignatureVersion", Just "2")
  , ("Timestamp", Just (renderTime time))
  , ("Version", Just apiVersion) ]

renderAccessKeyId aId
--    <> "Action=GetEligibleShippingServices&"
--    <> renderShipmentRequestDetails params
--    <> renderAuthToken token
--    <> renderMarketplaceIds mIds
--    <> renderSellerId sId
--    <> "SignatureMethod=HmacSHA256&"
--    <> "SignatureVersion=2&"
--    <> "Timestamp=" <> time <> "&"
--    <> "Version=2015-10-01"
