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

-------------------------------------------------------------
-- MAKING A PARAM LIST OUT OF ShipmetRequestDetails
-------------------------------------------------------------

renderAmazonOrderId ::
     AmazonOrderId
  -> ByteString
renderAmazonOrderId =
  encodeUtf8 . unAmazonOrderId

renderSellerOrderId ::
     Maybe SellerOrderId
  -> Maybe ByteString
renderSellerOrderId =
  fmap (encodeUtf8 . unSellerOrderId)

renderOrderItemId ::
     OrderItemId
  -> ByteString
renderOrderItemId =
  encodeUtf8 . unOrderItemId

renderQuantity :: Int -> ByteString
renderQuantity =
  toStrict . toLazyByteString . intDec


--PARAM LIST FROM [Item]

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


--PARAM LIST FROM Address

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
renderAddressName =
  encodeUtf8 . unAddressName

renderAddressLine ::
     AddressLine
  -> ByteString
renderAddressName =
  encodeUtf8 . unAddressName

renderSecondaryAddressLine ::
     Maybe SecondaryAddressLine
  -> Maybe ByteString
renderSecondaryAddressLine =
  fmap (encodeUtf8 . unSecondaryAddressName)

renderCounty ::
     Maybe County
  -> Maybe ByteString
renderCounty =
  fmap (encodeUtf8 . unCounty)

renderCity ::
     City
  -> ByteString
renderCity =
  encodeUtf8 . unCity

renderState ::
     Maybe State
  -> Maybe ByteString
renderState =
  fmap (encodeUtf8 . unState)

renderPostalCode ::
     PostalCode
  -> ByteString
renderPostalCode =
  encodeUtf8 . unPostalCode

renderCountryCode ::
     CountryCode
  -> ByteString
renderCountryCode =
  encodeUtf8 . toText

renderPhoneNumber ::
     PhoneNumber
  -> ByteString
renderPhoneNumber =
  encodeUtf8 . unPhoneNumber

--MAKE QUERY PARAMS FROM PackageDimensions

renderLength ::
     Maybe Length
  -> Maybe ByteString
renderLength =
  fmap (toStrict . toLazyByteString . floatDec . unLength)

renderWidth ::
     Maybe Width
  -> Maybe ByteString
renderWidth =
  fmap (toStrict . toLazyByteString . floatDec . unWidth)

renderHeight ::
     Maybe Height
  -> Maybe ByteString
renderHeight =
  fmap (toStrict . toLazyByteString . floatDec . unHeight)

renderLengthUnit ::
     Maybe LengthUnit
  -> Maybe ByteString
renderLengthUnit =
  fmap (\unit -> case unit of
         Inches -> "inches"
         Centimeters -> "centimeters")

renderPredefinedPackageDimensions ::
     Maybe PredefinedPackageDimensions
  -> Maybe ByteString
renderPredefinedPackageDimensions =
  fmap (\predefined -> case predefined of
         FedEx_Box_10kg -> "FedEx_Box_10kg"
         FedEx_Box_25kg -> "FedEx_Box_25kg"
         FedEx_Box_Extra_Large_1 -> "FedEx_Box_Extra_Large_1"
         FedEx_Box_Extra_Large_2 -> "FedEx_Box_Extra_Large_2"
         FedEx_Box_Large_1 -> "FedEx_Box_Large_1"
         FedEx_Box_Large_2 -> "FedEx_Box_Large_2"
         FedEx_Box_Medium_1 -> "FedEx_Box_Medium_1"
         FedEx_Box_Medium_2 -> "FedEx_Box_Medium_2"
         FedEx_Box_Small_1 -> "FedEx_Box_Small_1"
         FedEx_Box_Small_2 -> "FedEx_Box_Small_2"
         FedEx_Envelope -> "FedEx_Envelope"
         FedEx_Padded_Pak -> "FedEx_Padded_Pak"
         FedEx_Pak_1 -> "FedEx_Pak_1"
         FedEx_Pak_2 -> "FedEx_Pak_2"
         FedEx_Tube -> "FedEx_Tube"
         FedEx_XL_Pak -> "FedEx_XL_Pak"
         UPS_Box_10kg -> "UPS_Box_10kg"
         UPS_Box_25kg -> "UPS_Box_25kg"
         UPS_Express_Box -> "UPS_Express_Box"
         UPS_Express_Box_Large -> "UPS_Express_Box_Large"
         UPS_Express_Box_Medium -> "UPS_Express_Box_Medium"
         UPS_Express_Box_Small -> "UPS_Express_Box_Small"
         UPS_Express_Envelope -> "UPS_Express_Envelope"
         UPS_Express_Hard_Pak -> "UPS_Express_Hard_Pak"
         UPS_Express_Legal_Envelope -> "UPS_Express_Legal_Envelope"
         UPS_Express_Pak -> "UPS_Express_Pak"
         UPS_Express_Tube -> "UPS_Express_Tube"
         UPS_Laboratory_Pak -> "UPS_Laboratory_Pak"
         UPS_Pad_Pak -> "UPS_Pad_Pak"
         UPS_Pallet -> "UPS_Pallet"
         USPS_Card -> "USPS_Card"
         USPS_Flat -> "USPS_Flat"
         USPS_FlatRateCardboardEnvelope -> "USPS_FlatRateCardboardEnvelope"
         USPS_FlatRateEnvelope -> "USPS_FlatRateEnvelope"
         USPS_FlatRateGiftCardEnvelope -> "USPS_FlatRateGiftCardEnvelope"
         USPS_FlatRateLegalEnvelope -> "USPS_FlatRateLegalEnvelope"
         USPS_FlatRatePaddedEnvelope -> "USPS_FlatRatePaddedEnvelope"
         USPS_FlatRateWindowEnvelope -> "USPS_FlatRateWindowEnvelope"
         USPS_LargeFlatRateBoardGameBox -> "USPS_LargeFlatRateBoardGameBox"
         USPS_LargeFlatRateBox -> "USPS_LargeFlatRateBox"
         USPS_Letter -> "USPS_Letter"
         USPS_MediumFlatRateBox1 -> "USPS_MediumFlatRateBox1"
         USPS_MediumFlatRateBox2 -> "USPS_MediumFlatRateBox2"
         USPS_RegionalRateBoxA1 -> "USPS_RegionalRateBoxA1"
         USPS_RegionalRateBoxA2 -> "USPS_RegionalRateBoxA2"
         USPS_RegionalRateBoxB1 -> "USPS_RegionalRateBoxB1"
         USPS_RegionalRateBoxB2 -> "USPS_RegionalRateBoxB2"
         USPS_RegionalRateBoxC -> "USPS_RegionalRateBoxC"
         USPS_SmallFlatRateBox -> "USPS_SmallFlatRateBox"
         USPS_SmallFlatRateEnvelope -> "USPS_SmallFlatRateEnvelope")


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
