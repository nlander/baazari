module Baazari.Http where

import Baazari.Types
import Data.Time
import Data.Fixed
import Data.Time.LocalTime
import Data.Time.Zones.Internal
import Data.Text.Encoding
import Data.Monoid
import Data.Maybe
import Data.List
import Data.CountryCodes
import Text.Email.Validate
import Data.ByteString
import Data.ByteArray
import Crypto.Hash
import Crypto.MAC.HMAC
import qualified Data.ByteString.Base64 as B64
import Network.HTTP.Simple
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB
       (toStrict
       ,ByteString)

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
  if value == Nothing
  then ""
  else name <> "=" <> fromJust value <> "&"

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

shipmentRequestDetailsToParams ::
     ShipmentRequestDetails
  -> [(ByteString, Maybe ByteString)]
shipmentRequestDetailsToParams srds =
  [ toParam "ShipmentRequestDetails.AmazonOrderId" $
      Just . renderAmazonOrderId . amazonOrderId $ srds
  , toParam "ShipmentRequestDetails.SellerOrderId" $
      renderSellerOrderId . sellerOrderId $ srds ] ++
  ( nestParams "ShipmentRequestDetails." $
      itemsToParams . itemList $ srds ) ++
  ( nestParams "ShipmentRequestDetails.ShipFrom" $
      addressToParams . shipFromAddress $ srds ) ++
  ( nestParams "ShipmentRequestDetails." $
      packageDimensionsToParams . packageDimensions $ srds ) ++
  ( nestParams "ShipmentRequestDetails." $
      weightToParams . weight $ srds ) ++
  [ toParam "ShipmentRequestDetails.MustArriveByUTCTime" $
      fmap renderUTCTime . mustArriveByUTCTime $ srds
  , toParam "ShipmentRequestDetails.RequestShipUTCTime" $
      fmap renderUTCTime . requestShipUTCTime $ srds ] ++
  ( nestParams "ShipmentRequestDetails." $
      requestShippingServiceOptionsToParams
        . requestShippingServiceOptions $ srds )

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
  LB.toStrict . toLazyByteString . intDec


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
  Prelude.zipWith itemToParams [1..] items


--PARAM LIST FROM Address

addressToParams ::
     Address
  -> [(ByteString, Maybe ByteString)]
addressToParams address =
  [ toParam "Address.Name" $
      Just . renderAddressName $ name address
  , toParam "Address.AddressLine1" $
      Just . renderAddressLine $ addressLine1 address
  , toParam "Address.AddressLine2" $
      renderSecondaryAddressLine $ addressLine2 address
  , toParam "Address.AddressLine3" $
      renderSecondaryAddressLine $ addressLine3 address
  , toParam "Address.DistrictOrCounty" $
      renderCounty $ districtOrCounty address
  , toParam "Address.Email" $
      Just . toByteString $ email address
  , toParam "Address.City" $
      Just . renderCity $ city address
  , toParam "Address.StateOrProvinceCode" $
      renderState $ stateOrProvinceCode address
  , toParam "Address.PostalCode" $
      Just . renderPostalCode $ postalCode address
  , toParam "Address.CountryCode" $
      Just . renderCountryCode $ countryCode address
  , toParam "Address.Phone" $
      Just . renderPhoneNumber $ phone address ]

renderAddressName ::
     AddressName
  -> ByteString
renderAddressName =
  encodeUtf8 . unAddressName

renderAddressLine ::
     AddressLine
  -> ByteString
renderAddressLine =
  encodeUtf8 . unAddressLine

renderSecondaryAddressLine ::
     Maybe SecondaryAddressLine
  -> Maybe ByteString
renderSecondaryAddressLine =
  fmap (encodeUtf8 . unSecondaryAddressLine)

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

packageDimensionsToParams ::
     PackageDimensions
  -> [(ByteString, Maybe ByteString)]
packageDimensionsToParams dimensions =
  [ ( toParam "PackageDimensions.Length" $
        renderLength $ len dimensions )
  , ( toParam "PackageDimensions.Width" $
        renderWidth $ width dimensions )
  , ( toParam "PackageDimensions.Height" $
        renderHeight $ height dimensions )
  , ( toParam "PackageDimensions.Unit" $
        renderLengthUnit $ unit dimensions )
  , ( toParam "PackageDimensions.PredefinedPackageDimensions" $
        renderPredefinedPackageDimensions $
          predefinedPackageDimensions dimensions )
  ]

weightToParams ::
     Weight
  -> [(ByteString, Maybe ByteString)]
weightToParams weight =
  [ ( toParam "Weight.Value" $
      Just . renderWeightValue $ value weight )
  , ( toParam "Weight.Unit" $
      Just . renderWeightUnit $ units weight ) ]

renderLength ::
     Maybe Length
  -> Maybe ByteString
renderLength =
  fmap (LB.toStrict . toLazyByteString . floatDec . unLength)

renderWidth ::
     Maybe Width
  -> Maybe ByteString
renderWidth =
  fmap (LB.toStrict . toLazyByteString . floatDec . unWidth)

renderHeight ::
     Maybe Height
  -> Maybe ByteString
renderHeight =
  fmap (LB.toStrict . toLazyByteString . floatDec . unHeight)

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

renderWeightValue ::
     WeightValue
  -> ByteString
renderWeightValue =
  LB.toStrict . toLazyByteString . floatDec . unWeightValue

renderWeightUnit ::
     WeightUnit
  -> ByteString
renderWeightUnit Ounces = "ounces"
renderWeightUnit Grams  = "grams"

renderUTCTime ::
     UTCTime
  -> ByteString
renderUTCTime t =
     (renderDay . utctDay $ t)
  <> (renderTimeOfDay . timeToTimeOfDay . utctDayTime $ t)

renderDay ::
     Day
  -> ByteString
renderDay = renderGregorian . toGregorian

renderGregorian ::
     (Integer, Int, Int)
  -> ByteString
renderGregorian (year, month, day) =
  rendY year <> "-" <> rendInt month <> "-" <> rendInt day
  where rendY  = LB.toStrict
               . toLazyByteString
               . integerDec

rendInt :: Int -> ByteString
rendInt n | 0 <= n && n < 10 =
                "0"
             <> ( LB.toStrict
                . toLazyByteString
                . intDec ) n
          | otherwise   =
            ( LB.toStrict
            . toLazyByteString
            . intDec ) n

rendInteger :: Integer -> ByteString
rendInteger n | 0 <= n && n < 10 =
                   "0"
                <> ( LB.toStrict
                   . toLazyByteString
                   . integerDec ) n
              | otherwise   =
                ( LB.toStrict
                . toLazyByteString
                . integerDec ) n

renderTimeOfDay ::
     TimeOfDay
  -> ByteString
renderTimeOfDay t =
     "T"
  <> ( rendInt
     . todHour ) t
  <> "%3A"
  <> ( rendInt
     . todMin ) t
  <> "%3A"
  <> ( rendPico
     . todSec ) t
  <> "Z"

rendPico :: Pico -> ByteString
rendPico p = let smallInt = picoToInteger p `div` 10000000000 in
     ( rendInteger
     $ smallInt `div` 100 )
  <> "."
  <> ( rendInteger
     $ smallInt `rem` 100 )

--PARAM LIST FROM ShippingServiceOptions

renderDeliveryExperience ::
     DeliveryExperience
  -> ByteString
renderDeliveryExperience de = case de of
  DeliveryConfirmationWithAdultSignature
    -> "DeliveryConfirmationWithAdultSignature"
  DeliveryConfirmationWithSignature
    -> "DeliveryConfirmationWithSignature"
  DeliveryConfirmationWithoutSignature
    -> "DeliveryConfirmationWithoutSignature"
  NoTracking
    -> "NoTracking"

renderBool ::
     Bool
  -> ByteString
renderBool True  = "true"
renderBool False = "false"

requestShippingServiceOptionsToParams ::
     ShippingServiceOptions
  -> [(ByteString, Maybe ByteString)]
requestShippingServiceOptionsToParams o =
  [ toParam "RequestShippingServiceOptions.DeliveryExperience" $
      Just . renderDeliveryExperience . deliveryExperience $ o
  , toParam "RequestShippingServiceOptions.CarrierWillPickUp" $
      Just . renderBool . carrierWillPickUp $ o
  ] ++ nestParams "RequestShippingServiceOptions."
         ( declaredValueToParams . declaredValue $ o )

nestParams ::
     ByteString
  -> [(ByteString, Maybe ByteString)]
  -> [(ByteString, Maybe ByteString)]
nestParams name oldParams =
  [(name <> fst tup, snd tup) | tup <- oldParams]

declaredValueToParams ::
     Maybe CurrencyAmount
  -> [(ByteString, Maybe ByteString)]
declaredValueToParams c =
  [ toParam "DeclaredValue.CurrencyCode" $
      fmap (renderCurrencyCode . currencyCode) c
  , toParam "DeclaredValue.Amount" $
      fmap (renderAmount . amount) c ]

renderAmount ::
     Float
  -> ByteString
renderAmount =
  LB.toStrict . toLazyByteString . floatDec

renderCurrencyCode ::
     CurrencyCode
  -> ByteString
renderCurrencyCode code = case code of
  AED -> "AED"
  AFN -> "AFN"
  ALL -> "ALL"
  AMD -> "AMD"
  ANG -> "ANG"
  AOA -> "AOA"
  ARS -> "ARS"
  AUD -> "AUD"
  AWG -> "AWG"
  AZN -> "AZN"
  BAM -> "BAM"
  BBD -> "BBD"
  BDT -> "BDT"
  BGN -> "BGN"
  BHD -> "BHD"
  BIF -> "BIF"
  BMD -> "BMD"
  BND -> "BND"
  BOB -> "BOB"
  BOV -> "BOV"
  BRL -> "BRL"
  BSD -> "BSD"
  BTN -> "BTN"
  BWP -> "BWP"
  BYR -> "BYR"
  BZD -> "BZD"
  CAD -> "CAD"
  CDF -> "CDF"
  CHE -> "CHE"
  CHF -> "CHF"
  CHW -> "CHW"
  CLF -> "CLF"
  CLP -> "CLP"
  CNY -> "CNY"
  COP -> "COP"
  COU -> "COU"
  CRC -> "CRC"
  CUC -> "CUC"
  CUP -> "CUP"
  CVE -> "CVE"
  CZK -> "CZK"
  DJF -> "DJF"
  DKK -> "DKK"
  DOP -> "DOP"
  DZD -> "DZD"
  EGP -> "EGP"
  ERN -> "ERN"
  ETB -> "ETB"
  EUR -> "EUR"
  FJD -> "FJD"
  FKP -> "FKP"
  GBP -> "GBP"
  GEL -> "GEL"
  GHS -> "GHS"
  GIP -> "GIP"
  GMD -> "GMD"
  GNF -> "GNF"
  GTQ -> "GTQ"
  GYD -> "GYD"
  HKD -> "HKD"
  HNL -> "HNL"
  HRK -> "HRK"
  HTG -> "HTG"
  HUF -> "HUF"
  IDR -> "IDR"
  ILS -> "ILS"
  INR -> "INR"
  IQD -> "IQD"
  IRR -> "IRR"
  ISK -> "ISK"
  JMD -> "JMD"
  JOD -> "JOD"
  JPY -> "JPY"
  KES -> "KES"
  KGS -> "KGS"
  KHR -> "KHR"
  KMF -> "KMF"
  KPW -> "KPW"
  KRW -> "KRW"
  KWD -> "KWD"
  KYD -> "KYD"
  KZT -> "KZT"
  LAK -> "LAK"
  LBP -> "LBP"
  LKR -> "LKR"
  LRD -> "LRD"
  LSL -> "LSL"
  LYD -> "LYD"
  MAD -> "MAD"
  MDL -> "MDL"
  MGA -> "MGA"
  MKD -> "MKD"
  MMK -> "MMK"
  MNT -> "MNT"
  MOP -> "MOP"
  MRO -> "MRO"
  MUR -> "MUR"
  MVR -> "MVR"
  MWK -> "MWK"
  MXN -> "MXN"
  MXV -> "MXV"
  MYR -> "MYR"
  MZN -> "MZN"
  NAD -> "NAD"
  NGN -> "NGN"
  NIO -> "NIO"
  NOK -> "NOK"
  NPR -> "NPR"
  NZD -> "NZD"
  OMR -> "OMR"
  PAB -> "PAB"
  PEN -> "PEN"
  PGK -> "PGK"
  PHP -> "PHP"
  PKR -> "PKR"
  PLN -> "PLN"
  PYG -> "PYG"
  QAR -> "QAR"
  RON -> "RON"
  RSD -> "RSD"
  RUB -> "RUB"
  RWF -> "RWF"
  SAR -> "SAR"
  SBD -> "SBD"
  SCR -> "SCR"
  SDG -> "SDG"
  SEK -> "SEK"
  SGD -> "SGD"
  SHP -> "SHP"
  SLL -> "SLL"
  SOS -> "SOS"
  SRD -> "SRD"
  SSP -> "SSP"
  STD -> "STD"
  SYP -> "SYP"
  SZL -> "SZL"
  THB -> "THB"
  TJS -> "TJS"
  TMT -> "TMT"
  TND -> "TND"
  TOP -> "TOP"
  TRY -> "TRY"
  TTD -> "TTD"
  TWD -> "TWD"
  TZS -> "TZS"
  UAH -> "UAH"
  UGX -> "UGX"
  USD -> "USD"
  USN -> "USN"
  USS -> "USS"
  UYI -> "UYI"
  UYU -> "UYU"
  UZS -> "UZS"
  VEF -> "VEF"
  VND -> "VND"
  VUV -> "VUV"
  WST -> "WST"
  XAF -> "XAF"
  XAG -> "XAG"
  XAU -> "XAU"
  XBA -> "XBA"
  XBB -> "XBB"
  XBC -> "XBC"
  XBD -> "XBD"
  XCD -> "XCD"
  XDR -> "XDR"
  XFU -> "XFU"
  XOF -> "XOF"
  XPD -> "XPD"
  XPF -> "XPF"
  XPT -> "XPT"
  XSU -> "XSU"
  XTS -> "XTS"
  XUA -> "XUA"
  XXX -> "XXX"
  YER -> "YER"
  ZAR -> "ZAR"
  ZMW -> "ZMW"


makeQuery :: Endpoint -> Request
makeQuery host = 
    setRequestMethod "POST"
  $ setRequestSecure True
  $ setRequestHost (renderEndpoint host)
  $ defaultRequest

signatureToParam ::
     ByteString
  -> [(ByteString, Maybe ByteString)]
signatureToParam s =
  [ ( "SecretKey"
    , Just s ) ]

getEligibleShippingServices ::
     Endpoint
  -> ShipmentRequestDetails
  -> IO (Response LB.ByteString)
getEligibleShippingServices ep srds = do
  params <- getEligibleShippingServicesUnsignedParams srds
  httpLBS $ setRequestQueryString
          ( request params )
          $ makeQuery ep
  where
    signature params = sign
              $ getEligibleShippingServicesUnsigned ep params
    sign :: ByteString -> ByteString
    sign = convert
         . hmacGetDigest
         . (hmac secretKey :: ByteString -> HMAC SHA256)
    request params = params ++ signatureToParam (signature params)

getEligibleShippingServicesUnsigned ::
     Endpoint
  -> [(ByteString, Maybe ByteString)]
  -> ByteString
getEligibleShippingServicesUnsigned ep params =
     genericQueryStringStart ep
  <> flattenParams params

getEligibleShippingServicesUnsignedParams ::
     ShipmentRequestDetails
  -> IO [(ByteString, Maybe ByteString)]
getEligibleShippingServicesUnsignedParams srds =
      Data.List.sort
   .  (\ time ->
            shipmentRequestDetailsToParams srds
         ++ genericParams time )
  <$> getCurrentTime

flattenParams ::
     [(ByteString, Maybe ByteString)]
  -> ByteString
flattenParams params = flip (flip Prelude.foldr "") params
  (\tup acc -> case snd tup of
    Nothing   -> acc
    Just val  -> let param = fst tup <> "=" <> val in
                   if Data.ByteString.null acc
                   then param
                   else param <> "&")

genericQueryStringStart ::
     Endpoint
  -> ByteString
genericQueryStringStart ep =
     "POST\n"
  <> renderEndpoint ep
  <> "\n/\n"

genericParams :: UTCTime
              -> [(ByteString, Maybe ByteString)]
genericParams time =
  [ ("AWSAccessKeyId", Just accessKeyId)
  , ("Action", Just "GetEligibleShippingServices")
  , ("SellerId", Just sellerId)
  , ("SignatureMethod", Just "HmacSHA256")
  , ("SignatureVersion", Just "2")
  , ("Timestamp", Just (renderUTCTime time))
  , ("Version", Just apiVersion) ]
