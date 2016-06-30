{-# LANGUAGE OverloadedStrings #-}
module Baazari.Types where

import Currency
import Data.CountryCodes
import Data.Fixed
import qualified Data.Text as T
import Data.Time
import Text.Email.Validate
import Data.ByteString.Builder

-- getCurrentDay :: IO Day
-- getCurrentDay = getCurrentTime >>= return . utctDay

newtype AmazonOrderId =
  AmazonOrderId { unAmazonOrderId :: T.Text }
  deriving (Eq, Show)

newtype SellerOrderId =
  SellerOrderId { unSellerOrderId :: T.Text }
  deriving (Eq, Show)

data ShipmentRequestDetails =
       ShipmentRequestDetails
         { amazonOrderId                  :: AmazonOrderId
         , sellerOrderId                  :: Maybe SellerOrderId
         , itemList                       :: [Item]
         , shipFromAddress                :: Address
         , packageDimensions              :: PackageDimensions
         , weight                         :: Weight
         , mustArriveByUTCTime            :: Maybe UTCTime
         , requestShipUTCTime             :: Maybe UTCTime
         , requestShippingServiceOptions  :: ShippingServiceOptions }

newtype ShippingServiceName =
  ShippingServiceName { unShippingServiceName :: T.Text }
  deriving (Eq, Show)

newtype CarrierName =
  CarrierName { unCarrierName :: T.Text }
  deriving (Eq, Show)

newtype ShippingServiceId =
  ShippingServiceId { unShippingServiceId :: T.Text }
  deriving (Eq, Show)

newtype ShippingServiceOfferId =
  ShippingServiceOfferId { unShippingServiceOfferId :: T.Text }
  deriving (Eq, Show)

data ShippingService =
       ShippingService
         { shippingServiceName              :: ShippingServiceName
         , carrierName                      :: CarrierName
         , shippingServiceId                :: ShippingServiceId
         , shippingServiceOfferId           :: ShippingServiceOfferId
         , serviceShipUTCTime               :: UTCTime
         , earliestEstimatedDeliveryUTCTime :: Maybe UTCTime
         , latestEstimatedDeliveryUTCTime   :: Maybe UTCTime
         , rate                             :: (Fixed E2, Currency)
         , shippingServiceOptions           :: ShippingServiceOptions
         }

newtype TemporarilyUnavailableCarrier =
          TemporarilyUnavailableCarrier CarrierName

newtype TermsAndConditionsNotAcceptedCarrier =
          TermsAndConditionsNotAcceptedCarrier CarrierName

newtype OrderItemId =
  OrderItemId { unOrderItemId :: T.Text }
  deriving (Eq, Show)

data Item =
       Item
         { orderItemId :: OrderItemId
         , quantity    :: Int
         }

newtype AddressName =
  AddressName { unAddressName :: T.Text }
  deriving (Eq, Show)

newtype AddressLine =
  AddressLine { unAddressLine :: T.Text }
  deriving (Eq, Show)

newtype City =
  City { unCity :: T.Text }
  deriving (Eq, Show)

newtype County =
  County { unCounty :: T.Text }
  deriving (Eq, Show)

-- or "province"
newtype State =
  State { unState :: T.Text }
  deriving (Eq, Show)

newtype PostalCode =
  PostalCode { unPostalCode :: T.Text }
  deriving (Eq, Show)

newtype PhoneNumber =
  PhoneNumber { unPhoneNumber :: T.Text }
  deriving (Eq, Show)

data Address =
       Address
         { name                :: AddressName
         , addressLine1        :: AddressLine
         , addressLine2        :: Maybe AddressLine
         , addressLine3        :: Maybe AddressLine
         , districtOrCounty    :: Maybe County
         , email               :: EmailAddress
         , city                :: City
         , stateOrProvinceCode :: Maybe State
         , postalCode          :: PostalCode
         -- note that Currency uses a different CountryCode library
         , countryCode         :: CountryCode
         , phone               :: PhoneNumber
         }

newtype Name = Name T.Text

makeName :: T.Text -> Either T.Text Name
makeName name
  | T.length name == 0  = Left "Empty name"
  | T.length name >  30 = Left "Name is too long"
  | T.length name <= 30 = Right $ Name name

makeAddressLine :: T.Text -> Either T.Text AddressLine
makeAddressLine line
  | T.length line == 0   = Left "Empty line"
  | T.length line >  180 = Left "line is too long"
  | T.length line <= 180 = Right $ AddressLine line

newtype SecondaryAddressLine = SecondaryAddressLine T.Text

makeSecondaryAddressLine :: T.Text -> Either T.Text SecondaryAddressLine
makeSecondaryAddressLine line
  | T.length line == 0  = Left "Empty line"
  | T.length line >  60 = Left "line is too long"
  | T.length line <= 60 = Right $ SecondaryAddressLine line

newtype DistrictOrCounty = DistrictOrCounty T.Text

makeDistrictOrCounty :: T.Text -> Either T.Text DistrictOrCounty
makeDistrictOrCounty county
  | T.length county == 0  = Left "Empty county"
  | T.length county >  30 = Left "County is too long"
  | T.length county <= 30 = Right $ DistrictOrCounty county

newtype Email = Email T.Text

makeEmail :: T.Text -> Either T.Text Email
makeEmail email
  | T.length email == 0   = Left "Empty email"
  | T.length email >  256 = Left "Email is too long"
  | T.length email <= 256 = Right $ Email email

makeCity :: T.Text -> Either T.Text City
makeCity city
  | T.length city == 0  = Left "Empty city name"
  | T.length city >  30 = Left "City name is too long"
  | T.length city <= 30 = Right $ City city

newtype StateOrProvinceCode = StateOrProvinceCode T.Text

makeStateOrProvinceCode :: T.Text -> Either T.Text StateOrProvinceCode
makeStateOrProvinceCode state
  | T.length state == 0  = Left "Empty state or province code"
  | T.length state >  30 = Left "State or province code is too long"
  | T.length state <= 30 = Right $ StateOrProvinceCode state

makePostalCode :: T.Text -> Either T.Text PostalCode
makePostalCode code
  | T.length code == 0  = Left "Empty postal code"
  | T.length code >  30 = Left "Postal code is too long"
  | T.length code <= 30 = Right $ PostalCode code

makePhoneNumber :: T.Text -> Either T.Text PhoneNumber
makePhoneNumber number
  | T.length number == 0  = Left "Empty phone number"
  | T.length number >  30 = Left "Phone number is too long"
  | T.length number <= 30 = Right $ PhoneNumber number

data PackageDimensions =
       PackageDimensions
         { len    :: Maybe Length
         , width  :: Maybe Width
         , height :: Maybe Height
         , unit   :: Maybe LengthUnit
         , predefinedPackageDimensions ::
             Maybe PredefinedPackageDimensions
         }

newtype Length = Length Float

makeLength :: Float -> Either T.Text Length
makeLength len
  | len <= 0 = Left "Length must be greater than zero"
  | len >  0 = Right $ Length len

newtype Width = Width Float

makeWidth :: Float -> Either T.Text Width
makeWidth width
  | width <= 0 = Left "Width must be greater than zero"
  | width >  0 = Right $ Width width

newtype Height = Height Float

makeHeight :: Float -> Either T.Text Height
makeHeight height
  | height <= 0 = Left "Height must be greater than zero"
  | height >  0 = Right $ Height height

data LengthUnit =
       Inches
     | Centimeters

data Weight =
       Weight
         { value :: WeightValue
         , units :: WeightUnit
         }

newtype WeightValue = WeightValue Float

makeWeightValue :: Float -> Either T.Text WeightValue
makeWeightValue weight
  | weight <= 0 = Left "Weigth must be greater than zero"
  | weight >  0 = Right $ WeightValue weight

data WeightUnit =
       Ounces
     | Grams

data ShippingServiceOptions =
       ShippingServiceOptions
         { deliveryExperience :: DeliveryExperience
         , declaredValue      :: Maybe CurrencyAmount
         , carrierWillPickUp  :: Bool
         }

data DeliveryExperience =
         DeliveryConfirmationWithAdultSignature
       | DeliveryConfirmationWithSignature
       | DeliveryConfirmationWithoutSignature
       | NoTracking

data CurrencyAmount =
       CurrencyAmount
         { currencyCode :: CurrencyCode
         , amount       :: Float
         }

data PredefinedPackageDimensions =
       FedEx_Box_10kg
     | FedEx_Box_25kg
     | FedEx_Box_Extra_Large_1
     | FedEx_Box_Extra_Large_2
     | FedEx_Box_Large_1
     | FedEx_Box_Large_2
     | FedEx_Box_Medium_1
     | FedEx_Box_Medium_2
     | FedEx_Box_Small_1
     | FedEx_Box_Small_2
     | FedEx_Envelope
     | FedEx_Padded_Pak
     | FedEx_Pak_1
     | FedEx_Pak_2
     | FedEx_Tube
     | FedEx_XL_Pak
     | UPS_Box_10kg
     | UPS_Box_25kg
     | UPS_Express_Box
     | UPS_Express_Box_Large
     | UPS_Express_Box_Medium
     | UPS_Express_Box_Small
     | UPS_Express_Envelope
     | UPS_Express_Hard_Pak
     | UPS_Express_Legal_Envelope
     | UPS_Express_Pak
     | UPS_Express_Tube
     | UPS_Laboratory_Pak
     | UPS_Pad_Pak
     | UPS_Pallet
     | USPS_Card
     | USPS_Flat
     | USPS_FlatRateCardboardEnvelope
     | USPS_FlatRateEnvelope
     | USPS_FlatRateGiftCardEnvelope
     | USPS_FlatRateLegalEnvelope
     | USPS_FlatRatePaddedEnvelope
     | USPS_FlatRateWindowEnvelope
     | USPS_LargeFlatRateBoardGameBox
     | USPS_LargeFlatRateBox
     | USPS_Letter
     | USPS_MediumFlatRateBox1
     | USPS_MediumFlatRateBox2
     | USPS_RegionalRateBoxA1
     | USPS_RegionalRateBoxA2
     | USPS_RegionalRateBoxB1
     | USPS_RegionalRateBoxB2
     | USPS_RegionalRateBoxC
     | USPS_SmallFlatRateBox
     | USPS_SmallFlatRateEnvelope

data CurrencyCode =
       AED
     | AFN
     | ALL
     | AMD
     | ANG
     | AOA
     | ARS
     | AUD
     | AWG
     | AZN
     | BAM
     | BBD
     | BDT
     | BGN
     | BHD
     | BIF
     | BMD
     | BND
     | BOB
     | BOV
     | BRL
     | BSD
     | BTN
     | BWP
     | BYR
     | BZD
     | CAD
     | CDF
     | CHE
     | CHF
     | CHW
     | CLF
     | CLP
     | CNY
     | COP
     | COU
     | CRC
     | CUC
     | CUP
     | CVE
     | CZK
     | DJF
     | DKK
     | DOP
     | DZD
     | EGP
     | ERN
     | ETB
     | EUR
     | FJD
     | FKP
     | GBP
     | GEL
     | GHS
     | GIP
     | GMD
     | GNF
     | GTQ
     | GYD
     | HKD
     | HNL
     | HRK
     | HTG
     | HUF
     | IDR
     | ILS
     | INR
     | IQD
     | IRR
     | ISK
     | JMD
     | JOD
     | JPY
     | KES
     | KGS
     | KHR
     | KMF
     | KPW
     | KRW
     | KWD
     | KYD
     | KZT
     | LAK
     | LBP
     | LKR
     | LRD
     | LSL
     | LYD
     | MAD
     | MDL
     | MGA
     | MKD
     | MMK
     | MNT
     | MOP
     | MRO
     | MUR
     | MVR
     | MWK
     | MXN
     | MXV
     | MYR
     | MZN
     | NAD
     | NGN
     | NIO
     | NOK
     | NPR
     | NZD
     | OMR
     | PAB
     | PEN
     | PGK
     | PHP
     | PKR
     | PLN
     | PYG
     | QAR
     | RON
     | RSD
     | RUB
     | RWF
     | SAR
     | SBD
     | SCR
     | SDG
     | SEK
     | SGD
     | SHP
     | SLL
     | SOS
     | SRD
     | SSP
     | STD
     | SYP
     | SZL
     | THB
     | TJS
     | TMT
     | TND
     | TOP
     | TRY
     | TTD
     | TWD
     | TZS
     | UAH
     | UGX
     | USD
     | USN
     | USS
     | UYI
     | UYU
     | UZS
     | VEF
     | VND
     | VUV
     | WST
     | XAF
     | XAG
     | XAU
     | XBA
     | XBB
     | XBC
     | XBD
     | XCD
     | XDR
     | XFU
     | XOF
     | XPD
     | XPF
     | XPT
     | XSU
     | XTS
     | XUA
     | XXX
     | YER
     | ZAR
     | ZMW
