module Baazari.Types where

import Currency
import Data.CountryCodes
import Data.Fixed
import Data.Text (Text)
import Data.Time
import Text.Email.Validate

-- getCurrentDay :: IO Day
-- getCurrentDay = getCurrentTime >>= return . utctDay

data ShippingServiceOptions =
  ShippingServiceOptions

newtype AmazonOrderId =
  AmazonOrderId { unAmazonOrderId :: Text }
  deriving (Eq, Show)

newtype SellerOrderId =
  SellerOrderId { unSellerOrderId :: Text }
  deriving (Eq, Show)

-- Can we get more structured than this? Does it matter?
newtype PackageDimensions =
  PackageDimensions { unPackageDimensions :: Text }
  deriving (Eq, Show)

-- grams
data Weight =
  Weight { unWeight :: Integer }
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
  ShippingServiceName { unShippingServiceName :: Text }
  deriving (Eq, Show)

newtype CarrierName =
  CarrierName { unCarrierName :: Text }
  deriving (Eq, Show)

newtype ShippingServiceId =
  ShippingServiceId { unShippingServiceId :: Text }
  deriving (Eq, Show)

newtype ShippingServiceOfferId =
  ShippingServiceOfferId { unShippingServiceOfferId :: Text }
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
  OrderItemId { unOrderItemId :: Text }
  deriving (Eq, Show)

data Item =
       Item
         { orderItemId :: OrderItemId
         , quantity    :: Int
         }

newtype AddressName =
  AddressName { unAddressName :: Text }
  deriving (Eq, Show)

newtype AddressLine =
  AddressLine { unAddressLine :: Text }
  deriving (Eq, Show)

newtype City =
  City { unCity :: Text }
  deriving (Eq, Show)

newtype County =
  County { unCounty :: Text }
  deriving (Eq, Show)

-- or "province"
newtype State =
  State { unState :: Text }
  deriving (Eq, Show)

newtype PostalCode =
  PostalCode { unPostalCode :: Text }
  deriving (Eq, Show)

newtype PhoneNumber =
  PhoneNumber { unPhoneNumber :: Text }
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

newtype Name = Name Text

makeName :: Text -> Either Text Name
makeName name
  | length name == 0  = Left "Empty name"
  | length name >  30 = Left "Name is too long"
  | length name <= 30 = Right $ Name name

newtype AddressLine = AddressLine Text

makeAddressLine :: Text -> Either Text AddressLine
makeAddressLine line
  | length line == 0   = Left "Empty line"
  | length line >  180 = Left "line is too long"
  | length line <= 180 = Right $ AddressLine line

newtype SecondaryAddressLine = SecondaryAddressLine Text

makeSecondaryAddressLine :: Text -> Either Text SecondaryAddressLine
makeSecondaryAddressLine line
  | length line == 0  = Left "Empty line"
  | length line >  60 = Left "line is too long"
  | length line <= 60 = Right $ SecondaryAddressLine line

newtype DistrictOrCounty = DistrictOrCounty Text

makeDistrictOrCounty :: Text -> Either Text DistrictOrCounty
makeDistrictOrCounty county
  | length county == 0  = Left "Empty county"
  | length county >  30 = Left "County is too long"
  | length county <= 30 = Right $ DistrictOrCounty county

newtype Email = Email Text

makeEmail :: Text -> Either Text Email
makeEmail email
  | length email == 0   = Left "Empty email"
  | length email >  256 = Left "Email is too long"
  | length email <= 256 = Right $ Email email

newtype City = City Text

makeCity :: Text -> Either Text City
makeCity city
  | length city == 0  = Left "Empty city name"
  | length city >  30 = Left "City name is too long"
  | length city <= 30 = Right $ City city

newtype StateOrProvinceCode = StateOrProvinceCode Text

makeStateOrProvinceCode :: Text -> Either Text StateOrProvinceCode
makeStateOrProvinceCode state
  | length state == 0  = Left "Empty state or province code"
  | length state >  30 = Left "State or province code is too long"
  | length state <= 30 = Right $ StateOrProvinceCode state

newtype PostalCode = PostalCode Text

makePostalCode :: Text -> Either Text PostalCode
makePostalCode code
  | length code == 0  = Left "Empty postal code"
  | length code >  30 = Left "Postal code is too long"
  | length code <= 30 = Right $ PostalCode code

newtype PhoneNumber = PhoneNumber Text

makePhoneNumber :: Text -> Either Text PhoneNumber
makePhoneNumber number
  | length number == 0  = Left "Empty phone number"
  | length number >  30 = Left "Phone number is too long"
  | length number <= 30 = Right $ PhoneNumber number

data PackageDimensions =
       PackageDimensions
         { length :: Maybe Length
         , width  :: Maybe Width
         , height :: Maybe Height
         , unit   :: Maybe LengthUnit
         , predefinedPackageDimensions ::
             Maybe PredefinedPackageDimensions
         }

newtype Length = Length Float

makeLength :: Float -> Either Text Length
makeLength len
  | len <= 0 = Left "Length must be greater than zero"
  | len >  0 = Right $ Length len

newtype Width = Width Float

makeWidth :: Float -> Either Text Width
makeWidth width
  | width <= 0 = Left "Width must be greater than zero"
  | width >  0 = Right $ Width width

newtype Height = Height Float

makeHeight :: Float -> Either Text Height
makeHeight height
  | height <= 0 = Left "Height must be greater than zero"
  | height >  0 = Right $ Height height

data LengthUnit =
       Inches
     | Centimeters

data Weight =
       Weight
         { value :: WeightValue
         , unit  :: WeightUnit
         }

newtype WeigthValue = WeigthValue Float

makeWeigthValue :: Float -> Either Text WeigthValue
makeWeigthValue weight
  | weight <= 0 = Left "Weigth must be greater than zero"
  | weight >  0 = Right $ WeigthValue weight

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
         , amount       :: Amount
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
