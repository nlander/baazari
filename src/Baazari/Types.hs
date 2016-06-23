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
