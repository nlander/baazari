{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Time.ISO8601
       ( UTCTime )

data ShipmentRequestDetails =
       ShipmentRequestDetails
         { amazonOrderId           :: AmazonOrderId
         , sellerOrderId           :: Maybe SellerOrderId
         , itemList                :: [Item]
         , shipFromAddress         :: Address
         , packageDimensions       :: PackageDimensions
         , weight                  :: Weight
         , mustArriveByDate        :: Maybe UTCTime
         , shipDate                :: Maybe UTCTime
         , shippingServieceOptions :: ShippingServiceOptions }

newtype AmazonOrderId = AmazonOrderId Text

makeAmazonOrderId :: Text -> Either Text AmazonOrderId
makeAmazonOrderId id
  | length id == 0  = Left "Empty order id"
  | length id >  50 = Left "Order id is too long"
  | length id <= 50 = Right $ AmazonOrderId id

newtype SellerOrderId = SellerOrderId Text

makeSellerOrderId :: Text -> Either Text SellerOrderId
makeSellerOrderId id
  | length id == 0  = Left "Empty order id"
  | length id >  64 = Left "Order id is too long"
  | length id <= 64 = Right $ SellerOrderId id

data ShippingService =
       ShippingService
         { shippingServiceName           :: ShippingServiceName
         , carrierName                   :: CarrierName
         , shippingServiceId             :: ShippingServiceId
         , shippingServiceOfferId        :: ShippingServiceOfferId
         , shipDate                      :: UTCTime
         , earliestEstimatedDeliveryDate :: Maybe UTCTime
         , latestEstimatedDeliveryDate   :: Maybe UTCTime
         , rate                          :: CurrencyAmount
         , shippingServiceOptions        :: ShippingServiceOptions
         }

type ShippingServiceName    = Text
type CarrierName            = Text
type ShippingServiceId      = Text
type ShippingServiceOfferId = Text

newtype TemporarilyUnavailableCarrier =
          TemporarilyUnavailableCarrier CarrierName

newtype TermsAndConditionsNotAcceptedCarrier =
          TermsAndConditionsNotAcceptedCarrier CarrierName

data Item =
       Item
         { orderItemId :: OrderItemId
         , quantity    :: Int
         }

data Address =
       Address
         { name :: Name
         , addressLine1        :: AddressLine
         , addressLine2        :: Maybe AddressLine
         , addressLine3        :: Maybe AddressLine
         , districtOrCounty    :: Maybe DistrictOrCounty
         , email               :: Email
         , city                :: City
         , stateOrProvinceCode :: Maybe StateOrProvinceCode
         , postalCode          :: PostalCode
         , countryCode         :: CountryCode
         , phone               :: PhoneNumber
         }

data PackageDimensions =
       PackageDimensions
         { length :: Maybe Length
         , width  :: Maybe Width
         , height :: Maybe Height
         , unit   :: Maybe Unit
         , predefinedPackageDimensions ::
             Maybe PredefinedPackageDimensions
         }

data Weight =
       Weight
         { value :: Value
         , unit  :: Unit
         }

data ShippingServiceOptions =
       ShippingServiceOptions
         { deliveryExperience :: DeliveryExperience
         , declaredValue      :: Maybe CurrencyAmount
         , carrierWillPickUp  :: Bool
         }

data CurrencyAmount =
       CurrencyAmount
         { currencyCode :: CurrencyCode
         , amount       :: Amount
         }

data DeliveryExperience =
         DeliveryConfirmationWithAdultSignature
       | DeliveryConfirmationWithSignature
       | DeliveryConfirmationWithoutSignature
       | NoTracking
