module Baazari.Types where

data ShipmentRequestDetails =
       ShipmentRequestDetails
         { amazonOrderId           :: AmazonOrderId
         , sellerOrderId           :: Maybe SellerOrderId
         , itemList                :: [Item]
         , shipFromAddress         :: Address
         , packageDimensions       :: PackageDimensions
         , weight                  :: Weight
         , mustArriveByDate        :: Maybe Date
         , shipDate                :: Maybe Date
         , shippingServieceOptions :: ShippingServiceOptions }

data ShippingService =
       ShippingService
         { shippingServiceName           :: ShippingServiceName
         , carrierName                   :: CarrierName
         , shippingServiceId             :: ShippingServiceId
         , shippingServiceOfferId        :: ShippingServiceOfferId
         , shipDate                      :: Date
         , earliestEstimatedDeliveryDate :: Maybe Date
         , latestEstimatedDeliveryDate   :: Maybe Date
         , rate                          :: CurrencyAmount
         , shippingServiceOptions        :: ShippingServiceOptions
         }

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
