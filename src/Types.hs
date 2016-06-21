module Types where

data ShipmentRequestDetails =
       ShipmentRequestDetails
         { amazonOrderId           :: AmazonOrderId
         , sellerOrderId           :: Maybe SellerOrderId
         , itemList                :: [List]
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
