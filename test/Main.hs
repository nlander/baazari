module Main where

import Bazaari.Types
import Bazaari.Http
import Data.Monoid
import Data.CountryCodes
import Data.ByteString
import System.Environment
import Data.Time
import Test.Hspec
import Text.Email.Validate
import Network.HTTP.Types.URI

sample_ShipmentRequestDetails :: ShipmentRequestDetails
sample_ShipmentRequestDetails =
  ShipmentRequestDetails
    { amazonOrderId = AmazonOrderId "123"
    , sellerOrderId = Nothing
    , itemList =
      [ Item { orderItemId = OrderItemId "456", quantity = 2 }
      , Item { orderItemId = OrderItemId "789", quantity = 1 } ]
    , shipFromAddress =
        Address
          { name = AddressName "Leslie Generic"
          , addressLine1 = AddressLine "123 Anywhere Dr"
          , addressLine2 = Nothing
          , addressLine3 = Nothing
          , districtOrCounty = Nothing
          , email = unsafeEmailAddress "leslie_generic_1234" "somesite.com"
          , city = City "Somewheresville"
          , stateOrProvinceCode = Just $ State "FL"
          , postalCode = PostalCode "33133"
          , countryCode = US
          , phone = PhoneNumber "123-456-7890" }
    , packageDimensions = PredefinedDimensions FedEx_Tube
    , weight = 
        Weight
          { value = WeightValue 30.5
          , units = Ounces }
    , mustArriveByUTCTime =
        parseTimeM True defaultTimeLocale
          "%D %R"
          "08/15/16 12:30"
    , requestShipUTCTime = Nothing
    , requestShippingServiceOptions =
        ShippingServiceOptions
          { deliveryExperience = 
              DeliveryConfirmationWithAdultSignature
          , declaredValue = Just $
              CurrencyAmount
                { currencyCode = USD
                , amount = 44.99 }
          , carrierWillPickUp = True } }

sample_QueryString :: IO (ByteString, UTCTime)
sample_QueryString = do
  now <- getCurrentTime
  sellerId <- envBS "MWS_DEV_SELLER_ID"
  accessKeyId <- envBS "MWS_DEV_ACCESS_KEY_ID"
  return ( "POST\nmws.amazonservices.com\n/\nAWSAccessKeyId="
        <> accessKeyId
        <> "&Action=GetEligibleShippingServices&SellerId="
        <> sellerId
        <> "&ShipmentRequestDetails.AmazonOrderId=123&ShipmentRequestDetails.Item.1.OrderItemId=456&ShipmentRequestDetails.Item.1.Quantity=2&ShipmentRequestDetails.Item.2.OrderItemId=789&ShipmentRequestDetails.Item.2.Quantity=1&ShipmentRequestDetails.MustArriveByUTCTime=2016-08-15T12%3A30%3A00.00Z&ShipmentRequestDetails.PackageDimensions.PredefinedPackageDimensions=FedEx_Tube&ShipmentRequestDetails.RequestShippingServiceOptions.CarrierWillPickUp=true&ShipmentRequestDetails.RequestShippingServiceOptions.DeclaredValue.Amount=44.99&ShipmentRequestDetails.RequestShippingServiceOptions.DeclaredValue.CurrencyCode=USD&ShipmentRequestDetails.RequestShippingServiceOptions.DeliveryExperience=DeliveryConfirmationWithAdultSignature&ShipmentRequestDetails.ShipFromAddress.AddressLine1=123%20Anywhere%20Dr&ShipmentRequestDetails.ShipFromAddress.City=Somewheresville&ShipmentRequestDetails.ShipFromAddress.CountryCode=US&ShipmentRequestDetails.ShipFromAddress.Email=leslie_generic_1234%40somesite.com&ShipmentRequestDetails.ShipFromAddress.Name=Leslie%20Generic&ShipmentRequestDetails.ShipFromAddress.Phone=123-456-7890&ShipmentRequestDetails.ShipFromAddress.PostalCode=33133&ShipmentRequestDetails.ShipFromAddress.StateOrProvinceCode=FL&ShipmentRequestDetails.Weight.Unit=ounces&ShipmentRequestDetails.Weight.Value=30.5&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp="
        <> ( urlEncode True . renderUTCTime $ now )
        <>  "&Version=2015-06-01", now)

envBS :: String -> IO ByteString
envBS envVar = renderEnvironmentVariable <$> getEnv envVar

main :: IO ()
main = hspec $ do
  describe "Unsigned Query" $ do
    it "Test ShipmentRequestDetails should produce a proper unsigned query string." $ do
      (str, now) <- sample_QueryString
      sellerId <- envBS "MWS_DEV_SELLER_ID"
      accessKeyId <- envBS "MWS_DEV_ACCESS_KEY_ID"
      getEligibleShippingServicesUnsigned NorthAmerica
        (getEligibleShippingServicesUnsignedParams
          sellerId accessKeyId sample_ShipmentRequestDetails now)
        `shouldBe` str
