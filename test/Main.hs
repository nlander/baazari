module Main where

import Bazaari.Types
import Bazaari.Http
import Data.CountryCodes
import Data.ByteString
import System.Environment
import Data.Time
import Test.Hspec
import Text.Email.Validate

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
          "07/25/16 12:30"
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
  sellerId <- getEnv "MWS_DEV_SELLER_ID"
  accessKeyId <- getEnv "MWS_DEV_ACCESS_KEY_ID"
  return ("POST\nmws.amazonservices.com\n/\n", now)

envBS :: String -> IO ByteString
envBS envVar = renderEnvironmentVariable <$> getEnv envVar

main :: IO ()
main = hspec $ do
  describe "Unsigned Query" $ do
    it "Test ShippingRequestDetails should produce a proper unsigned query string." $ do
      (str, now) <- sample_QueryString
      sellerId <- envBS "MWS_DEV_SELLER_ID"
      accessKeyId <- envBS "MWS_DEV_ACCESS_KEY_ID"
      getEligibleShippingServicesUnsigned NorthAmerica
        (getEligibleShippingServicesUnsignedParams
          sellerId accessKeyId sample_ShipmentRequestDetails now)
        `shouldBe` str
