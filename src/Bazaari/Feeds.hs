{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bazaari.Feeds where

import Text.XML
import Text.Hamlet.XML
import Data.Map

productFeed = [xml|
<AmazonEnvelope xmlns:xsi=http://www.w3.org/2001/XMLSchema-instance xsi:noNamespaceSchemaLocation=amzn-envelope.xsd>
    <Header>
        <DocumentVersion>1.01
        <MerchantIdentifier>M_EXAMPLE_123456
|]
