module Product 
    ( Product(..)
    ) where

import Relude hiding (Product)

import Data.Aeson.TH
import RestApi

data Product = Product
  { productId   :: Int
  , name        :: Text
  , description :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Product)

instance HasId Product where
  type Id Product = Int

  p `withId` idx = p { productId = idx }
  getId = productId
