~~~haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Product 
    ( module Product
    ) where
~~~

This module defines an example Product type as a database entity.

~~~haskell
import Relude hiding (Product)

import Data.Aeson
import Data.Aeson.TH
import Database.Persist as DB
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Product
  name Text
  description Text

  deriving Show Eq
|]
~~~

We use default settings for the derivations here -- this produces
definitions akin to the following:

~~~haskell ignore
-- The basic Product type is created as-is in Haskell
data Product =
  Product { name :: !Text
          , description :: !Text }

-- Any entity is an instance of this type class. 
-- It defines a few associated types
instance PersistEntity Product where
  -- The datatype representing the implicitly generated Key
  newtype instance Key Product
    = ProductKey {unProductKey :: BackendKey SqlBackend}
  
  -- Which backend this datatype is compatible with.
  -- Because we used `sqlSettings`, this is `SqlBackend`,
  -- but it is possible to generate a generic version of `Product`
  -- instead that is indexed by the backend so it's usable
  -- across different backends.
  type instance PersistEntityBackend Product = SqlBackend
  
  -- Entity fields for use in queries, filters etc.
  data instance EntityField Product typ where 
    ProductId          :: EntityField Product ProductId
    ProductName        :: EntityField Product Text
    ProductDescription :: EntityField Product Text
  
  -- If any additional uniqueness constraints are defined,
  -- this datatype keeps ahold of them.
  data instance Unique Product

instance Eq              Product
instance PersistField    Product
instance PersistFieldSql Product
instance Show            Product
instance ToBackendKey    SqlBackend Product

-- This is for use with the OverloadedLabels extension
-- which we do not make use of.
-- It'd allow definitions like `#name :: EntityField Product Text`,
-- and in general to offer a different field access schema
-- but also to generalize across multiple entities
-- with a common field name and type via a
-- `SymbolToField "name" res Text` constraint.
instance SymbolToField "id"          Product (Key Product)
instance SymbolToField "name"        Product Text
instance SymbolToField "description" Product Text

instance Eq              (Key Product)
instance Ord             (Key Product)
instance PersistField    (Key Product)
instance PersistFieldSql (Key Product)
instance Read            (Key Product)
instance Show            (Key Product)
~~~

All of the above is implicitly generated from the simple QuasiQuote above!

~~~haskell
$(deriveJSON defaultOptions ''Product)

instance ToJSON (Entity Product) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Product) where
  parseJSON = entityIdFromJSON
~~~

Last but not least we derive JSON-instances for both
the basic Product type and the `Entity` constructor that
bundles the Product with its ID.

The `entityId*JSON` functions we use here provide for the
following JSON structure:

~~~json
{
  "id": 20,
  "name": "...",
  "description": "..."
}
~~~
