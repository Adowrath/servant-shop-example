~~~haskell
module RestApi
    ( RestAPI(..)
    , dbRestAPI
    ) where 
~~~

This module defines a common, re-usable Rest-API datatype and an implementation
based on `persistent` that automatically handles the correct status codes etc.

~~~haskell
import Relude

import MonadDB
import Control.Monad.Error.Class
import Data.Map.Strict as M
import Database.Persist as DB
import GHC.TypeLits (Symbol)
import Servant
import Servant.Server.Generic
import Servant.API.Generic

type RestAPI :: Symbol -> [Type] -> Type -> (Type -> Type)
data RestAPI name encs res route = RestAPI
  { _getAll    :: route :- name :> Get encs [Entity res]
  , _addNew    :: route :- name :>                           ReqBody encs res :> Verb 'POST 201 encs (Entity res)
  , _getSingle :: route :- name :> Capture "id" (Key res)                     :> Get encs (Entity res)
  , _replace   :: route :- name :> Capture "id" (Key res) :> ReqBody encs res :> Put encs (Entity res)
  , _delete    :: route :- name :> Capture "id" (Key res)                     :> Verb 'DELETE 204 encs NoContent
  } deriving (Generic)
~~~

A REST-API is defined by 3 things, represented by the first 3 parameters to the datatype: 

- `name` stands for the `/route` you want this resource be served under
- `encs` stands for a list of possible content-types for your datatype, like `'[JSON]`.
  Currently, the available content types for the responses and POST/PUT and in the future
  PATCH are equal, though this restriction might be lifted (mostly makes sense for PATCH though.)
- `res`, finally, defines the type of your resource that this Rest-API should handle.

~~~haskell
dbRestAPI :: forall name encs res m backend.
             ( CanDB res backend m
             , MonadError ServerError m
             )
          => Proxy backend
          -> ToServant (RestAPI name encs res) (AsServerT m)
~~~

This may appear a bit daunting at first, but it just restricts the underlying monad `m` to one
that can handle database calls for your specific `resource`.
The `Proxy backend` argument is sadly needed, because I'd rather do this than turn on `AllowAmbiguousTypes` right now.

~~~haskell
dbRestAPI _ = toServant api
  where
    api :: RestAPI _ _ _ (AsServerT m)
    api = RestAPI { .. }

    _getAll :: m [Entity res]
    _getAll = runDB @backend $ DB.selectList [] []
~~~

The `_getAll` handler simply queries every entry from the database -- no filter, no sorting, no pagination, nothing.

~~~haskell
    _addNew :: res -> m (Entity res)
    _addNew res = do
        idx <- runDB @backend (DB.insert res)
        pure $ Entity idx res
~~~

`_addNew` adds a new resource to the database and returns a combined `Entity` record consisting
of the key and value for this resource.

~~~haskell
    _getSingle :: Key res -> m (Entity res)
    _getSingle idx =
        Entity idx <$> whenNothingM (runDB @backend (DB.get idx)) (throwError err404)
~~~

`_getSingle` tries to query for the resource under the given key and errors out with a 404 page
if no result was found.

~~~haskell
    _replace :: Key res -> res -> m (Entity res)
    _replace idx res = do
        runDB @backend (DB.repsert idx res)
        pure $ Entity idx res
~~~

Currently, `_replace` can create new documents at arbitrary ID-values.
I'd normally use `update` for this but that doesn't return a count of changed
rows, so I will need to adapt it with a pre-emptive `count` to determine if the
resource exists.

~~~haskell
    _delete :: Key res -> m NoContent
    _delete key = do
        runDB @backend $ DB.delete key
        pure NoContent
~~~

Same caveat as with `_replace`, `_delete` removes the resource at the given key.
