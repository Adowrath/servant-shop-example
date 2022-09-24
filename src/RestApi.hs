module RestApi
    ( RestAPI(..)
    , HasId(..)
    , ioRefRestAPI
    ) where 

import Relude

import Data.Map.Strict as M
import GHC.TypeLits (Symbol)
import Servant
import Servant.Server.Generic
import Servant.API.Generic

type RestAPI :: Symbol -> [Type] -> Type -> (Type -> Type)
data RestAPI name encs res route = RestAPI
  { _getAll    :: route :- name :> Get encs [res]
  , _addNew    :: route :- name :>                          ReqBody encs res :> Verb 'POST 201 encs res
  , _getSingle :: route :- name :> Capture "id" (Id res)                     :> Get encs res
  , _replace   :: route :- name :> Capture "id" (Id res) :> ReqBody encs res :> Put encs res
  , _delete    :: route :- name :> Capture "id" (Id res)                     :> Verb 'DELETE 204 encs NoContent
  } deriving (Generic)

-- deriving instance Generic (RestAPI encs res route)

class HasId a where
    type Id a :: Type

    withId :: a -> Id a -> a
    getId :: a -> Id a

ioRefRestAPI :: forall sym encs res {id}.
                (HasId res, id ~ Id res, Ord id, Num id)
             => IORef (Id res, Map (Id res) res)
             -> ToServant (RestAPI sym encs res) AsServer
ioRefRestAPI ref = toServant api
  where
    api :: RestAPI _ _ _ AsServer
    api = RestAPI { .. }

    _getAll :: Handler [res]
    _getAll = elems . snd <$> readIORef ref
    
    _addNew :: res -> Handler res
    _addNew p = atomicModifyIORef' ref \(nextIdx, m) ->
      let p' = p `withId` nextIdx
          m' = insert nextIdx p' m
      in ((nextIdx + 1, m'), p')
    
    _getSingle :: Id res -> Handler res
    _getSingle idx = do
      m <- snd <$> readIORef ref

      whenNothing (M.lookup idx m) $ throwError err404
    
    _replace :: Id res -> res -> Handler res
    _replace idx p = do
      let p' = p `withId` idx

      join $ atomicModifyIORef' ref \(nextIdx, m) ->
        let replacer :: Maybe res -> Maybe (Maybe res)
            replacer Nothing  = Nothing -- Error case, no new map.
            replacer (Just _) = Just (Just p')
        
        in case alterF replacer idx m of
          Nothing -> ((nextIdx, m),  throwError err404) -- No new map means we didn't find the res, so we 404
          Just m' -> ((nextIdx, m'), pure p')          -- We successfully replaced the res.

    _delete :: Id res -> Handler NoContent
    _delete idx = join $ atomicModifyIORef' ref \(nextIdx, m) ->
      let remover :: Maybe res -> Maybe (Maybe res)
          remover Nothing  = Nothing -- Error case, nothing to delete.
          remover (Just _) = Just (Nothing)

      in case alterF remover idx m of
        Nothing -> ((nextIdx, m),  throwError err404)
        Just m' -> ((nextIdx, m'), pure NoContent)
