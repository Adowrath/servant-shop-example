~~~haskell
{-# LANGUAGE FlexibleInstances #-}

module MonadDB
    ( MonadDB(..)
    , HasCompatiblePool(..)
    , CanDB
    , CanDBs
    ) where
~~~

This file defines a `MonadDB` typeclass that provides a function `runDB`, which can be used
to embed database-accessing functions (which normally require `MonadUnliftIO` on the underlying
base monad, which is not provided by `servant`'s `Handler` type) in a monad that will later be run.

~~~haskell
import Relude
 
import Data.Pool (Pool)
import Database.Persist as DB
import Database.Persist.Sql as DB

class MonadIO m => MonadDB backend m where
    runDB :: ReaderT backend IO a
           -> m a
~~~

Note that currently, the `ReaderT`-stack is restricted to an underlying `IO` monad.
I have not yet seen a need to work around that restriction as it might well complicate
the implementation of `runDB` -- the underlying monad `n` needs to be convertible to `m`,
after all.

~~~haskell
instance (HasCompatiblePool SqlBackend backend cfg, MonadIO m) => MonadDB backend (ReaderT cfg m) where
    runDB f = do
        pool <- getBackend (Proxy :: Proxy SqlBackend) <$> ask
        liftIO $ runSqlPool f pool
~~~

One instance is provided, via a `ReaderT`-stack that has access to a compatible `Pool` for
any possible `backend`.

~~~haskell
class BackendCompatible sup sub => HasCompatiblePool sup sub a where
    getBackend :: Proxy sup -> a -> Pool sub

instance BackendCompatible sup sub => HasCompatiblePool sup sub (Pool sub) where
    getBackend _ = id
~~~

The degenerate case is for a `ReaderT` whose environment is just said `Pool` itself.
Other cases -- for example, if the `Pool` is part of a bigger `Config`-type, can
implement this `Has`-like typeclass.

~~~haskell
type CanDB res backend m = 
    ( MonadDB backend m
    , PersistRecordBackend res backend
    , PersistQuery backend
    )

type CanDBs :: [Type] -> Type -> (Type -> Type) -> Constraint
type family CanDBs res backend m where
    CanDBs '[] _ _ = ()
    CanDBs (r ': rs) backend m = (CanDB r backend m, CanDBs rs backend m)
~~~

The above are some convenience type aliases that make restricting the available monad type easier.
