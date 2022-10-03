{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Relude hiding (Product)

import Control.Concurrent (forkIO)
import Control.Monad.Logger
import Lib
import Data.Pool (Pool)
import Database.Persist.MySQL
import MonadDB
import Network.Wai.Handler.Warp
import qualified Product as P
import Servant.Auth.Server
import Servant.Server

migrateAll :: Migration
migrateAll = do
    P.migrateAll

data Config backend = Config
  { pool           :: Pool backend
  , jwtSettings    :: JWTSettings
  , cookieSettings :: CookieSettings
  }

instance BackendCompatible sup sub => HasCompatiblePool sup sub (Config sub) where
    getBackend _ (Config {..}) = pool

instance HasJWTSettings (Config b) where
    getJWTSettings = jwtSettings

instance HasCookieSettings (Config b) where
    getCookieSettings = cookieSettings

main :: IO ()
main = do
    pool <- runStderrLoggingT
            $ createMySQLPool (defaultConnectInfo { connectHost = "database", connectPassword = "secret" }) 10

    jwtKey <- generateKey

    flip runReaderT pool $ runDB (runMigration migrateAll)

    let jwtSettings :: _
        jwtSettings = defaultJWTSettings jwtKey

        cookieSettings :: CookieSettings
        cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

        config :: Config _
        config = Config { .. }

        readerToHandler :: forall a.
                           _
                        -> Handler a
        readerToHandler = Handler . runStderrLoggingT . (`runReaderT` config)

        ctx :: _
        ctx = getCookieSettings config :. getJWTSettings config :. EmptyContext

        application =
            serveWithContextT
                api
                ctx
                readerToHandler
                (server (Proxy :: Proxy SqlBackend))

    runStderrLoggingT $ logWarnN "Starting web application on localhost:8080"

    --void $ forkIO $
    run 8080 application

    runStderrLoggingT $ logWarnN "Started!"

    -- forever $ do
    --     x <- getLine
    --     putTextLn x
