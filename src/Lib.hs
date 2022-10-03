{-# LANGUAGE DeriveAnyClass #-}

module Lib
    -- ( startApp
    -- , app
    -- ) where
      where

import Relude hiding (Product)

import Control.Monad.Error.Class
import Control.Monad.Logger
import Data.Aeson
import Database.Persist.Sql
import MonadDB
import Product
import RestApi
import Servant
import Servant.API.Generic
import Servant.Auth.Server
import Servant.HTML.Blaze
import Text.Blaze
import Text.Hamlet
import Text.Julius
import Web.FormUrlEncoded

type instance IsElem' e (Auth auths v :> s) = IsElem e s

class HasJWTSettings c where
  getJWTSettings :: c -> JWTSettings
class HasCookieSettings c where
  getCookieSettings :: c -> CookieSettings

data LoginData = LoginData { username :: Text, password :: Text }
  deriving stock Generic
  deriving anyclass (FromJSON, FromForm)
data LoginToken = LoginToken { loginToken :: Text }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

type LoginRedirect = Headers '[ Header "Set-Cookie" SetCookie
                              , Header "Set-Cookie" SetCookie
                              , Header "Location" Link
                              ] NoContent

type LoginNoRedirect = Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie
                                ] NoContent

type LoginAPI =
  "login" :> ReqBody '[JSON] LoginData
          :> Post '[JSON] LoginNoRedirect
  :<|> "logout" :> Post '[JSON] LoginNoRedirect

type SecuredAPI =
  "api" :> ToServantApi (RestAPI "product" '[JSON] Product)
  :<|> "api" :> LoginAPI
  :<|> "search" :> QueryParam "searchTerm" Text :> Get '[HTML] Html

type FrontendLoginAPI =
  "login" :> (
    Get '[HTML] Html
    :<|>
    ReqBody '[FormUrlEncoded] LoginData :> Verb 'POST 302 '[HTML] LoginRedirect
  ) :<|>
  "logout" :> (
    Get '[HTML] Html
    :<|> Verb 'POST 302 '[HTML] LoginRedirect
  )

type API =
  Auth '[Cookie] LoginToken :> (
    SecuredAPI
    :<|> FrontendLoginAPI
  )

api :: Proxy API
api = Proxy

searchLink :: Link
searchLink = safeLink api (Proxy :: Proxy ("search" :> Get '[] Html))
loginLink :: Link
loginLink = safeLink api (Proxy :: Proxy ("login" :> Get '[] Html))

-- TODO Stub for actual database access
tryLoginDB :: Monad m => Text -> Text -> m (Maybe Text)
tryLoginDB "Test" "password" = pure $ Just "token"
tryLoginDB "Test" "username" = error "Can't do this"
tryLoginDB _ _               = pure Nothing

verifyToken :: Monad m => LoginToken -> m Bool
verifyToken (LoginToken "token") = pure True
verifyToken _                    = pure False

loginAPI :: forall m ctx.
            ( MonadIO m
            , MonadError ServerError m
            , MonadReader ctx m
            , HasJWTSettings ctx
            , HasCookieSettings ctx
            )
         => AuthResult LoginToken
         -> ServerT LoginAPI m
loginAPI _ = tryLogin :<|> doLogout
  where
    tryLogin (LoginData {..}) =
      -- We could force somebody to log out first, but we won't.
      tryLoginDB username password >>= \case
        Nothing -> throwError err401
        Just token -> do
          jwtSettings <- asks getJWTSettings
          cookieSettings <- asks getCookieSettings

          liftIO (acceptLogin cookieSettings jwtSettings $ LoginToken token) >>= \case
            Nothing -> throwError err401
            Just applyCookies -> return $ applyCookies NoContent

    doLogout = do
      cookieSettings <- asks getCookieSettings
      pure $ clearSession cookieSettings NoContent

frontendLoginAPI :: forall m ctx.
                    ( MonadReader ctx m
                    , _
                    )
                 => AuthResult LoginToken
                 -> ServerT FrontendLoginAPI m
frontendLoginAPI authResult = (getLogin :<|> doLogin) :<|> (getLogout :<|> doLogout)
  where
    loginPage wrongLogin = [shamlet|
      $doctype 5

      $if wrongLogin
        <p>Invalid credentials, try again.
      $else
        <p>Welcome! Please log in to use this service.

      <form action="" method="POST">
        <label for="username">Username
        <input type="text" name="username" id="username">
        <br>
        <label for="password">Password
        <input type="password" name="password" id="password">
        <br>
        <button type="submit">Login
    |]
    logoutPage = [shamlet|
      $doctype 5

      <form action="" method="POST">
        Do you want to log out?
        <button type="submit">Logout
    |]

    getLogin = do
      logInfoN $ "Login: " <> show authResult
      case authResult of
        -- TODO Check if actually still logged in?
        Authenticated _ -> throwError err302 { errHeaders = [ ("Location", show $ linkURI searchLink)] }
        _ -> pure $ loginPage False

    doLogin (LoginData {..}) = do
      logInfoN $ "Trying Login: " <> show authResult
      tryLoginDB username password >>= \case
        Nothing -> throwError err401 { errBody = mimeRender (Proxy :: Proxy HTML) $ loginPage True }
        Just token ->  do
          jwtSettings <- asks getJWTSettings
          cookieSettings <- asks getCookieSettings

          liftIO (acceptLogin cookieSettings jwtSettings $ LoginToken token) >>= \case
            Nothing -> throwError err401 { errBody = "Error creating the cookies" }
            Just applyCookies -> do
              return $ applyCookies
                     $ addHeader searchLink NoContent

    getLogout = do
      logInfoN $ "Logout: " <> show authResult
      case authResult of
        Authenticated _ -> pure logoutPage
        _ -> throwError err302 { errHeaders = [ ("Location", show $ linkURI loginLink)] }

    doLogout = do
      logInfoN $ "Trying Logout: " <> show authResult
      cookieSettings <- asks getCookieSettings
      pure $ clearSession cookieSettings
           $ addHeader loginLink NoContent

server :: forall backend m ctx.
          ( CanDB Product backend m
          , MonadLogger m
          , MonadError ServerError m
          , MonadReader ctx m
          , HasJWTSettings ctx
          , HasCookieSettings ctx
          )
       => Proxy backend
       -> ServerT API m
server bp authResult =
  (dbRestAPI bp :<|> loginAPI authResult :<|> searchApi bp authResult)
  :<|> frontendLoginAPI authResult

like :: PersistField typ
     => EntityField record typ
     -> typ
     -> Filter record
f `like` a = Filter f (FilterValue a) (BackendSpecificFilter "LIKE")

searchApi :: forall backend m.
              ( CanDB Product backend m
              , _
              )
           => Proxy backend
           -> AuthResult LoginToken
           -> Maybe Text
           -> m Html
searchApi _ authResult filterTerm = do
  logInfoN $ "Search: " <> show authResult
  products :: [Entity Product] <- runDB @backend (
    selectList
      (maybe
          []
          (\term -> [ProductName `like` ("%" <> term <> "%")]
                ||. [ProductDescription `like` ("%" <> term <> "%")])
          filterTerm
      )
      []
    )


  let script :: Text
      script = toStrict $ renderJavascriptUrl undefined [julius|
        function addItem(productId, name, description) {
          const list = document.getElementById('list');
          if(list instanceof HTMLDivElement) {
            const parent = list.parentElement;
            list.remove();
            parent.innerHTML += `<ul><li> (${productId}) - ${name}: ${description}</ul>`;
          } else {
            list.innerHTML += `<li> (${productId}) - ${name}: ${description}`;
          }
        }

        async function createNewItem(event) {
          event.preventDefault();
          const nameElem = document.getElementById('nameInput');
          const descElem = document.getElementById('descInput');

          const productName = nameElem.value;
          const productDescription = descElem.value;

          const res = await fetch('http://localhost:8080/api/product', {
            headers: {
              'Accept': 'application/json',
              'Content-Type': 'application/json'
            },
            body: JSON.stringify({productName, productDescription }),
            method: 'POST'
          });

          if(res.ok) {
            const {id, productName, productDescription} = await res.json();
            addItem(id, productName, productDescription);
          } else { alert('An error has occurred: ' + res.statusText); }

          nameElem.value = '';
          descElem.value = '';
          nameElem.focus();
        }

        document.addEventListener('DOMContentLoaded', () => {
          document.getElementById('addProduct').addEventListener('submit', createNewItem);
        });
      |]

  pure [shamlet|
    $doctype 5
    <html>
        <head>
            <title>Search Interface
            <script>
              #{preEscapedToMarkup script}
        <body>
            <h1>Add a new Product
            <form method="POST" #addProduct action="">
                <label for="nameInput">Product name:
                <input name="nameInput" #nameInput type="text">
                <br>
                <label for="descInput">Product description:
                <input name="descInput" #descInput type="text">
                <br>
                <button type="submit">Add Product

            <h1>Search for Products
            <form method="GET" action="">
                <label for="searchTerm">Search Term:
                <input name="searchTerm" #searchTerm type="text" value="#{fromMaybe "" filterTerm}">
                <button type="submit">Search

            <h1>Product List
            $if null products
                <div #list>
                    <strong>No products found.
            $else
                <ul #list>
                    $forall Entity key (Product {..}) <- products
                        <li>(#{fromSqlKey key}) - #{productName}: #{productDescription}
  |]
