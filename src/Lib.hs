module Lib
    -- ( startApp
    -- , app
    -- ) where
      where

import Relude hiding (Product)

import qualified Data.Map.Lazy as M
import Data.Text (isInfixOf)
import Network.Wai
import Network.Wai.Handler.Warp
import Product
import RestApi
import Servant
import Servant.API.Generic
import Servant.HTML.Blaze
import Text.Blaze
import Text.Hamlet
import Text.Julius

type API =
  "api" :> ToServantApi (RestAPI "product" '[JSON] Product)
  :<|> "search" :> QueryParam "searchTerm" Text :> Get '[HTML] Html

startApp :: IORef (Int, Map Int Product)
         -> IO ()
startApp = run 8080 . app

app :: IORef (Int, Map Int Product)
    -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: IORef (Int, Map Int Product)
       -> Server API
server ref = ioRefRestAPI ref :<|> searchApi ref

searchApi :: IORef (Int, Map Int Product)
          -> Maybe Text
          -> Handler Html
searchApi ref filterTerm = readIORef ref <&> (M.elems . snd) >>= \products -> do
  let infixFilter :: Text -> Product -> Bool
      term `infixFilter` p = term `isInfixOf` name p || term `isInfixOf` description p

      searchFilter :: [Product] -> [Product]
      searchFilter = maybe id (filter . infixFilter) $ mfilter (/= "") filterTerm

      filteredProducts :: [Product]
      filteredProducts = searchFilter products

      script :: Text
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

          const name = nameElem.value;
          const description = descElem.value;

          const res = await fetch('http://localhost:8080/api/product', {
            headers: {
              'Accept': 'application/json',
              'Content-Type': 'application/json'
            },
            body: JSON.stringify({productId: -1, name, description }),
            method: 'POST'
          });

          if(res.ok) {
            const {productId, name, description} = await res.json();
            addItem(productId, name, description);
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
                    $forall Product {..} <- filteredProducts
                        <li>(#{productId}) - #{name}: #{description}
  |]
