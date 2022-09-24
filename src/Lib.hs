module Lib
    -- ( startApp
    -- , app
    -- ) where
      where

import Relude hiding (Product)

import qualified Data.Map.Lazy as M
import Data.Text (isInfixOf)
import Network.HTTP.Media ((//))
import Network.Wai
import Network.Wai.Handler.Warp
import Product
import RestApi
import Servant
import Servant.API.Generic

data RawHTML

instance Accept RawHTML where
  contentType _ = "text" // "html"
instance MimeRender RawHTML Text where
  mimeRender _ text = encodeUtf8 text

type API =
  "api" :> ToServantApi (RestAPI "product" '[JSON] Product)
  :<|> "search" :> QueryParam "searchTerm" Text :> Get '[RawHTML] Text

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
          -> Handler Text
searchApi ref filterTerm = readIORef ref <&> (M.elems . snd) >>= \products -> do
  let infixFilter :: Text -> Product -> Bool
      term `infixFilter` p = term `isInfixOf` name p || term `isInfixOf` description p

      searchFilter :: [Product] -> [Product]
      searchFilter = maybe id (filter . infixFilter) filterTerm

      productToLi :: Product -> Text
      productToLi (Product {..}) = mconcat [ "<li> (", show productId, ") - ", name, ": ", description ]

      productLines :: [Text]
      productLines = map productToLi $ searchFilter products

      productBlock :: [Text]
      productBlock =
        if null productLines
        then [ "<div id=\"list\"><strong>No products found.</strong></div>" ]
        else "<ul id=\"list\">" : productLines <> [ "</ul>" ]

  pure $ unlines
    (
      [ "<!DOCTYPE html>"
      , "<html><head><title>Search Interface</title></head><body>"
      , "<script>"
      , "function addItem(productId, name, description) {"
      , "  const list = document.getElementById('list');"
      , "  if(list instanceof HTMLDivElement) {"
      , "    const parent = list.parentElement;"
      , "    list.remove();"
      , "    parent.innerHTML += `<ul><li> (${productId}) - ${name}: ${description}</ul>`;"
      , "  } else {"
      , "    list.innerHTML += `<li> (${productId}) - ${name}: ${description}`;"
      , "  }"
      , "}"

      , "async function createNewItem(event) {"
      , "  event.preventDefault();"
      , "  const nameElem = document.getElementById('nameInput');"
      , "  const descElem = document.getElementById('descInput');"

      , "  const name = nameElem.value;"
      , "  const description = descElem.value;"

      , "  const res = await fetch('http://localhost:8080/api/product', {"
      , "    headers: {"
      , "      'accept': 'application/json',"
      , "      'Content-Type': 'application/json'"
      , "    },"
      , "    body: JSON.stringify({productId: -1, name, description }),"
      , "    method: 'POST'"
      , "  });"

      , "  if(res.ok) {"
      , "    const {productId, name, description} = await res.json();"
      , "    addItem(productId, name, description);"
      , "  } else { alert('An error has occurred: ' + res.statusText); }"

      , "  nameElem.value = '';"
      , "  descElem.value = '';"
      , "}"
      , "document.addEventListener('DOMContentLoaded', () => {"
      , "  document.getElementById('addProduct').addEventListener('submit', createNewItem);"
      , "});"
      , "</script>"
      , "<h1>Add new product</h1>"
      , "<form method=\"POST\" id=\"addProduct\" action=\"\">"
      , "    <label for=\"nameInput\">Product name:</label>"
      , "    <input name=\"nameInput\" id=\"nameInput\" type=\"text\">"
      , "    <br>"
      , "    <label for=\"descInput\">Product description:</label>"
      , "    <input name=\"descInput\" id=\"descInput\" type=\"text\">"
      , "    <br>"
      , "    <button type=\"submit\">Add Product</button>"
      , "</form>"
      , "<h1>Search for product</h1>"
      , "<form method=\"GET\" action=\"\">"
      , "    <label for=\"searchTerm\">Search Term:</label>"
      , "    <input name=\"searchTerm\" id=\"searchTerm\" type=\"text\">"
      , "    <button type=\"submit\">Search</button>"
      , "</form>"
      , "<h1>Product list</h1>"
      ] <> productBlock <>
      [ "</body></html>"
      ]
    )

