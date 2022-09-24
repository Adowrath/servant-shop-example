module Main (main) where

import Relude hiding (Product)

import Data.Map.Strict as M
import Lib
import Product

main :: IO ()
main = newIORef knownProducts >>= startApp

knownProducts :: (Int, Map Int Product)
knownProducts = (5, M.fromList items)
  where items =
            [ (0, Product 0 "PS4" "The PS4 is a console from Sony Entertainment.")
            , (1, Product 1 "PS5" "The PS5 is also a console from Sony Entertainment.")
            , (2, Product 2 "PS Vita" "The PS Vita is another console from Sony Entertainment.")
            , (3, Product 3 "Nintendo Switch" "The Nintendo Switch is not a console from Sony Entertainment.")
            , (4, Product 4 "Xbox Series X" "The XSX is a console from Microsoft.")
            ]
