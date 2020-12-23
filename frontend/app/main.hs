module Main where

import Data.Text (pack)
import Reflex.Dom
  ( mainWidget,
    el,
    text,
  )
import Types (User (..))

main :: IO ()
main = mainWidget $ do
  let user = User 1 "World!"
  el "div" $ text $ "Hello " <> pack (show user)
