module Types (User (..)) where

import Data.Text
  ( Text,
    unpack,
  )

-- | A User with an ID and a name
data User = User
  { id :: Int,
    name :: Text
  }

instance Show User where
  show (User id' name') = "[" <> show id' <> "] " <> unpack name'
