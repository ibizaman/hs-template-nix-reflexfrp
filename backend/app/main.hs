module Main
  ( main,
  )
where

import Types (User (..))
import qualified Utils as U

main :: IO ()
main =
  let user = User 1 "World"
   in if U.equal True True
        then putStrLn ("Hello " <> (show user))
        else putStrLn "Bye"
