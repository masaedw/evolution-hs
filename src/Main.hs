module Main (main) where

import Data.List
import Evolution
import Evolution.Imports

main :: IO ()
main = do
  let world = initWorld 100 30
  gen <- getStdGen
  void $ runStateT (loop world) gen
  where
    loop :: World -> StateT StdGen IO World
    loop world = do
      nw <- step world
      liftIO . putStr $ showWorld nw
      liftIO . putStrLn $ "--------------"
      line <- liftIO getLine
      if "q" `isPrefixOf` line
        then return nw
        else loop nw
