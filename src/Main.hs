module Main (main) where

import Data.List
import Evolution
import Evolution.Imports
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
  let world = initWorld 20 10 ((5,3), (15,6))
  gen <- getStdGen
  evalStateT (loop world) gen
  where
    loop :: World -> StateT StdGen IO ()
    loop world = void . runMaybeT . foldM_ (flip id) world $ repeat mainstep

    mainstep :: World -> MaybeT (StateT StdGen IO) World
    mainstep world = do
      nw <- lift $ step world
      liftIO . putStr $ showWorld nw
      liftIO . putStrLn $ replicate (width world) '-'
      line <- liftIO getLine
      guard . not $ "q" `isPrefixOf` line
      return nw
