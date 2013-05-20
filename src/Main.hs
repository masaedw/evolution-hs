module Main (main) where

import Control.Monad.Random (RandT, evalRandT)
import Control.Monad.Trans.Maybe
import Data.List
import Evolution
import Evolution.Imports

main :: IO ()
main = do
  let world = initWorld 100 30
  gen <- getStdGen
  evalRandT (loop world) gen
  where
    loop :: World -> RandT StdGen IO ()
    loop world = void . runMaybeT . foldM_ (flip id) world $ repeat mainstep

    mainstep :: World -> MaybeT (RandT StdGen IO) World
    mainstep world = do
      nw <- lift $ step world
      liftIO . putStr $ showWorld nw
      liftIO . putStrLn $ replicate (width world) '-'
      line <- liftIO getLine
      guard . not $ "q" `isPrefixOf` line
      return nw
