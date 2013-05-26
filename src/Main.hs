module Main (main) where

import Control.Monad.Random (MonadRandom, RandT, evalRandT)
import Control.Monad.Trans.Maybe
import Data.List
import Evolution
import Evolution.Imports

main :: IO ()
main = do
  gen <- getStdGen
  evalRandT (initWorld 100 30 >>= loop) gen

loop :: (RandomGen g) => World -> RandT g IO ()
loop world = void . runMaybeT . (flip evalStateT) 1 . foldM_ (flip id) world $ repeat mainstep

mainstep :: (RandomGen g) => World -> StateT Int (MaybeT (RandT g IO)) World
mainstep world = do
  n <- get
  nw <- lift . lift $ nstep n world
  liftIO . putStr $ showWorld nw
  liftIO . putStrLn $ replicate (width world) '-'
  line <- liftIO getLine
  put . fromMaybe n $ parseInt line
  guard . not $ "q" `isPrefixOf` line
  return nw

parseInt :: String -> Maybe Int
parseInt s =
    case reads s of
      [] -> Nothing
      ((i,_):_) -> Just i

nstep :: (Applicative m, MonadRandom m) => Int -> World -> m World
nstep 1 w = step w
nstep n w = step w >>= nstep (n - 1)
