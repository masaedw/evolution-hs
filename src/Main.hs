module Main (main) where

import Control.Monad.Random (MonadRandom, RandT, evalRandT)
import Control.Monad.Trans.Maybe
import Data.List
import Evolution
import Evolution.Imports

main :: IO ()
main = initWorld 100 30 >>= loop

loop :: World -> IO ()
loop world = void . runMaybeT . foldM_ (flip id) (1, world) $ repeat mainstep

mainstep :: (Int, World) -> MaybeT IO (Int, World)
mainstep (n, world) = do
  nw <- lift $ nstep n world
  liftIO . putStr $ showWorld nw
  liftIO . putStrLn $ replicate (width world) '-'
  line <- liftIO getLine
  let nn = fromMaybe n $ parseInt line
  guard . not $ "q" `isPrefixOf` line
  return (nn, nw)

parseInt :: String -> Maybe Int
parseInt s =
    case reads s of
      [] -> Nothing
      ((i,_):_) -> Just i

nstep :: (Applicative m, MonadRandom m) => Int -> World -> m World
nstep n w = foldM (flip id) w $ replicate n step
