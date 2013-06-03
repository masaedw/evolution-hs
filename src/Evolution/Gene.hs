module Evolution.Gene
  ( Direction
  , Gene
  , delta
  , initGene
  , mutateGene
  , newDirection
  ) where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, getRandomRs, runRand)
import Evolution.Imports

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

type Gene = [(Direction, Int)]

initGene :: (Functor m, MonadRandom m) => m Gene
initGene = zip [minBound ..] <$> getRandomRs (1, 10)

mutateGene :: (Functor m, MonadRandom m) => Gene -> m Gene
mutateGene g = do
  i <- getRandom
  r <- getRandomR (-1, 1)
  let f (d, v) | d == i && 1 <= v + r = (d, v + r)
      f a = a
  return $ map f g

randomFromList :: (MonadRandom m) => [(a, Int)] -> m a
randomFromList list = do
  r <- getRandomR (1, mx)
  return . fst . head . dropWhile (\(_, v) -> v < r) $ xs
  where
    xs = scanl1 (\(_, a) (k, v) -> (k, a + v)) list
    mx = foldl (\a (_, r) -> a + r) 0 list

newDirection :: (MonadRandom m) => Gene -> m Direction
newDirection = randomFromList

delta :: Direction -> (Int, Int)
delta North     = ( 0, -1)
delta Northeast = ( 1, -1)
delta East      = ( 1,  0)
delta Southeast = ( 1,  1)
delta South     = ( 0,  1)
delta Southwest = (-1,  1)
delta West      = (-1,  0)
delta Northwest = (-1, -1)
