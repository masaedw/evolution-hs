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

minDir :: Direction
minDir = minBound

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

data Gene = Gene Int [Int] deriving (Show)

makeGene :: [Int] -> Gene
makeGene list = Gene mx xs
  where
    mx = sum list
    xs = scanl1 (+) list

initGene :: (Functor m, MonadRandom m) => m Gene
initGene = do
  list <- zipWith (flip const) [minDir ..] <$> getRandomRs (1, 10)
  return $ makeGene list

mutateGene :: (MonadRandom m) => Gene -> m Gene
mutateGene a@(Gene _ g) = do
  r <- getRandomR (-1, 1)
  if r == 0 then return a
  else do
    i <- getRandom
    let f d v | d == i && 1 <= v + r = v + r
        f _ v = v
        ps = zipWith f [minDir ..] $ zipWith (-) g (0:g)
    return $ makeGene ps

newDirection :: (MonadRandom m) => Gene -> m Direction
newDirection (Gene mx xs) = do
  r <- getRandomR (1, mx)
  return . toEnum . length . takeWhile (< r) $ xs

delta :: Direction -> (Int, Int)
delta North     = ( 0, -1)
delta Northeast = ( 1, -1)
delta East      = ( 1,  0)
delta Southeast = ( 1,  1)
delta South     = ( 0,  1)
delta Southwest = (-1,  1)
delta West      = (-1,  0)
delta Northwest = (-1, -1)
