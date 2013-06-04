module Evolution.Gene
  ( Direction
  , Gene
  , delta
  , initGene
  , mutateGene
  , newDirection
  ) where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, runRand)
import Evolution.Imports

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

data Gene = Gene Int [(Direction, Int)] deriving (Show)

initGene :: (Functor m, MonadRandom m) => m Gene
initGene = do
  list <- replicateM dircount $ getRandomR (1, 10)
  let mx = sum list
      xs = scanl1 (+) list
  return . Gene mx $ zip [minBound ..] xs
  where
    dircount = fromEnum (maxBound :: Direction) - fromEnum (minBound :: Direction) + 1

mutateGene :: (Functor m, MonadRandom m) => Gene -> m Gene
mutateGene a@(Gene _ g) = do
  r <- getRandomR (-1, 1)
  if r == 0 then return a
  else do
    i <- getRandom
    let f (d, v) | d == i && 1 <= v + r = (d, v + r)
        f x = x
        ps = map f $ zipWith (\(d, v) w -> (d, v - w)) g (0:map snd g)
        xs = scanl1 (\(_, v) (k, w) -> (k, v + w)) ps
        mx = sum . map snd $ ps
    return $ Gene mx xs

randomFromList :: (MonadRandom m) => Int -> [(a, Int)] -> m a
randomFromList mx xs = do
  r <- getRandomR (1, mx)
  return . fst . head . dropWhile (\(_, v) -> v < r) $ xs

newDirection :: (MonadRandom m) => Gene -> m Direction
newDirection (Gene mx xs) = randomFromList mx xs

delta :: Direction -> (Int, Int)
delta North     = ( 0, -1)
delta Northeast = ( 1, -1)
delta East      = ( 1,  0)
delta Southeast = ( 1,  1)
delta South     = ( 0,  1)
delta Southwest = (-1,  1)
delta West      = (-1,  0)
delta Northwest = (-1, -1)
