module Evolution where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, runRand)
import Data.Map as Map (Map, empty, fromList, insert, lookup)
import Evolution.Imports

data World = World
             { size :: Point
             , plants :: Map Point Plant
             , creatures :: [Creature]
             }

width :: World -> Int
width = x . size

height :: World -> Int
height = y . size

area :: World -> (Point, Point)
area w = (Point 0 0, Point (width w - 1) (height w - 1))

jungleArea :: World -> (Point, Point)
jungleArea w = (startPoint, endPoint)
  where
    jungleWidth = 10 :: Int
    jungleHeight = 10 :: Int
    sx = (width w - jungleWidth) `div` 2 :: Int
    sy = (height w - jungleHeight) `div` 2 :: Int

    startPoint = Point sx sy
    endPoint = Point (x startPoint + jungleWidth - 1) (y startPoint + jungleHeight - 1)

data Creature = Creature
                { point :: Point
                , gene :: Gene
                , direction :: Direction
                }

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

data Gene = Gene
data Plant = Plant
data Point = Point { x :: Int, y :: Int }
           deriving (Eq, Ord, Show)

instance Random Point where
  randomR (s, e) = runRand $ Point <$> getRandomR (x s, x e) <*> getRandomR (y s, y e)
  random         = runRand $ Point <$> getRandom             <*> getRandom

-- | stringify the world
--
-- >>> let plants = fromList [(Point 0 1, Plant), (Point 0 2, Plant)]
-- >>> let creatures = [Creature (Point 1 1) Gene minBound, Creature (Point 1 2) Gene minBound]
-- >>> let expected = unlines ["     ","*M   ","*M   ","     ","     "]
-- >>> let actual = showWorld $ World (Point 5 5) plants creatures
-- >>> expected == actual
-- True

showWorld :: World -> String
showWorld world = unlines [lineString y | y <- [0..(h - 1)]]
  where
    lineString :: Int -> String
    lineString y = [getState $ Point x y | x <- [0..(w - 1)]]

    w = width world
    h = height world
    creatureMap = fromList . map withIndex $ creatures world
    withIndex c = (point c, c)
    getState point = fromMaybe ' ' $ ('M' <$ creature) <|> ('*' <$ plant)
      where creature = Map.lookup point creatureMap
            plant = Map.lookup point $ plants world

initWorld :: (Applicative m, MonadRandom m) => Int -> Int -> m World
initWorld w h = do
  creature <- Creature (Point cw ch) Gene <$> getRandom
  return World { size = Point w h
               , plants = Map.empty
               , creatures = [creature]
               }
  where
    [cw, ch] = (`div` 2) <$> [w, h]

moveCreatures :: World -> World
moveCreatures world = world { creatures = move <$> creatures world }
  where
    Point w h = size world
    move c = c { point = np }
      where
        op = point c
        np = Point ((x op + dx) `mod` w) ((y op + dy) `mod` h)
        (dx, dy) = case direction c of
          North     -> ( 0, -1)
          Northeast -> ( 1, -1)
          East      -> ( 1,  0)
          Southeast -> ( 1,  1)
          South     -> ( 0,  1)
          Southwest -> (-1,  1)
          West      -> (-1,  0)
          Northwest -> (-1, -1)

-- | create plants
--
-- >>> let x = runRand (initWorld 3 3 >>= addPlants) $ mkStdGen 32
-- >>> let expected = unlines ["   "," M ","  *"]
-- >>> let actual = showWorld $ fst x
-- >>> expected == actual
-- True
addPlants :: (MonadRandom m) => World -> m World
addPlants world = do
  points <- mapM getRandomR $ sequence [area, jungleArea] world
  let plants' = foldr (uncurry Map.insert) (plants world) . zip points $ repeat Plant
  return $ world { plants = plants' }

step :: (MonadRandom m) => World -> m World
step = return . moveCreatures >=> addPlants
