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

    sx :: Int
    sx = floor $ fromIntegral (width w - jungleWidth) / 2

    sy :: Int
    sy = floor $ fromIntegral (height w - jungleHeight) / 2

    startPoint = Point sx sy
    endPoint = Point (x startPoint + jungleWidth - 1) (y startPoint + jungleHeight - 1)

data Creature = Creature
                { point :: Point
                , gene :: Gene
                , direction :: Direction
                }

data Direction = Direction
data Gene = Gene
data Plant = Plant
data Point = Point { x :: Int, y :: Int }
           deriving (Eq, Ord, Show)

instance Random Point where
  randomR (s, e) = runRand $ Point <$> getRandomR (x s, x e) <*> getRandomR (y s, y e)
  random         = runRand $ Point <$> getRandom             <*> getRandom

-- | stringify the world
--
-- >>> let plants = fromList [((0::Int, 1::Int), Plant) ,((0, 2), Plant)]
-- >>> let creatures = [Creature 1 1 Gene Direction, Creature 1 2 Gene Direction]
-- >>> let expected = unlines ["     ","*M   ","*M   ","     ","     "]
-- >>> let actual = showWorld $ World 5 5 plants creatures
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

initWorld :: Int -> Int -> World
initWorld x y =
  World { size = Point x y
        , plants = Map.empty
        , creatures = []
        }

-- | create plants
--
-- >>> let x = runState (addPlants $ initWorld 3 3) $ mkStdGen 32
-- >>> let expected = unlines ["   ","   ","  *"]
-- >>> let actual = showWorld $ fst x
-- >>> expected == actual
-- True
addPlants :: (MonadRandom m) => World -> m World
addPlants world = do
  point <- getRandomR $ area world
  point' <- getRandomR $ jungleArea world
  let plants' = Map.insert point Plant $ plants world
      plants'' = Map.insert point' Plant $ plants'
  return $ world { plants = plants'' }

step :: (MonadRandom m) => World -> m World
step = addPlants
