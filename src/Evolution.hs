module Evolution where

import Data.Map as Map (Map, empty, fromList, insert, lookup)
import Evolution.Imports

data World = World
             { width :: Int
             , height :: Int
             , plants :: Map (Int, Int) Plant
             , creatures :: [Creature]
             , jungle :: ((Int, Int), (Int, Int))
             }

data Creature = Creature
                { x :: Int
                , y :: Int
                , gene :: Gene
                , direction :: Direction
                }

data Direction = Direction
data Gene = Gene
data Plant = Plant

-- | stringify the world
--
-- >>> let plants = fromList [((0::Int, 1::Int), Plant) ,((0, 2), Plant)]
-- >>> let creatures = [Creature 1 1 Gene Direction, Creature 1 2 Gene Direction]
-- >>> let expected = unlines ["     ","*M   ","*M   ","     ","     "]
-- >>> let actual = showWorld $ World 5 5 plants creatures ((0,0), (0,0))
-- >>> expected == actual
-- True

showWorld :: World -> String
showWorld world = unlines [lineString y | y <- [0..(h - 1)]]
  where
    lineString :: Int -> String
    lineString y = [getState x y | x <- [0..(w - 1)]]

    w = width world
    h = height world
    creatureMap = fromList . map withIndex $ creatures world
    withIndex c = ((x c, y c), c)
    getState x y = fromMaybe ' ' $ ('M' <$ creature) <|> ('*' <$ plant)
      where creature = Map.lookup (x, y) creatureMap
            plant = Map.lookup (x, y) $ plants world

initWorld :: Int -> Int -> ((Int, Int), (Int, Int)) -> World
initWorld x y region =
  World { width = x
        , height = y
        , plants = Map.empty
        , creatures = []
        , jungle = region
        }

randomRSt :: (RandomGen g, Random a, Monad m) => (a, a) -> StateT g m a
randomRSt range = state $ randomR range

-- | create plants
--
-- >>> let x = evalState (addPlants $ initWorld 3 3 ((1,1), (1,1))) $ mkStdGen 32
-- >>> let expected = unlines ["   "," * ","  *"]
-- >>> let actual = showWorld x
-- >>> expected == actual
-- True
addPlants :: (Monad m) => World -> StateT StdGen m World
addPlants world = foldM addPlant world [((0, 0), (width world - 1, height world - 1)), jungle world]
  where
    addPlant :: (Monad m) => World -> ((Int, Int), (Int, Int)) -> StateT StdGen m World
    addPlant w ((left, top), (right, bottom)) = do
      x <- randomRSt (left, right)
      y <- randomRSt (top, bottom)
      let newPlants = Map.insert (x, y) Plant $ plants w
      return $ w { plants = newPlants }

step :: (Monad m) => World -> StateT StdGen m World
step = addPlants
