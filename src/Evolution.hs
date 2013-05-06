module Evolution where

import Control.Applicative ((<$), (<|>))
import Data.Functor ((<$>))
import Data.Map as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)

data World = World
             { width :: Int
             , height :: Int
             , plants :: Map (Int, Int) Plant
             , creatures :: [Creature]
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
-- >>> let actual = showWorld $ World 5 5 plants creatures
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

initWorld :: Int -> Int -> World
initWorld x y =
  World { width = x
        , height = y
        , plants = fromList [((0, 1), Plant)
                            ,((0, 2), Plant)
                            ]
        , creatures = [Creature 0 1 Gene Direction
                      ,Creature 0 2 Gene Direction
                      ,Creature 0 3 Gene Direction
                      ]
        }

step :: World -> World
step = id -- undefined
