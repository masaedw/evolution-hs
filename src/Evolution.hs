module Evolution where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, getRandomRs, runRand)
import qualified Control.Monad.Random as Rnd (fromList)
import Data.Array.IArray as Arr (Array, Ix, assocs, listArray)
import Data.Map as Map (Map, delete, empty, fromList, insert, lookup)
import Data.List (mapAccumL)
import Evolution.Imports

data World = World
             { size :: Point
             , plants :: Plants
             , creatures :: [Creature]
             }

type Plants = Map Point Plant

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
                , energy :: Int
                , direction :: Direction
                }
              deriving (Show)

initCreature :: (Functor m, MonadRandom m) => Point -> m Creature
initCreature p = do
  gen <- initGene
  Creature p gen 200 <$> getRandom

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
               deriving (Eq, Ord, Show, Enum, Bounded, Ix)

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

type Gene = Array Direction Int

initGene :: (Functor m, MonadRandom m) => m Gene
initGene = listArray (minBound, maxBound) <$> getRandomRs (1, 10)

newDirection :: (MonadRandom m) => Gene -> m Direction
newDirection gen =
  Rnd.fromList (fmap fromIntegral <$> assocs gen)

data Plant = Plant
data Point = Point { x :: Int, y :: Int }
           deriving (Eq, Ord, Show)

instance Random Point where
  randomR (s, e) = runRand $ Point <$> getRandomR (x s, x e) <*> getRandomR (y s, y e)
  random         = runRand $ Point <$> getRandom             <*> getRandom

-- | stringify the world
--
-- >>> let plants = fromList [(Point 0 1, Plant), (Point 0 2, Plant)]
-- >>> let gen = listArray (minBound, maxBound) (repeat 1) :: Gene
-- >>> let creatures = [Creature (Point 1 1) gen 200 minBound, Creature (Point 1 2) gen 200 minBound]
-- >>> let expected = unlines ["     ","*M   ","*M   ","     ","     "]
-- >>> let actual = showWorld $ World (Point 5 5) plants creatures
-- >>> expected == actual
-- True

showWorld :: World -> String
showWorld world = unlines $ map lineString [0..(h - 1)]
  where
    lineString :: Int -> String
    lineString l = map (getState . flip Point l) [0..(w - 1)]

    w = width world
    h = height world
    creatureMap = fromList . map withIndex $ creatures world
    withIndex c = (point c, c)
    getState pt = fromMaybe ' ' $ ('M' <$ creature) <|> ('*' <$ plant)
      where creature = Map.lookup pt creatureMap
            plant = Map.lookup pt $ plants world

initWorld :: (Applicative m, MonadRandom m) => Int -> Int -> m World
initWorld w h = do
  creature <- initCreature (Point cw ch)
  return World { size = Point w h
               , plants = Map.empty
               , creatures = [creature]
               }
  where
    [cw, ch] = (`div` 2) <$> [w, h]

turnCreatures :: (Applicative m, MonadRandom m) => World -> m World
turnCreatures world = do
  nc <- mapM turn $ creatures world
  return world { creatures = nc }
  where
    turn c = do
      ndir <- newDirection $ gene c
      return c { direction = ndir }

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

-- | animals eat plants
--
-- >>> let gen = listArray (minBound, maxBound) (repeat 1) :: Gene
-- >>> let w = World { size = Point 3 3, plants = Map.fromList [(Point 0 0, Plant)], creatures = [Creature (Point 0 0) gen 200 North] }
-- >>> let nw = moveCreatures $ eatPlants w
-- >>> showWorld nw
-- "   \n   \nM  \n"
-- >>> energy . head $ creatures nw
-- 280
eatPlants :: World -> World
eatPlants world =
  let (np, nc) = mapAccumL eat (plants world) $ creatures world in
  world { plants = np, creatures = nc }
  where
    eat :: Plants -> Creature -> (Plants, Creature)
    eat ps c =
      case Map.lookup pc ps of
        Just p -> (Map.delete pc ps, c { energy = energy c + 80 })
        Nothing -> (ps, c)
      where pc = point c

-- | create plants
--
-- >>> let x = runRand (initWorld 3 3 >>= addPlants) $ mkStdGen 32
-- >>> let expected = unlines [" * "," M ","   "]
-- >>> let actual = showWorld $ fst x
-- >>> expected == actual
-- True
addPlants :: (MonadRandom m) => World -> m World
addPlants world = do
  points <- mapM getRandomR $ sequence [area, jungleArea] world
  let plants' = foldr (uncurry Map.insert) (plants world) . zip points $ repeat Plant
  return $ world { plants = plants' }

step :: (Applicative m, MonadRandom m) => World -> m World
step = turnCreatures
   >=> return . moveCreatures
   >=> return . eatPlants
   >=> addPlants
