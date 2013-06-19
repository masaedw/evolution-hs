module Evolution where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, runRand)
import Data.Foldable (foldrM)
import Data.List (mapAccumL)
import Data.Map as Map (Map, delete, empty, fromList, insert, lookup)
import Evolution.Gene (Direction, Gene, delta, initGene, mutateGene, newDirection)
import Evolution.Imports

-- $setup
-- >>> let testGene = fst . runRand initGene $ mkStdGen 37

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
    jungleWidth = 10
    jungleHeight = 10
    sx = (width w - jungleWidth) `div` 2
    sy = (height w - jungleHeight) `div` 2

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

data Plant = Plant
data Point = Point { x :: Int, y :: Int }
           deriving (Eq, Ord, Show)

instance Random Point where
  randomR (s, e) = runRand $ Point <$> getRandomR (x s, x e) <*> getRandomR (y s, y e)
  random         = runRand $ Point <$> getRandom             <*> getRandom

-- | stringify the world
--
-- >>> let plants = fromList [(Point 0 1, Plant), (Point 0 2, Plant)]
-- >>> let creatures = [Creature (Point 1 1) testGene 200 minBound, Creature (Point 1 2) testGene 200 minBound]
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

removeDeadCreatures :: World -> World
removeDeadCreatures world = world { creatures = nc }
  where
    nc = filter ((> 0) . energy) $ creatures world

moveCreatures :: World -> World
moveCreatures world = world { creatures = move <$> creatures world }
  where
    Point w h = size world
    move c = c { point = np, energy = ne }
      where
        ne = energy c - 1
        op = point c
        np = Point ((x op + dx) `mod` w) ((y op + dy) `mod` h)
        (dx, dy) = delta $ direction c

-- | animals eat plants
--
-- >>> let w = World { size = Point 3 3, plants = Map.fromList [(Point 0 0, Plant)], creatures = [Creature (Point 0 0) testGene 200 minBound] }
-- >>> let nw = moveCreatures $ eatPlants w
-- >>> showWorld nw
-- "   \n   \nM  \n"
-- >>> energy . head $ creatures nw
-- 279
eatPlants :: World -> World
eatPlants world =
  let (np, nc) = mapAccumL eat (plants world) $ creatures world in
  world { plants = np, creatures = nc }
  where
    eat :: Plants -> Creature -> (Plants, Creature)
    eat ps c =
      case Map.lookup pc ps of
        Just _ -> (Map.delete pc ps, c { energy = energy c + 80 })
        Nothing -> (ps, c)
      where pc = point c


devide :: (Functor m, MonadRandom m) => Creature -> [Creature] -> m [Creature]
devide c@Creature { energy = en } cx
  | en < 200 = return (c:cx)
  | otherwise = do
      gen <- mutateGene $ gene c
      return $ c { energy = en `div` 2 } : c { energy = en `div` 2, gene = gen } : cx

reproduceCreatures :: (Functor m, MonadRandom m) => World -> m World
reproduceCreatures world = do
  nc <- foldrM devide [] $ creatures world
  return $ world { creatures = nc }

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
   >=> return . removeDeadCreatures
   >=> return . moveCreatures
   >=> return . eatPlants
   >=> reproduceCreatures
   >=> addPlants
