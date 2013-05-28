module Evolution where

import Control.Monad.Random (MonadRandom, getRandom, getRandomR, getRandomRs, runRand)
import Data.Foldable (foldrM)
import Data.List (mapAccumL)
import Data.Map as Map (Map, delete, empty, fromList, insert, lookup)
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
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Random Direction where
  randomR (s, e) = runRand $ toEnum <$> getRandomR (fromEnum s, fromEnum e)
  random = randomR (minBound, maxBound)

type Gene = [(Direction, Int)]

initGene :: (Functor m, MonadRandom m) => m Gene
initGene = do
  rs <- getRandomRs (1::Int, 10::Int)
  return $ zip [(minBound::Direction)..] rs

mutateGene :: (Functor m, MonadRandom m) => Gene -> m Gene
mutateGene g = forM g $ \(d, i) -> do
  r <- getRandomR (-1, 1)
  return $ (d, i + r)

randomFromList :: (MonadRandom m) => [(a, Int)] -> m a
randomFromList list = do
  r <- getRandomR (1, mx)
  return . fst . head . dropWhile (\(_, v) -> v < r) $ xs
  where
    xs = scanl1 (\(_, a) (k, v) -> (k, a + v)) list
    mx = foldl (\a (_, r) -> a + r) 0 list

newDirection :: (MonadRandom m) => Gene -> m Direction
newDirection gen = randomFromList gen

data Plant = Plant
data Point = Point { x :: Int, y :: Int }
           deriving (Eq, Ord, Show)

instance Random Point where
  randomR (s, e) = runRand $ Point <$> getRandomR (x s, x e) <*> getRandomR (y s, y e)
  random         = runRand $ Point <$> getRandom             <*> getRandom

-- | stringify the world
--
-- >>> let plants = fromList [(Point 0 1, Plant), (Point 0 2, Plant)]
-- >>> let gen = zip [minBound ..] (repeat 1) :: Gene
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
-- >>> let gen = zip [minBound ..] (repeat 1) :: Gene
-- >>> let w = World { size = Point 3 3, plants = Map.fromList [(Point 0 0, Plant)], creatures = [Creature (Point 0 0) gen 200 North] }
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
devide c cx =
  if energy c < 200
  then return $ c:cx
  else do
    gen <- mutateGene $ gene c
    return $ c { energy = energy c `div` 2 } : c { energy = energy c `div` 2, gene = gen } : cx

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
