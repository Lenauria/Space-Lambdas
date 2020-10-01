{-# LANGUAGE TemplateHaskell #-}
module Datastructure where

import Debug.Trace
import Data.Set.Ordered as Set
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import Graphics.Gloss.Interface.Environment ()
import System.Random

-- type synonyms
type Score          = Int
type Healthpoints   = Int
type Coordinate     = (Float, Float)
type Velocity       = (Float, Float)


-- data types for player, bullets, asteroids, images, world and world status
data WorldStatus = Pause | Playing | Won | Lost
    deriving (Show, Eq)

data Asteroid = Asteroid {
    _aPosition      :: Coordinate,
    _aVelocity      :: Velocity,
    _rotation, _rotationSpeed, _aWidth  :: Float,
    _aHits          :: Int
} deriving (Show, Eq)

data Bullet = Bullet {
    _bPosition :: Coordinate,
    _bVelocity :: Velocity,
    _bHits     :: Int,
    _bWidth   :: Float
} deriving (Show, Eq)

data Images = Images {
    _backgroundImg      :: Picture,
    _backgroundImgWon   :: Picture,
    _backgroundImgLost  :: Picture,
    _playerImg          :: Picture,
    _bulletImg          :: Picture,
    _enemyBulletImg     :: Picture,
    _asteroidImg        :: Picture,
    _explosionImg       :: Picture,
    _explosion2Img      :: Picture,
    _heart1Img          :: Picture,
    _heart2Img          :: Picture,
    _heart3Img          :: Picture,
    _enemyImg           :: Picture,
    _lambdaImg          :: Picture
} deriving Show

data Lambda = Lambda {
    _lPosition  :: Coordinate,
    _lVelocity  :: Velocity,
    _lWidth    :: Float,
    _lHits      :: Int
} deriving (Show)

data Player = Player {
    _pPosition          :: Coordinate,
    _pHealth            :: Healthpoints,
    _pScore             :: Score,
    _pHits, _pLambdas   :: Int,
    _pVelocity          :: Velocity,
    _pWidth             :: Float
} deriving Show

data Enemy = Enemy {
    _enemyTime      :: Float,
    _enemyPosition  :: Coordinate,
    _enemyVelocity  :: Velocity,
    _enemyHits      :: Int,
    _enemyWidth     :: Float
} deriving Show

data Explosion = Explosion {
    _ePosition  :: Coordinate,
    _eTime      :: Float,
    _scaling    :: (Float, Float),
    _eImg       :: Picture
} deriving Show

data World = World {
    _asteroids              :: [Asteroid],
    _bullets, _enemyBullets :: [Bullet],
    _enemies                :: [Enemy],
    _explosions             :: [Explosion],
    _lambda                 :: [Lambda],
    _player                 :: Player,
    _status                 :: WorldStatus,
    _keysPressed            :: Set.OSet SpecialKey,
    _images                 :: Images,
    _wSeed                  :: Float,
    _screenSize             :: (Int, Int)
} deriving Show

-- make lenses for the datatypes by using Template Haskell
makeLenses ''Bullet
makeLenses ''Explosion
makeLenses ''Player
makeLenses ''Asteroid
makeLenses ''World
makeLenses ''Images
makeLenses ''Enemy
makeLenses ''Lambda

{- these methods create random positions and velocity for asteroids and lambdas -}
randomPosition :: StdGen -> Coordinate
randomPosition gen0 = (x,y)
    where
        (x, gen1) = randomR(-600.0, 600.0) gen0
        (y, gen2) = randomR(500, 1200.0) gen1

randomPositionLambda :: StdGen -> Coordinate
randomPositionLambda gen0 = (x,y)
    where
        (x, gen1) = randomR(-600.0, 600.0) gen0
        (y, gen2) = randomR(800, 1200.0) gen1

randomVelocity :: StdGen -> Float
randomVelocity gen0 = v
    where
        (v, g1) = randomR(-3, -5) gen0

{- initAsteroid creates a new asteroid with a random position and velocity -}
initAsteroid :: StdGen -> Asteroid
initAsteroid g0 = Asteroid {
        _aPosition = randomPosition g0,
        _aVelocity = (0, randomVelocity g0),
        _rotation = 0.0,
        _rotationSpeed = 3,
        _aHits = 0,
        _aWidth = 50
}

{- initLambda creates a new lambda with a random position -}
initLambda :: StdGen -> Lambda
initLambda g0 = Lambda{
    _lPosition = randomPositionLambda g0,
    _lVelocity = (0, -5),
    _lHits = 0,
    _lWidth = 50
}

{- initEnemy creates a new enemy and appends it to the list of enemies.
In our game only one enemy can appear at once, so only if list is empty,
    a list containing one enemy is created -}
initEnemy :: [Enemy] -> World -> [Enemy]
initEnemy [] world = [Enemy {
    _enemyTime = 0,
    _enemyPosition = (-800, fromIntegral (world ^. screenSize . _2)/2 - 100),
    _enemyVelocity = (2, 0),
    _enemyHits = 0,
    _enemyWidth = 100
}]
initEnemy xs world = xs

-- get screen width
screenWidth :: World -> Int
screenWidth world = world ^. screenSize . _1

-- returns 1,2 or 3 hearts according to the health points
getHeartImg :: (Eq a, Num a) => World -> a -> Picture
getHeartImg world hp
    | hp == 1 = world ^. (images.heart1Img)
    | hp == 2 = world ^. (images.heart2Img)
    | hp == 3 = world ^. (images.heart3Img)
    | otherwise = Text "lost"


makeBulletExplosion :: World -> Coordinate -> Explosion
makeBulletExplosion world coordinate = Explosion {
    _ePosition = coordinate,
    _eTime = 0,
    _scaling = (0,0),
    _eImg = world ^. (images . explosionImg)
}

makeAsteroidExplosion :: World -> Coordinate -> Explosion
makeAsteroidExplosion world coordinate = Explosion {
    _ePosition = coordinate,
    _eTime = 0,
    _scaling = (0,0),
    _eImg = world ^. (images.explosion2Img)
}


-- add key to set of pressed keys and set speed of player accordingly to the key with index 0 in the set
addKey :: SpecialKey -> World -> World
addKey key = keysPressed %~ (key |<)

-- remove key if the key is released
removeKey :: SpecialKey -> World -> World
removeKey key = keysPressed %~ Set.delete key

updatePosition :: World -> Player
updatePosition world = (world ^. player) & pPosition %~ addCoordinates (world ^. (player . pVelocity))

updatePositionAsteroid :: Asteroid -> World -> Asteroid
updatePositionAsteroid asteroid world = asteroid & aPosition %~ addCoordinates (asteroid ^. aVelocity)

addCoordinates :: Coordinate -> Velocity -> Coordinate
addCoordinates (a,b) (c,d) = (a+c,b+d)

-- add bullet to world
shoot :: World -> World
shoot world = world & bullets %~ (makePlayerBullet world : )
    where
        makePlayerBullet world = Bullet {
            _bPosition = bulletPosition,
            _bVelocity = (0,10.0),
            _bHits = 0,
            _bWidth = 14
        } where
            bulletPosition = getPosition (_player world)

-- add a score to the score of the player (e.g. when destroying an asteroid)
addScore :: Player -> Score -> Player
addScore player score = player & pScore %~ (+ score)

-- subtract the number of hits from the healthpoints of the player
remHealth :: Player -> Healthpoints -> Player
remHealth player hp = player & (pHealth %~ subtract hp)

-- add a collected lambda to the lambdacount of the player
addLambda :: Player -> Int -> Player 
addLambda player lambdaCount = player & (pLambdas +~ lambdaCount)

-- generate enemies bullet that goes in direction of the player
makeEnemyBullet :: HasPosition a => a -> World -> Bullet
makeEnemyBullet enemy world = Bullet {
    _bPosition = bulletPosition,
    _bVelocity = (-v1, -v2),
    _bHits = 0,
    _bWidth = 14
} where
    bulletPosition = getPosition enemy
    v1 = (enemy ^. xPosition - (world ^. (player . xPosition)))/100
    v2 = (enemy ^. yPosition - (world ^. (player . yPosition)))/100


{- type class HasPosition provides a generic lens "position", 
that represents the specific position lenses of the different data types.
With that you can access the positions of the different data types with the generic function "getPosition"
and don't have to write the same function for all the data types multiple times. -}
class HasPosition a where
    position        :: Lens' a Coordinate

-- generic getPosition, xPosition, getXPosition, yPosition and getYPosition functions that work for all Positionble types
getPosition :: HasPosition a => a -> Coordinate
getPosition x = x ^. position

xPosition :: HasPosition a => Lens' a Float
xPosition = position . _1

yPosition :: HasPosition a => Lens' a Float
yPosition = position . _2


{- all Entities have a position (HasPosition a => Entity a) -}
class HasPosition a => Entity a where
    hits            :: Lens' a Int
    velocity        :: Lens' a Velocity
    width           :: Lens' a Float

-- generic addHit, getHits, setHit, getVelocity, setVelocity functions that work for all Entity types
addHit :: Entity a => a -> a
addHit x = x & hits %~ (+1)

getHits :: Entity a => a -> Int
getHits x = x ^. hits

setHit :: Entity a => a -> Int -> a
setHit x y = x & hits .~ y

getVelocity :: Entity a => a -> Velocity
getVelocity x = x ^. velocity

setVelocity :: Entity a => a -> Velocity -> a
setVelocity x v = x & velocity .~ v

getWidth :: Entity a => a -> Float
getWidth x = x ^. width

{- class for types where the measured time (amuont of ticks) is relevant -}
class Timeable a where
    time :: Lens' a Float

-- generic getTime and addTime functions that work for all Timeable types
getTime :: Timeable a => a -> Float
getTime x = x ^. time

addTime :: Timeable a => a -> Float -> a
addTime x t  = x & time %~ (+t)


{- instance declarations -}
-- Bullet
instance HasPosition Bullet where
    position = bPosition

instance Entity Bullet where
    hits        = bHits
    velocity    = bVelocity
    width       = bWidth


-- Explosion
instance HasPosition Explosion where
    position = ePosition

instance Timeable Explosion where
    time = eTime


-- Enemy
instance HasPosition Enemy where
    position = enemyPosition

instance Entity Enemy where
    width       = enemyWidth
    velocity    = enemyVelocity
    hits        = enemyHits

instance Timeable Enemy where
    time = enemyTime


-- Asteroid
instance HasPosition Asteroid where
    position = aPosition

instance Entity Asteroid where
    hits        = aHits
    velocity    = aVelocity
    width       = aWidth


-- Lambda
instance HasPosition Lambda where
    position = lPosition

instance Entity Lambda where
    hits        = lHits
    velocity    = lVelocity
    width       = lWidth


-- Player
instance HasPosition Player where
    position = pPosition

instance Entity Player where
    hits        = pHits
    velocity    = pVelocity
    width       = pWidth


-- debugging :: Show a1 => a1 -> a2 -> a2
-- debugging world = trace ("World: " ++ show world)