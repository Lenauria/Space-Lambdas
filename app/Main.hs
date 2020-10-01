module Main where

import Graphics
import Datastructure
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Data.Set.Ordered as Set
import System.Random
import Control.Exception


{-  loadImg tries to find the image file (specified in path)
    if image can not be found take a default picture so the game can be played anyway 
    (default picture ist specified in shape) -}
loadImg :: [Char] -> Picture -> IO Picture
loadImg path shape = do
    result <- try $ loadBMP path :: IO (Either IOException Picture)
    case result of
        Left exception  -> pure shape -- in case of exception take the shape
        Right image     -> pure image -- if there's no exception take the image


main :: IO ()
main = do
    gen <- getStdGen
    -- load images
    backgroundImg       <- loadImg "src/img/Space002.bmp" (color black (rectangleSolid 1600 1600))
    backgroundImgWon    <- loadImg "src/img/SpaceWon.bmp" (color black (rectangleSolid 1600 1600))
    backgroundImgLost   <- loadImg "src/img/SpaceLost.bmp" (color black (rectangleSolid 1600 1600))
    bulletImg           <- loadImg "src/img/shoot.bmp" (color green (circleSolid 4))
    enemyBulletImg      <- loadImg "src/img/enemy-shoot.bmp" (color (makeColor 0.8 0.1 0.1 1) (circleSolid 4))
    playerImg           <- loadImg "src/img/Alien.bmp" (color green (circleSolid 50))
    asteroidImg         <- loadImg "src/img/Asteroid.bmp" (color (makeColor 0.7 0.4 0.4 1) (circleSolid 25))
    explosionImg        <- loadImg "src/img/explosion.bmp" (color blue (circleSolid 11))
    explosion2Img       <- loadImg "src/img/explosion2.bmp" (color blue (circleSolid 20))
    heart1Img           <- loadImg "src/img/hearts1.bmp" (color red (rectangleSolid 25 25))
    heart2Img           <- loadImg "src/img/hearts2.bmp" (color red (rectangleSolid 50 25))
    heart3Img           <- loadImg "src/img/hearts3.bmp" (color red (rectangleSolid 75 25))
    enemyImg            <- loadImg "src/img/enemy.bmp" (color (makeColor 0.8 0.1 0.1 1) (rectangleSolid 50 50))
    lambdaImg           <- loadImg "src/img/lambda.bmp" (color yellow (circleSolid 25))

    {-  get the screen size, so the positions can be determined according to the screen, 
        for example the position of the player should have the same distance from the bottom on every screen -}
    (screenWidth, screenHeight) <- getScreenSize 

    -- save the images in a data structure
    let images = Images {
        _backgroundImg      = backgroundImg,
        _backgroundImgWon   = backgroundImgWon,
        _backgroundImgLost  = backgroundImgLost,
        _playerImg          = playerImg,
        _bulletImg          = bulletImg,
        _enemyBulletImg     = enemyBulletImg,
        _asteroidImg        = asteroidImg,
        _explosionImg       = explosionImg,
        _explosion2Img      = explosion2Img,
        _heart1Img          = heart1Img,
        _heart2Img          = heart2Img,
        _heart3Img          = heart3Img,
        _enemyImg           = enemyImg,
        _lambdaImg          = lambdaImg
    }

    -- initialize player
    let initialPlayer = Player {
        _pPosition  = (0, - fromIntegral screenHeight/2 + 100),
        _pHealth    = 3,
        _pScore     = 0,
        _pHits      = 0,
        _pLambdas   = 0,
        _pVelocity  = (0, 0),
        _pWidth     = 100
    }

    -- initialize world
    let world gen = World {
        _player         = initialPlayer,
        _status         = Pause,
        _lambda         = Prelude.take 3 [initLambda $ mkStdGen g | g <- randoms gen],
        _keysPressed    = Set.empty,        
        _asteroids      = Prelude.take 10 [initAsteroid $ mkStdGen g | g <- randoms gen],
        _bullets        = [],
        _enemyBullets   = [],
        _explosions     = [],
        _enemies        = [],
        _images         = images,
        _wSeed          = 0,
        _screenSize     = (screenWidth, screenHeight)
    }

    -- play function of Gloss
    play windowDisplay backgroundColor 60 (world gen) drawFunc inputHandler (updateFunc gen)