module Graphics where

import Debug.Trace

import Datastructure
import Collision
import Graphics.Gloss
import Control.Monad.State
import Data.Set.Ordered as Set
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Data.Maybe
import Data.Ix
import System.Random
import Control.Lens
import Control.Lens.Traversal

-- Display mode -> FullScreen
windowDisplay :: Display
windowDisplay = FullScreen 

-- Set the backgroundColor
backgroundColor :: Color
backgroundColor = black

-- The initial world state. Depending on the different world states, a different screen is shown
-- When the state is on "Pause" -> only the background is shown with instructions how to start/continue the game
-- When the state is on "Verloren" -> only the background is shown with a capture and instructions how to restart the game
-- When the state is on "Gewonnen" -> a similar screen is shown as in the state "Verloren" with fitting capture
-- When the state is on "Spielend" -> show background, as well the asteroids, the player, enemy and lambda. 
drawFunc :: World -> Picture
drawFunc world  | world ^. status == Pause    = pictures $ [world ^. (images.backgroundImg)] ++ [showStartViewSecondLine] ++ [showStartViewThirdLine] ++ [showStartView] 
                  | world ^. status == Lost     = pictures $ (world ^. (images . backgroundImgLost)) : [showLostView]
                  | world ^. status == Won      = pictures $ (world ^. (images . backgroundImgWon)) : [showWinView]
                  | world ^. status == Playing  = pictures ([world ^. (images.backgroundImg)] ++ showAsteroid ++ showLambda ++ showBullets ++ showEnemyBullets ++ [showPlayer world (world ^. player)] ++ [showHealthPoints (world ^. player)] ++ showEnemies ++ showExplosions ++ [showScore (world ^. player)] ++ [showLambdaPoints (world ^. player)])
                  | otherwise                   = pictures ([world ^. (images.backgroundImg)] ++ showAsteroid ++ showBullets ++ showEnemyBullets ++ [showPlayer world (world ^. player)] ++ [showHealthPoints (world ^. player)] ++ showEnemies ++ showExplosions ++ [showScore (world ^. player)] ++ [showLambdaPoints (world ^. player)])
 where 
    showStartView :: Picture
    showStartView = pictures $ showStartViewSecondLine : [translate (- 100) 0 $ Scale 0.16 0.25 (showText white "Welcome to Space Lambdas!")]
    showStartViewSecondLine :: Picture
    showStartViewSecondLine = translate (-450) (-50) $ Scale 0.16 0.25 (showText white "Collect all the lambdas without being hit by the asteroids. Also beware of any hostile UFOs!")
    showStartViewThirdLine :: Picture
    showStartViewThirdLine = pictures $ translate (- 350) (- 150) (Scale 0.16 0.25 (showText white "Navigate your spaceship with left and right arrow keys and shoot with the spacebar.")) : [translate (- 130) (- 250) $ Scale 0.15 0.25 (showText white "Press Enter to start. Good Luck! ")]
    showLostView :: Picture
    showLostView = translate (-350) 0 $ Scale 0.25 0.25 (showText white "You've lost! Press Enter to start a new game.")
    showWinView :: Picture
    showWinView = translate (-370) 0 $ Scale 0.25 0.25 (showText white "Congratulations! Press Enter to start a new game.")
    showPlayer :: World -> Player -> Picture
    showPlayer world player = translate (player ^. xPosition) (player ^. yPosition) (world ^. (images.playerImg))
    showBullets :: [Picture]
    showBullets = map (displayBullet world) (world ^. bullets)
      where displayBullet world bullet = translate (bullet ^. xPosition) (bullet ^. yPosition) (world ^. (images.bulletImg))
    showEnemyBullets :: [Picture]
    showEnemyBullets = map (displayBullet world) (_enemyBullets world)
      where displayBullet world bullet = translate (bullet ^. xPosition) (bullet ^. yPosition) (world ^. (images.enemyBulletImg))
    showExplosions ::[Picture]
    showExplosions = map (displayExplosion world) (_explosions world)
      where displayExplosion world explosion = translate (explosion ^. xPosition) (explosion ^. yPosition) (Scale ((explosion ^. eTime)/5) ((explosion ^. eTime)/5) (explosion ^. eImg))
    showHealthPoints :: Player -> Picture -- right upper corner of screen
    showHealthPoints player = color lifePointColor $ translate (fromIntegral(world ^. screenSize . _1)/2 - 150) (fromIntegral(world ^. screenSize . _2)/2 -50) (showHearts world (player ^. pHealth))
      where lifePointColor = yellow
            showHearts world x = getHeartImg world x
    showScore :: Player -> Picture
    showScore player = color lifePointColor $ translate (fromIntegral(world ^. screenSize . _1)/2 - 200) (fromIntegral(world ^. screenSize . _2)/2 - 100) (Scale 0.20 0.20 (Text ("Score: " ++ show (player ^. pScore))))
      where lifePointColor = yellow
    showLambdaPoints :: Player -> Picture
    showLambdaPoints player = color lifePointColor $ translate (fromIntegral(world ^. screenSize . _1)/2 - 200) (fromIntegral(world ^. screenSize . _2)/2 - 150) (Scale 0.20 0.20 (Text ("Lamdas: " ++ show (player ^. pLambdas) ++ "/3")))
      where lifePointColor = yellow
    showEnemies :: [Picture]
    showEnemies = map (displayEnemy world) (_enemies world)
      where displayEnemy world enemy = translate (enemy ^. xPosition) (enemy ^. yPosition) (world ^. (images.enemyImg))
    showAsteroid :: [Picture] 
    showAsteroid =  map (displayAsteroid world) (_asteroids world) 
      where displayAsteroid world asteroid = translate (asteroid ^. xPosition) (asteroid ^. yPosition) (rotate (asteroid ^. rotation) (world ^. (images.asteroidImg)))
    showLambda :: [Picture] 
    showLambda = map (displayLambdas world) (_lambda world)
      where displayLambdas world lambda = translate (lambda ^. xPosition) (lambda ^. yPosition) (world ^. (images.lambdaImg))
    showText :: Color -> String -> Picture
    showText color text = Color color (Text text)
      


{- possible keys: left arrow key, right arrow key, space 
    if an arrow key is pressed:
      1. add the key to the set of actual pressed keys in the world (addKey :: SpecialKey -> World -> World (module Typklassen))
      2. set the speed of the player according to the key, that is at index 0 of the set of pressed keys. 
      With that there is the possibility to press the left and right arrow key at the same time (or let one key pressed and press the other key additionaly). 
      The last pressed key is decisive. And with that we can remember the order of pressed keys and when the last pressed key is released the key pressed before is decisive.
    
    if the space key is pressed:
      shoot
    if the Enter key is pressed:
      start the game
    if the p or P key are pressed:
      pause the game -}
inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) world  = updatePlayerSpeed (addKey KeyRight world)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) world   = updatePlayerSpeed (addKey KeyLeft world)
inputHandler (EventKey (SpecialKey KeyRight) Up _ _) world    = updatePlayerSpeed (removeKey KeyRight world)
inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) world     = updatePlayerSpeed (removeKey KeyLeft world)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world  = shoot world
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _ ) world = startGame world
inputHandler (EventKey (Char 'p') Down _ _ ) world            = pauseGame world
inputHandler (EventKey (Char 'P') Down _ _ ) world            = pauseGame world
inputHandler _ world                                          = world -- do nothing, if no key is pressed



-- A function to convert the world to a picture depending on the world state
-- If the world state is "Spielend", the player has still more than 0 healthpoints, there are still asteroids in the play and the amount of collected lambdas is not 3, then generate a new world with updating objects
-- In the same state ("Spielend"), but with no asteroids in the game anymore, generate new asteroids
-- If the game state is "Spielend" but the player collected all 3 lambdas, the game state is set to "Gewonnen"
-- If the game state is "Spielend" but the player has no healthpoints anymore, the game state is set to "Verloren"
-- If the game state is either "Verloren" or "Gewonnen", a new world with reset objects is created, with the current of the player as new seed for generating random positions for asteroids and lambdas
-- Otherwise the world stays the same, for example if the game state is "Pause"
updateFunc :: StdGen -> Float -> World -> World
updateFunc generate _ world 
  | (world ^. status == Playing) && not (noLambdas world) && not (noAsteroids world) && not (lostGame world (world ^. player)) = collision world {
      _player = updatePlayer (updatePosition (updatePlayerSpeed world)),
      _asteroids = map (updateAsteroid (mkStdGen (floor (world ^. wSeed))) world)(world ^. asteroids),
      _lambda = updateLambda (mkStdGen (floor (world ^. wSeed))) world (world ^. player) (world ^. lambda),
      _wSeed = world ^. (player . pPosition . _1),
      _bullets = over (traverse . bPosition) (addCoordinates (0,10)) (world ^. bullets),
      _enemyBullets = map updateBullet (world ^. enemyBullets),
      _explosions = filterExplosions $ map (addScale . flip addTime 1) (world ^. explosions),
      _enemies = map (flip updateEnemy world . flip addTime 1) (world ^. enemies)}
  | (world ^. status == Playing) && not (noLambdas world) && not (lostGame world (world ^. player)) && noAsteroids world = world {
      _asteroids = Prelude.take 10 [initAsteroid $ mkStdGen ge | ge <- randoms (mkStdGen (floor (world ^. wSeed)))]
      }
  | (world ^. status == Playing) && noLambdas world = world & status .~ Won
  | (world ^. status == Playing) && lostGame world (world ^. player) = world & status .~ Lost
  | world ^. status == Lost = collision world {
    _player = Player {
      _pPosition = (0,- fromIntegral (world ^. screenSize . _2)/2 + 100),             
      _pHealth = 3,                       
      _pScore = 0,                       
      _pHits = 0,  
      _pLambdas = 0,                       
      _pVelocity = (0,0),
      _pWidth = 100
      }, 
    _lambda = Prelude.take 3 [initLambda $ mkStdGen g | g <- randoms (mkStdGen (floor (world ^. wSeed)))],
    _asteroids = Prelude.take 10 [initAsteroid $ mkStdGen ge | ge <- randoms (mkStdGen (floor (world ^. wSeed)))],
    _bullets = [],    
    _enemyBullets = [],
    _enemies = []}
  | world ^. status == Won = collision world {
    _player = Player {
      _pPosition = (0,- fromIntegral (world ^. screenSize . _2)/2 + 100),             
      _pHealth = 3,                     
      _pScore = 0,                      
      _pHits = 0, 
      _pLambdas = 0,                     
      _pVelocity = (0,0),
      _pWidth = 100
      }, 
    _lambda = Prelude.take 3 [initLambda $ mkStdGen g | g <- randoms (mkStdGen (floor (world ^. wSeed)))],
    _asteroids = Prelude.take 10 [initAsteroid $ mkStdGen ge | ge <- randoms (mkStdGen (floor (world ^. wSeed)))],
    _bullets = [],
    _enemyBullets = [],
    _enemies = []}
  | otherwise =  world 
  
-- scales the explosion according to the time elapsed (explosion gets bigger)
addScale :: Explosion -> Explosion
addScale explosion = explosion & scaling .~ (explosion^.eTime, explosion^.eTime)

-- filter explosions that have eTime < 10
filterExplosions :: [Explosion] -> [Explosion]
filterExplosions explosions = [e | e <- explosions, e^.eTime < 10]

-- update the position of a bullet by adding the velocity to the position of the bullet for every tick
updateBullet :: Bullet -> Bullet 
updateBullet bullet = bullet & bPosition %~ addCoordinates (bullet ^. bVelocity) 

-- update the position of a asteroid by adding the velocity to the position of the asteroid for every tick
-- checks if asteroid leafs screen and spawns it again at top of screen
updateAsteroid :: StdGen -> World -> Asteroid -> Asteroid
updateAsteroid gen world asteroid 
  | outOfScreen asteroid world = initAsteroid gen
  | otherwise = asteroid & (rotation +~ (asteroid ^. rotationSpeed)).( aPosition %~ addCoordinates (asteroid ^. aVelocity)) 

-- update the position of the lambdas by adding the velocity to the position of the lambdas for every tick
-- if the lambda leaves the bottom screen boundaries, a new list of lambdas is generated
-- if the score of the player is at 70, the first lambda starts
-- if the score of the player is at 180, the second lambda starts
-- if the score of the player is at 240, the third lambda starts
updateLambda :: StdGen -> World -> Player -> [Lambda] -> [Lambda]
updateLambda gen world player lambdas 
  | outOfScreen (head lambdas) world = Prelude.take 3 [initLambda $ mkStdGen g | g <- randoms gen]
  | player ^. pScore >= 70 && player ^. pLambdas == 0 = [head lambdas & lPosition %~ addCoordinates (head lambdas ^. lVelocity), lambdas !! 2, last lambdas]
  | player ^. pScore >= 180 && player ^. pLambdas == 1 = [head lambdas & lPosition %~ addCoordinates (head lambdas ^. lVelocity), last lambdas]
  | player ^. pScore >= 240 && player ^. pLambdas == 2 = [head lambdas & lPosition %~ addCoordinates (head lambdas ^. lVelocity)]
  | otherwise = lambdas
  
-- update the position of an enemy by adding the velocity to the position of the enemy for every tick
updateEnemy :: Enemy -> World -> Enemy
updateEnemy enemy = updateEnemySpeed (enemy & (enemyPosition %~ addCoordinates (enemy ^. enemyVelocity)))

-- update the position of a player by adding the velocity to the position of the player for every tick
updatePlayer :: Player -> Player
updatePlayer player = player & pPosition %~ addCoordinates (player ^. pVelocity) 

-- set the world status to "Spielend"
startGame :: World -> World 
startGame world = world & status .~ Playing

-- set the world status to "Pause"
pauseGame :: World -> World 
pauseGame world = world & status .~ Pause

-- Checks if the healthPoints of the player are smaller then 0
lostGame :: World -> Player -> Bool 
lostGame world player
  | player ^. pHealth <= 0 = True
  | otherwise = False

-- Checks if the list of asteroids is empty
noAsteroids :: World -> Bool
noAsteroids world 
  | Prelude.null (world ^. asteroids) = True
  | otherwise = False

-- Checks if the amount of collected lambdas of the player is 3
noLambdas :: World -> Bool
noLambdas world 
  | (world ^. (player . pLambdas)) == 3 = True
  | otherwise = False