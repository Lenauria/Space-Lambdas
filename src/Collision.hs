module Collision where

import Datastructure
import Control.Lens
import Data.Set.Ordered as Set
import Graphics.Gloss.Interface.IO.Game
import System.Random


{- Check for collision of the player with the right and left boundary:
    Gloss coordinate system has (0,0) at the center of the window 
        (so the "negative" half of screen width is at left boundary, the "positive" half of screen width is at right boundary). 

        Gloss:
                           screenheight/2
                         ___________________
                        |                   |
                        |                   |
        - screenwidth/2 |       (0,0)       | screenwidth/2
                        |                   |
                        |___________________|
                          - screenheight/2

    inRightBoundary returns True, if the player is inside the world
                    return False, if the player hits the right boundary

    inLeftBoundary  returns True, if the player is inside the world
                    return False, if the player hits the left boundary
    -}

inRightBoundary :: Entity a => a -> World -> Bool
inRightBoundary entity world
    | entity ^. xPosition >= (fromIntegral screenWidth/2) - (getWidth entity/2) = False
    | otherwise = True
        where screenWidth = world ^. screenSize . _1

inLeftBoundary :: Entity a => a -> World -> Bool
inLeftBoundary entity world
    | entity ^. xPosition <= - (fromIntegral screenWidth)/2 + (getWidth entity/2) = False
    | otherwise = True
        where screenWidth = world ^. screenSize . _1

outOfScreen :: Entity a => a -> World -> Bool
outOfScreen asteroid world
    | asteroid ^. yPosition <= -(fromIntegral screenHeight/2) = True
    | otherwise = False
        where screenHeight = world ^. screenSize . _2
  

{- elemAt returns the "Maybe element" for a specific index. If there's no element at the index, it returns Nothing
    if the key at the index 0 is the right arrow key, then set player speed to 10 (go right)
    if the key at the index 0 is the left arrow key, then set player speed to -10 (go left)
    if neither left nor right arrow key pressed, then set player speed to 0 (stand still)
-}
updatePlayerSpeed :: World -> World
updatePlayerSpeed world
    | elemAt (_keysPressed world) 0 == Just KeyRight && inRightBoundary (world ^. player) world = world & (player.pVelocity) .~ (3,0)
    | elemAt (_keysPressed world) 0 == Just KeyLeft  && inLeftBoundary  (world ^. player) world = world & (player.pVelocity) .~ (-3,0)
    | otherwise                                                                                   = world & (player.pVelocity) .~ (0,0)

updateAsteroidSpeed :: Asteroid -> World -> Asteroid
updateAsteroidSpeed asteroid world
    | not (outOfScreen asteroid world)   = asteroid & aVelocity .~ (0,0)
    | otherwise                             = asteroid & aVelocity .~ (0,-5)
    
{- enemy goes right and left all the time until it's defeated
if enemy is on right boundary -> turn left (Velocity (-2,0))
if enemy is on left boundary  -> turn right (Velocity (2,0)) -}
updateEnemySpeed :: Enemy -> World -> Enemy
updateEnemySpeed enemy world 
    | (not (inRightBoundary enemy world)) && isGoingRight enemy  = enemy & enemyVelocity .~ (-2,0)
    | (not (inLeftBoundary enemy world)) && isGoingLeft  enemy  = enemy & enemyVelocity .~ (2,0)
    | otherwise = enemy 
        where 
          isGoingRight entity   = entity ^. velocity . _1 >= 0  
          isGoingLeft entity    = entity ^. velocity . _1 <= 0  


{- Check collision between entities by checking if the distance ist less than the sum of radii -}
collisionBetween :: (Entity a, Entity b) => World -> a -> b  -> Bool
collisionBetween world ent1 ent2 = distance <= sumOfRadii
    where 
        distance = distanceBetween (getPosition ent1) (getPosition ent2)
        sumOfRadii = (getWidth ent1)/2 + (getWidth ent2)/2

{- distance between two points -}
distanceBetween (x1, y1) (x2, y2) = sqrt (x * x  +  y * y)
    where
        x = x1 - x2
        y = y1 - y2

{- actualize world according to collisions -}
collision :: World -> World
collision world = world {
    _bullets        = newBullets,
    _enemyBullets   = newEnemyBullets,
    _asteroids      = newAsteroids,
    _player         = newPlayer,
    _lambda         = newLambda,
    _explosions     = world ^. explosions ++ newExplosions, -- the explosions of the previous ticks are still relevant (at least some of them), because an explosion exists a few ticks and is scaled according to its lifetime
    _enemies        = newEnemies
    }
    where
        {- 1. The first list comprehension (asteroidPlayerTuples = ...) returns a list with tuples [(Score, Healthpoints, Asteroid)]
            Asteroid is the updated asteroid, that means the asteroid either with 
                hits == 0 (if it didn't get hit so far)
                hits == 1 (if it got hit by a bullet once)
                hits == 2 (if it got hit by a bullet twice or got hit by the player)
            Score == 10, if an asteroid got hit by a bullet, otherwise 0 (gets added to the players score)
            Healthpoints == 1, if an asteroid got hit by the player (gets subtracted from the players healthpoints)
        
        2. newAsteroids takes all the asteroids that still exist (getHits modifiedAsteroid < 2). 
            An asteroid with hits >= 2 is destroyed.

        3. newPlayer counts all scores and adds them to the players scores and counts all hp to remove from the players hp.

        4. bulletExplosionTuples is a list of the actualized bullets of form [(Bool, Int)]. 
            If a bullet hits something relevant, the Bool-Flag turns to True to mark that there must be an explosion.
            Also the hits of the bullet are incremented by 1

        5. newBullets takes all the bullets, that still should exist. These are all the bullets with hits == 0 (getHits modifiedBullet == 0)
            
        6. newExplosions generates a list of explosions at the positions where the dedicated bullets got destroyed.
            Also if an asteroid collides with the player there is generated an explosion

        7. newEnemies is the modified list of enemies (modified hits). If players score is a multiple of 50, a new enemy appears.
            -> in our game the max. count of enemies is 1, so only if enemy list is empty, a new enemy is generated
        
        8. enemyBulletExplosionTuples on the lines of bulletExplosionTuples

        9. newEnemyBullets on the lines of newBullets plus a new enemy bullet every 100 ticks

        10. lambdaPlayerTuples is the list of actualizes lambdas of the form [(Int, Lambda)]. 
            If a lambda is collected by the player the hits are incremented by 1 and the Int flag is set to 1 (that is the number that is added to the players lambdaCount).

        11. newLambda takes all the lambdas, that still should exist (all the lamdas with hits == 0) -}
        asteroidPlayerTuples        = [ asteroidCollides asteroid world | asteroid <- world ^. asteroids ]
        newAsteroids                = [ modifiedAsteroid | (_, _, modifiedAsteroid) <- asteroidPlayerTuples, getHits modifiedAsteroid < 2 ]
        newPlayer                   = playerCollides (addLambda (remHealth (addScore (_player world) sumOfScores) sumOfHPs) sumOfLambdas) world
            where 
                sumOfScores  = sum [ score | (score, _, _) <- asteroidPlayerTuples]
                sumOfHPs     = sum [hp | (_, hp, _) <- asteroidPlayerTuples ]
                sumOfLambdas = sum [ lambdasGotten | (lambdasGotten, _) <- lambdaPlayerTuples]
        bulletExplosionTuples       = [ bulletCollides bullet world | bullet <- world ^. bullets ]
        newBullets                  = [ modifiedBullet | (_, modifiedBullet) <- bulletExplosionTuples, getHits modifiedBullet == 0 ]
        newExplosions               = [ makeBulletExplosion world (getPosition bullet) | (explode, bullet) <- bulletExplosionTuples, explode] ++ [makeAsteroidExplosion world (getPosition asteroid) | (_, hp, asteroid) <- asteroidPlayerTuples, hp == 1] ++ [ makeAsteroidExplosion world (getPosition enemyBullet) | (explode, enemyBullet) <- enemyBulletExplosionTuples, explode]
        bulletEnemyTuples           = [ enemyCollides enemy world | enemy <- world ^. enemies ]
        newEnemies              
            | Prelude.null (world ^. enemies) && (newPlayer ^. pScore) `mod` 40 == 0 && (newPlayer ^. pScore) /= 0    = initEnemy (_enemies world) world
            | otherwise                                                                                             = [ modifiedEnemy | (_, modifiedEnemy) <- bulletEnemyTuples, getHits modifiedEnemy < 5 ]
        enemyBulletExplosionTuples  = [bulletCollides bullet world | bullet <- world ^. enemyBullets]
        newEnemyBullets             = [ modifiedEnemyBullet | (_, modifiedEnemyBullet) <- enemyBulletExplosionTuples, getHits modifiedEnemyBullet == 0 ] ++ [ makeEnemyBullet enemy world | enemy <- _enemies world, floor (getTime enemy) `mod` 100 == 0]
        lambdaPlayerTuples          = [lambdaCollides lambda world | lambda <- world ^. lambda]
        newLambda                   = [ modifiedLambda | (_, modifiedLambda) <- lambdaPlayerTuples, getHits modifiedLambda < 1]
        

{- all possible collisions:
player <-> asteroid --> handled in asteroidCollision
player <-> enemyBullet --> handled in
asteroid <-> playerBullet --> handled in asteroidCollision
enemy <-> playerBullet

                          player
                        /       \
            enemyBullet         asteroid
                                  \
                                 playerBullet
                                    \
                                    enemy
-}


{- playerCollides checks is the player has collided with an enemy bullet. If so, remove 1 hp from player. -}
playerCollides :: Player -> World -> Player
playerCollides player world
    | any (collisionBetween world player) (_enemyBullets world) = remHealth player 1
    | otherwise                                                 = player

{- asteroid collisions: returns (score, health, asteroid)
    if asteroid hits player bullet, it gets hits+1
        if hits was 0 the velocity of asteroid decreases
        if hits was 1, the player gets score+10, because asteroid got destroyed (flag score is set to 10)
    if asteroid hits player, it gets hits=2, so it is destroyed and player gets hp-1 (flag hp ist set to 1) -}
asteroidCollides :: Asteroid -> World -> (Score, Healthpoints, Asteroid)
asteroidCollides asteroid world
    | any (collisionBetween world asteroid) (_bullets world)    = case (getHits asteroid) of -- wurde asteroid von einem bullet getrofen?
                                                                    0 -> (0, 0, addHit $ asteroid `setVelocity` (0,-1)) -- asteroid lebt noch, keine scores, kein health abzug, asteroid bekommt hits+1
                                                                    1 -> (10, 0, addHit asteroid) -- asteroid gestorben, 10 scores, kein health abzug, asteroid bekommt hits+1
    | collisionBetween world asteroid (_player world)           = (0, 1, setHit asteroid 2) -- asteroid stirbt durch kollision mit player -> setze hits sofort auf 2 (asteroid stirbt), player bekommt -1 hp (-1 heart)
    | otherwise                                                 = (0, 0, asteroid) -- no hit


{- bullet collisions:
    if a playerbullet hits an asteroid or an enemy, the bullet is destroyed (scores for the player are already handled in asteroidCollides and enemyCollides)
    if an enemybullet hits the player, the bullet is destroyed (healthpoints are handled in playerCollides) -}
bulletCollides :: Bullet -> World -> (Bool, Bullet)
bulletCollides bullet world 
    | any (collisionBetween world bullet) (_asteroids world) && not (isEnemyBullet bullet) || any (collisionBetween world bullet) (_enemies world) && (isEnemyBullet bullet == False) || (collisionBetween world bullet (_player world)) && isEnemyBullet bullet = (True, addHit bullet)
    | otherwise = (False, bullet)
        where
            isEnemyBullet bullet -- check if bullet ist enemybullet or playerbullet
                | bullet ^. velocity . _2 < 0   = True  -- enemybullet
                | otherwise                     = False -- playerbullet


enemyCollides :: Enemy -> World -> (Score, Enemy) 
enemyCollides enemy world
    | any (collisionBetween world enemy) (_bullets world)   = case (getHits enemy) of -- how often was enemy hit already?
                                                                0 -> (0, addHit enemy) -- enemy still alive, no scores, enemy gets hits+1
                                                                1 -> (0, addHit enemy) -- enemy still alive, no scores, enemy gets hits+1
                                                                2 -> (0, addHit enemy) -- enemy still alive, no scores, enemy gets hits+1
                                                                3 -> (0, addHit enemy) -- enemy still alive, no scores, enemy gets hits+1
                                                                4 -> (50, addHit enemy) -- enemy dies, 50 scores for player, enemy gets hits+1
    | otherwise                                             = (0, enemy) -- no hit



lambdaCollides :: Lambda -> World -> (Int, Lambda)
lambdaCollides lambda world
    | collisionBetween world lambda (_player world)   = (1, setHit lambda 1)
    | otherwise                                         = (0, lambda)