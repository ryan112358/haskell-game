{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import System.Random
import Data.Serialize

import GHC.Generics

------------------------
--- Data Definitions ---
------------------------

type Health = Int
type Ammo = Int
type Velocity = (Float, Float)
type Time = Float

data Player = Player Point Velocity Health Ammo deriving (Generic, Eq, Show)
data Bullet = Bullet Point Velocity deriving (Generic)
data Type = BoostHealth | BoostAmmo deriving (Generic, Enum, Eq)
data Food = Food Point Type deriving (Generic)
data World = World Player Player [Food] [Bullet] deriving (Generic)

instance Serialize Player 
instance Serialize Bullet 
instance Serialize Type 
instance Serialize Food 
instance Serialize World 

deriving instance Generic MouseButton
deriving instance Generic Key
deriving instance Generic SpecialKey
deriving instance Generic KeyState 
deriving instance Generic Modifiers 
deriving instance Generic Event 

instance Serialize MouseButton
instance Serialize Key
instance Serialize SpecialKey
instance Serialize KeyState
instance Serialize Modifiers
instance Serialize Event

----------------------
--- Game Constants ---
----------------------

gameWidth, gameHeight :: Int
gameWidth = 1000
gameHeight = 1000

spriteRad = 30
bulletRad = 10
foodRad = 30

initialPlayer pos = Player pos (0, 0) 100 10
initialFood = map (flip Food BoostAmmo) . take 25 . 
				zip (randomRs (100,900) (mkStdGen 111)) $ 
				randomRs (100,900) (mkStdGen 999)

initialWorld = World (initialPlayer (100, 100)) (initialPlayer (900, 900)) initialFood [] 

------------------------
--- Simple Functions ---
------------------------

touching :: Point -> Float -> Point -> Float -> Bool
touching (x1,y1) r1 (x2,y2) r2 = d < r1 + r2
    where d = sqrt $ (x1-x2)^2 + (y1-y2)^2
    
move :: Point -> Velocity -> Time -> Point
move (x,y) (vx, vy) t = (x+t*vx, y+t*vy)

    
------------------
--- Game Logic ---
------------------

collide :: Player -> [Bullet] -> (Player, [Bullet])
collide (Player p v h a) bs = (Player p v (h - length hit) a, miss)
    where 
        (hit, miss) = partition hitting bs
        hitting (Bullet q _) = touching p spriteRad q bulletRad 
        
gather :: Player -> [Food] -> (Player, [Food])
gather player@(Player p _ _ _) fs = (foldr pick player hit, miss)
    where
        (hit, miss) = partition hitting fs
        hitting (Food q _) = touching p spriteRad q foodRad 
        pick (Food _ BoostHealth) (Player p v h a) = Player p v (h+1) a
        pick (Food _ BoostAmmo) (Player p v h a) = Player p v h (a+1)

update :: Time -> World -> World
update t (World p1 p2 food bullets) = 
    World (moveP p1'') (moveP p2'') f2 (map moveB b2)
    where
        (p1', b1) = collide p1 bullets
        (p2', b2) = collide p2 b1
        (p1'', f1) = gather p1' food
        (p2'', f2) = gather p2' f1
        moveB (Bullet p v) = Bullet (move p v t) v
        moveP (Player p v h a) = Player (move p v t) v h a


-----------------
--- Rendering ---
-----------------

render :: World -> Picture
render (World p1 p2 food bullets) = pictures pics
    where   
        pics = [drawP p1 blue, drawP p2 red] ++ map drawB bullets ++ map drawF food
        drawB (Bullet (x,y) _)          = translate x y . color black   $ circleSolid bulletRad
        drawF (Food (x,y) BoostHealth)  = translate x y . color green   $ circleSolid foodRad
        drawF (Food (x,y) BoostAmmo)    = translate x y . color yellow  $ circleSolid foodRad
        drawP (Player (x,y) _ _ _) col  = translate x y . color col     $ circleSolid spriteRad

-------------------
-- Event Handler --
-------------------

movePlayer KeyUp    (Player p (vx, vy) h a) = Player p (vx, vy+1) h a
movePlayer KeyDown  (Player p (vx, vy) h a) = Player p (vx, vy-1) h a
movePlayer KeyLeft  (Player p (vx, vy) h a) = Player p (vx-1, vy) h a
movePlayer KeyRight (Player p (vx, vy) h a) = Player p (vx+1, vy) h a
movePlayer _ player = player
        
handleKey 1 key (World p1 p2 food bullets) = World (movePlayer key p1) p2 food bullets  
handleKey 2 key (World p1 p2 food bullets) = World p1 (movePlayer key p2) food bullets  
handleKey _ key w = w

handleMouse plyr LeftButton (x,y) (World p1 p2 food bullets)
    = World p1 p2 food (bullet:bullets) where 
        (Player (x',y') _ _ ammo) = if plyr == 1 then p1 else p2
        bullet = Bullet (x',y') (10*vx, 10*vy)
        (vx, vy) = let d = sqrt $ (x-x')^2+(y-y')^2 in ((x'-x)/d, (y'-y)/d)
handleMouse _ _ _ w = w
        
handleEvent :: Int -> Event -> World -> World
handleEvent plyr (EventKey key Down _ posn) = case key of
    (SpecialKey k) -> handleKey plyr k
    (MouseButton m) -> handleMouse plyr m posn
handleEvent _ _ = id

