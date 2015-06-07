import Graphics.Gloss
import Data.List

------------------------
--- Data Definitions ---
------------------------

type Health = Int
type Ammo = Int
-- type Point = (Float, Float)
type Velocity = (Float, Float)
type Time = Float

data Player = Player Point Velocity Health Ammo
	deriving (Eq)
	
data Bullet = Bullet Point Velocity

data Type = BoostHealth | BoostAmmo
	deriving (Enum, Eq)
	
data Food = Food Point Type

data World = World Player Player [Food] [Bullet]

----------------------
--- Game Constants ---
----------------------

gameWidth = 1000
gameHeight = 1000

spriteRad = 50
bulletRad = 10
foodRad = 30

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
		drawB (Bullet (x,y) _) = translate x y . color black $ circleSolid bulletRad
		drawF (Food (x,y) BoostHealth) = translate x y . color green $ circleSolid foodRad
		drawF (Food (x,y) BoostAmmo) = translate x y . color yellow $ circleSolid foodRad
		drawP (Player (x,y) _ _ _) col = translate x y . color col $ circleSolid spriteRad


------------
--- Main ---
------------

{-
main = play window 
			white 
			30 
			world0 
			render 
			handleEvent 
			update
-}