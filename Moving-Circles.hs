import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import Debug.Trace
import System.Random

type Posn = (Float, Float)
type Velocity = (Float, Float)
type Radius = Float

data Unit = Unit Float Posn Velocity
	deriving (Eq, Ord, Show, Read)
	
nullUnit = Unit 0 (0,0) (0,0)

data World = World Unit [Unit]
	deriving (Eq, Ord, Show, Read)
	
instance Random Unit where
	randomR = undefined
	random g = (Unit rd (x,y) (4*vx, 4*vy),g5) where
		(x,g1) = randomR (-width/2,width/2) g
		(y,g2) = randomR (0,height) g1
		(vx,g3) = randomR (-20,20) g2
		(vy,g4) = randomR (-20,20) g3
		(rd,g5) = randomR (5,25) g4
	
merge :: Unit -> Unit -> (Unit, Unit)
merge (Unit r1 p1@(x1,y1) v1) (Unit r2 p2@(x2,y2) v2)
	| r1 > r2 + d 	= (Unit (sqrt a) p1 v1, nullUnit)
	| r1 + d <= r2 	= (nullUnit, Unit (sqrt a) p2 v2) 
	| r1 > r2 		= (Unit r1' p1 v1, Unit r2' p2 v2)
	| otherwise 	= (Unit r2' p1 v1, Unit r1' p2 v2)
	where
		r1' = (d + (sqrt $ 2*a-d^2))/2
		r2' = abs (d - r1')
		d = sqrt $ (x1-x2)^2 + (y1-y2)^2
		a = r1^2 + r2^2

touching :: Unit -> Unit -> Bool
touching (Unit r1 (x1,y1) _) (Unit r2 (x2,y2) _)
		= (x1-x2)^2 + (y1-y2)^2 < (r1+r2)^2
	
placeUnit :: Unit -> Picture
placeUnit (Unit r (x,y) _) = translate x y . circle $ r
	
render :: World -> Picture
render (World m es) = pictures (good:bad) where 
	good = color blue $ placeUnit m
	bad = map (color red . placeUnit) es

handleKey :: Key -> World -> World
handleKey (SpecialKey key) (World (Unit r p (vx,vy)) es)
	| key == KeyUp 	 = next (vx, vy+10)
	| key == KeyDown = next (vx, vy-10)
	| key == KeyLeft = next (vx-10, vy)
	| key == KeyRight = next (vx+10, vy)
	where next v = World (Unit r p v) es

handleEvent :: Event -> World -> World
handleEvent (EventKey key Down _ _) = handleKey key
handleEvent _ = id

moveUnit :: Float -> Unit -> Unit
moveUnit t (Unit r (x,y) (vx, vy))
	| -width > 2*x && vx < 0 	= Unit r p' (-vx,vy)
	| 2*x > width && vx > 0 	= Unit r p' (-vx,vy)
	| -height > 2*y && vy < 0 	= Unit r p' (vx,-vy)
	| 2*y > height && vy > 0 	= Unit r p' (vx,-vy)
	| otherwise		 			= Unit r p' (vx,vy)
	where p' = (x+t*vx,y+t*vy)

update :: Float -> World -> World
update t (World m es) = uncurry World $ foldr f (m',[]) es'
	where 
		m' 	= moveUnit t m 
		es' = map (moveUnit t) . filter (/=nullUnit) $ es 
		f a b@(x, ys) = if (touching x a) then fmap (:ys) (merge x a) else fmap (a:) b
	
--------------------
-- Game Constants --
--------------------
(width, height) = (400,400) :: (Float, Float)
window = InWindow "Moving Circles" (floor width,floor height) (0,0)
randWorld gen = World (Unit 10 (0,0) (0,0)) (take 5 $ randoms gen)
	
main = do 
	gen <- getStdGen
	let world0 = randWorld gen
	play window 
			white 
			30 
			world0 
			render 
			handleEvent 
			update