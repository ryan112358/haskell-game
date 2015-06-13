module Game where

type World = Int
type Action = Int

applyActionToWorld :: Action -> World -> World
applyActionToWorld = (+)

--render :: World -> Picture
render = error "not yet implemented"
