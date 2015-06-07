import Graphics.Gloss

main = display 	(InWindow "Hello" (100,100) (100,100)) 
				white 
				(scale 0.3 0.3 $ text "13")
