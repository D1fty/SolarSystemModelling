module Bodies where

import Text.Printf ( printf )

-- The vector type
data Vec2 = Vec2 { x :: Double
                 , y :: Double
                 } deriving Show

-- Vector operators
(+.) :: Vec2 -> Vec2 -> Vec2
(+.) (Vec2 x y) (Vec2 x1 y1) = Vec2 (x + x1) (y + y1)

(-.) :: Vec2 -> Vec2 -> Vec2
(-.) (Vec2 x y) (Vec2 x1 y1) = Vec2 (x - x1) (y - y1)

(*.) :: Vec2 -> Vec2 -> Vec2
(*.) (Vec2 x y) (Vec2 x1 y1) = Vec2 (x * x1) (y * y1)

(.*) :: Vec2 -> Double -> Vec2
(.*) (Vec2 x y) z = Vec2 (x * z) (y * z)

(/.) :: Vec2 -> Vec2 -> Vec2
(/.) (Vec2 x 0) (Vec2 x1 0)  = error "Devide by Zero error in Vec2 devision"
(/.) (Vec2 x y) (Vec2 x1 y1) = Vec2 (x / x1) (y / y1)

(./) :: Vec2 -> Double -> Vec2
(./) (Vec2 x 0) 0  = error "Devide by Zero error in Vec2 devision"
(./) (Vec2 x y) z  = Vec2 (x / z) (y / z)

-- Vector functions
myLength :: Vec2 -> Double
myLength (Vec2 x y) = sqrt $ x^2 + y^2

myLength2 :: Vec2 -> Double
myLength2 (Vec2 x y) = x^2 + y^2

normalise :: Vec2 -> Vec2
normalise vec = vec ./ myLength vec

printVec2 :: Vec2 -> String
printVec2 (Vec2 x y) = printf "%+016.4f %+016.4F\n" x y

-- Body type
data Body = Body {  pos    :: Vec2
                 ,  vel    :: Vec2
                 ,  acc    :: Vec2
                 ,  ke     :: Double
                 ,  pe     :: Double
                 ,  mass   :: Double
                 ,  radius :: Double
                 ,  name   :: String 
                 }  deriving Show

-- Starting conditions for bodies
sun     = Body (Vec2 0       0) (Vec2 0  0        ) (Vec2 0 0) 0 0 1.989e30 696340.0 "Sun"

mercury = Body (Vec2 5.791e7 0) (Vec2 0 (-4138560)) (Vec2 0 0) 0 0  3.285e23 002439.7 "Mercury"

venus   = Body (Vec2 1.082e8 0) (Vec2 0 (-3024000)) (Vec2 0 0) 0 0  4.867e24 006051.8 "Venus"

earth   = Body (Vec2 1.496e8 0) (Vec2 0 (-2574720)) (Vec2 0 0) 0 0  5.972e24 006371.0 "Earth"

mars    = Body (Vec2 2.279e8 0) (Vec2 0 (-2082240)) (Vec2 0 0) 0 0  6.390e23 003389.5 "Mars"

jupiter = Body (Vec2 7.785e8 0) (Vec2 0 (-1131840)) (Vec2 0 0) 0 0  1.898e27 069911.0 "Jupiter"

saturn  = Body (Vec2 1.434e9 0) (Vec2 0 (-838080 )) (Vec2 0 0) 0 0  5.683e26 058232.0 "Saturn"

uranus  = Body (Vec2 2.871e9 0) (Vec2 0 (-587520 )) (Vec2 0 0) 0 0  8.681e25 025362.0 "Uranus"

neptune = Body (Vec2 4.495e9 0) (Vec2 0 (-466560 )) (Vec2 0 0) 0 0  1.024e26 024622.0 "Neptune"

pluto   = Body (Vec2 5.900e9 0) (Vec2 0 (-403488 )) (Vec2 0 0) 0 0  1.309e22 001188.3 "Pluto"

-- Generate bodies
startingBodies :: [Body]
startingBodies = [sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune, pluto]