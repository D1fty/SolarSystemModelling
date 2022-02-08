{-
This fold necessary for simulation.

Basically it is a nested for loop that cumulatively adds accellerations and energies
as we fold through the list, and then outputs the answers in a list

Initially written and tested with Int/Doubles then incrementally expanded into the final algorith as per the comments
-}
-- Start by getting it working with a list of integers
testFold :: [Int] -> [Int]
testFold [] = []
testFold (x: t) = let (y: t2) = secondFold (x: t) []
                  in y : testFold t2

secondFold :: [Int] -> [Int] -> [Int]
secondFold [] _        = []
secondFold [x] list    = x: list
secondFold (x: y: t) list = let z = x + y
                            in secondFold (z:t) (list ++ [z])

-- Now have it + - on the left and right
testFold1 :: [Int] -> [Int]
testFold1 [] = []
testFold1 (x: t) = let (y: t2) = secondFold1 (x: t) []
                   in y : testFold1 t2

secondFold1 :: [Int] -> [Int] -> [Int]
secondFold1 [] _           = []
secondFold1 [x] list       = x: list
secondFold1 (x: y: t) list = let (a, b) = testAcc1 x y
                             in secondFold1 (a:t) (list ++ [b])

testAcc1 :: Int -> Int -> (Int, Int)
testAcc1 x y = let z = x + y                                        -- Accelleration equation here
                                                                    -- Enery equation here
               in  (x+z, y-z)

-- Extend to list of custom items
data Thing = Thing { accc :: Int
                   , kke  :: Double
                   , ppe  :: Double
                   } deriving Show

testFold2 :: [Thing] -> [Thing]
testFold2 [] = []
testFold2 (x: t) = let (y: t2) = secondFold2 (x: t) []
                   in y : testFold2 t2

secondFold2 :: [Thing] -> [Thing] -> [Thing]
secondFold2 [] _        = []
secondFold2 [x] list    = x: list
secondFold2 (x: y: t) list = let (a, b) = testAcc2 x y
                             in secondFold2 (a:t) (list ++ [b])     -- Total energy conversion here

testAcc2 :: Thing -> Thing -> (Thing, Thing)
testAcc2 (Thing x _ _) (Thing y _ _) = let z = x + y                -- Accelleration equation here
                                                                    -- Potential Enery equation here
                                       in  (Thing (x+z) 0 0, Thing (y-z) 0 0)

-- Add in Energy calculations
testFold3 :: [Thing] -> [Thing]
testFold3 [] = []
testFold3 (x: t) = let (y: t2) = secondFold3 (x: t) []
                   in y : testFold3 t2

secondFold3 :: [Thing] -> [Thing] -> [Thing]
secondFold3 [] _        = []
secondFold3 [Thing a _ p] list    = Thing a 0 p: list              
secondFold3 (x: y: t) list = let (a, b) = testAcc3 x y
                             in secondFold3 (a:t) (list ++ [b])     -- Total energy conversion here 

testAcc3 :: Thing -> Thing -> (Thing, Thing)
testAcc3 (Thing x _ pe1) (Thing y _ pe2) = let z = x + y            -- Accelleration equation here
                                               pe3 = pe1 + pe2      -- Potential Energy equation here
                                           in  (Thing (x+z) 0 pe3, Thing (y-z) 0 pe3)

-- Return only the accellerations and energy sums
-- From here if you see '15' it gets replaced with (0.5 * mass * myLength vel) which is the kinetic energy 
-- of the planet, calculated only once it is completely finished in the nested for loop
testFold4 :: [Thing] -> ([Int], Double)
testFold4 [] = error "dont get to here"
testFold4 [Thing acl k p ] = ([acl], 15 + p)                        
testFold4 (x: t) = let (y: t2) = secondFold4 (x: t) []
                       (accelleration, energy) = testFold4 t2
                   in  (accc y : accelleration, ppe y + kke y + energy) 

secondFold4 :: [Thing] -> [Thing] -> [Thing]
secondFold4 [] _        = []
secondFold4 [Thing a _ p] list    = Thing a 15 p: list          
secondFold4 (x: y: t) list = let (a, b) = testAcc4 x y
                             in secondFold4 (a:t) (list ++ [b])

testAcc4 :: Thing -> Thing -> (Thing, Thing)
testAcc4 (Thing x _ pe1) (Thing y _ pe2) = let z = x + y            
                                               pe3 = pe1 + pe2      
                                           in  (Thing (x+z) 0 pe3, Thing (y-z) 0 pe3)