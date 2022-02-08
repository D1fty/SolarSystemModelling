module Main where

import Bodies
import System.Process
import Data.Time.Clock

-- Simulation Type
data Simulation = Simulation { planets       :: [Body]
                             , initialEnergy :: Double
                             , currentEnergy :: Double
                             , step          :: Int
                             }

-- Simulation variables
maxstep :: Int
maxstep = 365

outName :: String
outName = "output.dat"

gc :: Double
gc = 6.67408e-11 * (86400.0*86400.0) / (1000.0*1000.0*1000.0)

min2 :: Double
min2 = 2.0

dt :: Integer
dt = 1

-- Main
main :: IO ()
main = do system "cls"
          putStrLn "--------------------------------------------------------------"
          putStrLn "                      Solar System Simulation"
          putStrLn "                          Midpoint Method"
          putStrLn "--------------------------------------------------------------"
          putStrLn "Running simulation..."
          startTime     <- getCurrentTime
          finishedSim   <- runSimulation (Simulation startingBodies 0 0 0)
          printFinishedSim finishedSim
          endTime       <- getCurrentTime
          putStrLn "Finished simulation..."
          putStrLn "Calculating time taken..."
          putStr   "Time taken: "
          print $ diffUTCTime endTime startTime
      
-- Simulation
runSimulation :: Simulation -> IO Simulation
runSimulation sim
 | step sim < maxstep - 1 = let (Simulation planets initialEnergy currentEnergy step) = stepForward sim
                            in do printSim initialEnergy currentEnergy step
                                  runSimulation (Simulation planets initialEnergy  0 (step + 1))
 | otherwise              = let (Simulation planets initialEnergy currentEnergy step) = stepForward sim
                             in do printSim initialEnergy currentEnergy step
                                   return (Simulation planets initialEnergy currentEnergy step) 

stepForward :: Simulation -> Simulation
stepForward (Simulation bods _ _ 0)                =  let accellerations = calculateMidAcc bods
                                                          newBods        = midMoveBodies bods accellerations
                                                          (acc, energy)  = calculateAccEn newBods
                                                          velocities     = map vel newBods
                                                      in  Simulation (moveBodies bods acc velocities) energy energy 0
stepForward (Simulation bods initialEnergy _ step) =  let accellerations = calculateMidAcc bods
                                                          newBods        = midMoveBodies bods accellerations
                                                          (acc, energy)  = calculateAccEn newBods
                                                          velocities     = map vel newBods
                                                      in  Simulation (moveBodies bods acc velocities) initialEnergy energy step

moveBodies :: [Body] -> [Vec2] -> [Vec2] -> [Body]
moveBodies _ [] _ = error "Uneven lists in moveBodies"
moveBodies [] _ _ = error "Uneven lists in moveBodies"
moveBodies _ _ [] = error "Uneven lists in moveBodies"
moveBodies [Body pos _ _ _ _ mass radius name] [acc] [vel]                      = let vel2 = vel +. acc
                                                                                      pos2 = pos +. vel
                                                                                  in [Body pos2 vel2 (Vec2 0 0) 0 0 mass radius name]
moveBodies (Body pos _ _ _ _ mass radius name: bTail) (acc: aTail) (vel: vTail) = let vel2 = vel +. acc
                                                                                      pos2 = pos +. vel
                                                                                  in  Body pos2 vel2 (Vec2 0 0) 0 0 mass radius name : moveBodies bTail aTail vTail

midMoveBodies :: [Body] -> [Vec2] -> [Body]
midMoveBodies _ [] = error "Uneven lists in moveBodies"
midMoveBodies [] _ = error "Uneven lists in moveBodies"
midMoveBodies [Body pos vel _ _ _ mass radius name] [acc]               = let newPos = (pos +. (vel .* 0.5))
                                                                              newVel = (vel +. (acc .* 0.5))
                                                                          in [Body newPos newVel (Vec2 0 0) 0 0 mass radius name]
midMoveBodies (Body pos vel _ _ _ mass radius name: bTail) (acc: aTail) = let newPos = (pos +. (vel .* 0.5))
                                                                              newVel = (vel +. (acc .* 0.5))
                                                                          in  Body newPos newVel (Vec2 0 0) 0 0 mass radius name : midMoveBodies bTail aTail

-- Midpoint accellerations
calculateMidAcc :: [Body] -> [Vec2]
calculateMidAcc []                       = error "calculateAccEn called on empty list"
calculateMidAcc [Body _ _ acc _ _ _ _ _] = [acc]
calculateMidAcc (b: t) = let (b1: t2)   = accMidFold (b: t) []
                         in  acc b1: calculateMidAcc t2

accMidFold :: [Body] -> [Body] -> [Body]
accMidFold [] _                            = error "accFold called on empty list"
accMidFold [b] list   = b: list
accMidFold (b1: b2: t) list                = let (b3, b4) = twoBodyMidAcc b1 b2
                                             in  accMidFold (b3: t) (list ++ [b4])

twoBodyMidAcc :: Body -> Body -> (Body, Body)
twoBodyMidAcc (Body pos1 _ acc1 _ _ mass1 radius1 _) (Body pos2 _ acc2 _ _ mass2 radius2 _)
 = let dx = pos1 -. pos2
       d2 = myLength2 dx
   in if   d2 > min2 
      then let u    = normalise dx
               f    = (-gc) * mass1 * mass2 / d2              
               acc3 = acc1 +. (u .* f ./ mass1)
               acc4 = acc2 -. (u .* f ./ mass2)
           in (Body pos1 (Vec2 0 0) acc3 0 0 mass1 radius1 "", Body pos2 (Vec2 0 0) acc4 0 0 mass2 radius2 "") 
      else    (Body pos1 (Vec2 0 0) acc1 0 0 mass1 radius1 "", Body pos2 (Vec2 0 0) acc2 0 0 mass2 radius2 "")

-- Final calculation
calculateAccEn :: [Body] -> ([Vec2], Double)
calculateAccEn []                                   = error "calculateAccEn called on empty list"
calculateAccEn [Body _ vel acc _ pe mass _ _]       = ([acc], (0.5 * mass * (myLength vel^2)) + pe)
calculateAccEn (b: t) = let (b1: t2)                = accFold (b: t) []
                            (accelleration, energy) = calculateAccEn t2
                        in  (acc b1: accelleration, pe b1 + ke b1 + energy)

accFold :: [Body] -> [Body] -> [Body]
accFold [] _ = error "accFold called on empty list"
accFold [Body pos vel acc ke pe mass radius name] list = Body pos vel acc (0.5 * mass * (myLength vel^2)) pe mass radius name: list
accFold (b1: b2: t) list = let (b3, b4) = twoBodyAcc b1 b2
                           in  accFold (b3: t) (list ++ [b4])

twoBodyAcc :: Body -> Body -> (Body, Body)
twoBodyAcc (Body pos1 vel1 acc1 _ pe1 mass1 radius1 name1) (Body pos2 vel2 acc2 _ pe2 mass2 radius2 name2)
 = let dx = pos1 -. pos2
       d2 = myLength2 dx
   in if   d2 > min2 
      then let u    = normalise dx
               f    = (-gc) * mass1 * mass2 / d2              
               acc3 = acc1 +. (u .* f ./ mass1)
               acc4 = acc2 -. (u .* f ./ mass2)
               uu   = ((-gc) * mass1 * mass2 / sqrt d2) * 0.5
               pe3  = pe1 + uu
               pe4  = pe2 + uu
           in (Body pos1 vel1 acc3 0 pe3 mass1 radius1 name1, Body pos2 vel2 acc4 0 pe4 mass2 radius2 name2) 
      else    (Body pos1 vel1 acc1 0 pe1 mass1 radius1 name1, Body pos2 vel2 acc2 0 pe2 mass2 radius2 name2)

-- Print to screen
printSim :: Double -> Double -> Int -> IO ()
printSim initialEnergy currentEnergy step = do putStr "Step: "
                                               print step
                                               putStr "Initial Energy: "
                                               print initialEnergy 
                                               putStr "Current Energy: "
                                               print currentEnergy 
                                               return ()

-- Print to file
printFinishedSim :: Simulation -> IO ()
printFinishedSim (Simulation bods initialEnergy currentEnergy _) = do writeFile  outName ("Total Initial Energy: " ++ show initialEnergy ++ "\n")
                                                                      appendFile outName ("Total Current Energy: " ++ show currentEnergy  ++ "\n")
                                                                      appendFile outName ("Energy Change: " ++ show ((initialEnergy - currentEnergy) / currentEnergy) ++ "%"  ++ "\n\n")
                                                                      appendFile outName "Planetary positions:\n"
                                                                      printBods bods
                                                                      return ()

printBods :: [Body] -> IO ()
printBods []          = return ()
printBods (head:tail) = do appendFile outName (name head ++ ": " ++ insertSpaces (10 - length (name head)) "" ++ printVec2 (pos head))
                           printBods tail

insertSpaces :: Int -> String -> String
insertSpaces 0 spaces = spaces  
insertSpaces x spaces = " " ++ insertSpaces (x - 1) spaces