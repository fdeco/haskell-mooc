module Main where

import Data.List
import System.Random

maxTurns = 5
colours = "RGBYW"

skewed :: String -> String -> Int -> Int
skewed xs ys (-1) =
  skewed (sort xs) (sort ys) 0
skewed [] _ acc = acc
skewed _ [] acc = acc
skewed (x:xs) (y:ys) acc
  | x < y = skewed xs (y:ys) acc
  | x > y = skewed (x:xs) ys acc
  | otherwise = skewed xs ys (acc + 1)

check :: String -> String -> (Int,Int)
check attempt secret =
  let
    (corr, miss) = partition (uncurry (==)) (zip secret attempt)
  in
  (length corr, uncurry skewed (unzip miss) (-1))

turn :: [String] -> String -> IO ()
turn [] secret = mkguess [] secret
turn attempts@(x:_) secret
  | x == secret = putStrLn "congrats.. secret found"
  | length attempts == maxTurns = putStrLn ("sorry .. secret was " ++ secret)
  | otherwise = mkguess attempts secret

mkguess :: [String] -> String -> IO ()
mkguess attempts secret =
  do putStrLn ("+-----------------+------+--------+\n" ++
               "| [?] [?] [?] [?] | CORR | SKEWED |\n" ++
               "+-----------------+------+--------+")
     putStrLn (concat [attemptStr att secret | att <- reverse attempts])
     putStrLn  "+---------------------------------+"
     q <- getLine
     turn (take 4 q : attempts) secret

attemptStr :: String -> String -> String
attemptStr attempt secret =
  let
    (c, s) = check attempt secret
  in
  "|" ++ concat [ "  "++[x]++" "| x <- attempt] ++ " " ++
  "|   " ++ show c ++ "  " ++
  "|   " ++ show s ++ "    |\n"

genRandColour :: IO String
genRandColour = do
  gen <- getStdGen
  return [ colours !! r | r <- take 4 (randomRs (0, length colours - 1) gen)]


main :: IO ()
main =
  do putStrLn "Guess the 4 colours"
     putStrLn ("These are the colours you can choose from: " ++ intersperse ',' colours)
     secret <- genRandColour
     turn [] secret
