module Main where

keys :: [String]
keys = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

keyToIdx :: String -> Int
-- A
keyToIdx "A" = 0
keyToIdx "A#" = 1
keyToIdx "Bb" = 1
-- B
keyToIdx "B" = 2
keyToIdx "Cb" = 2
-- C
keyToIdx "C" = 3
keyToIdx "B#" = 3
keyToIdx "C#" = 4
keyToIdx "Db" = 4
-- D
keyToIdx "D" = 5
keyToIdx "D#" = 6
keyToIdx "Eb" = 6
-- E
keyToIdx "E" = 7
keyToIdx "Fb" = 7
-- F
keyToIdx "F" = 8
keyToIdx "E#" = 8
keyToIdx "F#" = 9
keyToIdx "Gb" = 9
-- G
keyToIdx "G" = 10
keyToIdx "G#" = 11
keyToIdx "Ab" = 11
keyToIdx _ = error "Invalid note name"

main :: IO ()
main = putStrLn "Hello, Haskell!"
