module Main where

data ScaleType = Major | Minor

keys :: [String]
keys = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

keysToStr :: [Int] -> [String]
keysToStr arr = [keys !! idx | idx <- arr]

strToScaleType :: String -> ScaleType
strToScaleType "Major" = Major
strToScaleType "Minor" = Minor
strToScaleType _ = error "Invalid scale type"

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

getKeyFromOffset :: Int -> Int -> Int
getKeyFromOffset origin offset = mod (origin + offset) (length keys)

majorSteps :: [Int]
majorSteps = [0, 2, 4, 5, 7, 9, 11, 12]

minorSteps :: [Int]
minorSteps = [0, 2, 3, 5, 7, 8, 10, 12]

getScale :: Int -> ScaleType -> [Int]
getScale start Major = [mod (start + x) (length keys) | x <- majorSteps]
getScale start Minor = [mod (start + x) (length keys) | x <- minorSteps]

generateTriad :: String -> ScaleType -> Int -> [Int]
generateTriad scaleBase scaleType deg =
    let
        scale = getScale (keyToIdx scaleBase) scaleType
        degree = deg - 1
     in
        [scale !! mod degree (length keys), scale !! mod (degree + 2) (length keys), scale !! mod (degree + 4) (length keys)]

generateSeventhChord :: String -> ScaleType -> Int -> [Int]
generateSeventhChord scaleBase scaleType deg =
    let
        scale = getScale (keyToIdx scaleBase) scaleType
        degree = deg - 1
     in
        [scale !! mod degree (length keys), scale !! mod (degree + 2) (length keys), scale !! mod (degree + 4) (length keys), scale !! mod (degree + 6) (length keys)]

main :: IO ()
main = putStrLn "Hello, Haskell!"
