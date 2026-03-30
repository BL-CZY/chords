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
majorSteps = [0, 2, 4, 5, 7, 9, 11]

minorSteps :: [Int]
minorSteps = [0, 2, 3, 5, 7, 8, 10]

getScale :: String -> ScaleType -> [Int]
getScale start Major = [mod (keyToIdx start + x) (length keys) | x <- majorSteps]
getScale start Minor = [mod (keyToIdx start + x) (length keys) | x <- minorSteps]

generateTriad :: String -> ScaleType -> Int -> [Int]
generateTriad scaleBase scaleType deg =
    let
        scale = getScale scaleBase scaleType
        degree = deg - 1
     in
        [scale !! mod degree (length scale), scale !! mod (degree + 2) (length scale), scale !! mod (degree + 4) (length scale)]

generateSeventhChord :: String -> ScaleType -> Int -> [Int]
generateSeventhChord scaleBase scaleType deg =
    let
        scale = getScale scaleBase scaleType
        degree = deg - 1
     in
        [scale !! mod degree (length scale), scale !! mod (degree + 2) (length scale), scale !! mod (degree + 4) (length scale), scale !! mod (degree + 6) (length scale)]

majorProgression :: [Int]
majorProgression = [4, 5, 3, 6, 2, 5, 1]

minorProgression :: [Int]
minorProgression = [4, 7, 3, 6, 2, 5, 1]

generateProgression :: String -> ScaleType -> [Int] -> [(Int, [Int])]
generateProgression scaleBase scaleType progression = [(x, generateSeventhChord scaleBase scaleType x) | x <- progression]

degreeToRomanNumeral :: Int -> String
degreeToRomanNumeral x
    | x < 4 = replicate x 'I'
    | x == 4 = "IV"
    | x == 5 = "V"
    | otherwise = 'V' : replicate (x - 5) 'I'

printProgression :: [(Int, [Int])] -> IO ()
printProgression input = putStr (concat [degreeToRomanNumeral degree ++ ": " ++ concat [keys !! key ++ ", " | key <- chord] ++ ['\n'] | (degree, chord) <- input])

main :: IO ()
main = putStrLn "Hello, Haskell!"
