module Day1
    where

data Rotation = R Int | L Int
    deriving (Eq, Ord, Show)

parseRotation :: String -> Rotation
parseRotation xs
    | head xs == 'R' = R rest
    | head xs == 'L' = L rest
    | otherwise = error "No parse."
        where
            rest = read (tail xs) :: Int

type Dial = Int

rotateDial :: Dial -> Rotation -> Dial
rotateDial d (R n) = (d + n) `mod` 100
rotateDial d (L n) = (d - n) `mod` 100

countDialOnZero :: Dial -> [Rotation] -> Int
countDialOnZero d rots = aux d rots 0
    where
        aux _ [] i = i
        aux d (rot:rots) i = let newDial = rotateDial d rot in
            if 
                newDial == 0
            then
                aux newDial rots (i + 1)
            else
                aux newDial rots i