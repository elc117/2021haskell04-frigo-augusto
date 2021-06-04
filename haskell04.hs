-- Prática 04 de Haskell
-- Nome: Augusto Pagnossim Frigo

faixaIdoso :: Int -> String
faixaIdoso age
  |age >= 60 && age <= 64 = "IDO64"
  |age >= 65 && age <= 69 = "IDO69"
  |age >= 70 && age <= 74 = "IDO74"
  |age >= 75 && age <= 79 = "IDO79"
  |age > 80 = "IDO80"
  |otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos var = [(fst aux, snd aux, faixaIdoso (snd aux)) | aux <- var]

classifIdosos' :: [(String, Int)] -> [(String,Int,String)]
classifIdosos' var = map (\(name, age)-> (name, age,faixaIdoso age)) var

strColor :: (Int,Int,Int) -> String
strColor var = (\(value1, value2, value3) -> "rgb(" ++ show value1 ++ "," ++ show value2 ++ "," ++ show value3 ++")") var



genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs nCircles (x, y) radius = [(newX,y,radius)| newX <- [x + 0*radius, x + 4*radius .. x + 4*(nCircles -1)*radius]]


genReds :: Int -> [(Int,Int,Int)]
genReds n = [(x, 0,0) | x <- [255 `mod` 1, 255 `mod` 2 .. 255 `mod` n]]

