-- pattern matching statt if else
potenz :: Double -> Int -> Double
potenz x 0 = 1
potenz x n  = x * (potenz x (n-1))

-- Datentypen
data Coordinate = Coordinate {y :: Int, x :: Int} deriving Show


data List a = List {element :: a, rest::(List a)}
            | Empty deriving Show

in_range :: Integer -> Integer -> Integer -> Bool 
in_range min max x = x >= min && x <= max


in_range' :: Ord p => p -> p -> p -> Bool
in_range' min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x
-- in_range, in_range' und '' sind das gleiche, andere Schreibweise
in_range'' :: Ord p => p -> p -> p -> Bool
in_range'' min max x =
    let in_lower_bound = min <= x
        in_upper_bound = max >= x
    in
        in_lower_bound && in_upper_bound
-- für Output
-- Strings sind char arrays "abc" = ['a', 'b', 'c'] = 'a':'b'
data Pair a b = PairConstructor a b deriving Show

--       (Integral a) heißt, dass lucky von der Schnittstelle Integral erbt
lucky :: (Integral a) => a -> String 
lucky 7 = "lucky number Seven!"
lucky x = "sorry not a seven"

-- gib das letzte Element einer Liste zurück
last' :: [a] -> a
-- tail xs gibt dir alles bis auf head der liste
-- also if (liste besteht nur aus einem element),
-- dann gib dieses Element zurück
-- sonst rek aufruf mit tail von xs
last' xs = if (null (tail xs)) then (head xs)
          else last' (tail xs)

-- besser mit Pattern Matching
last'' :: [a] -> a
last''(x:[]) = x
last''(x:xs) = last'' xs

faf :: [Integer]
faf = [2,3,5,7,11]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

data Board = Board{row0::String,
                   row1::String,
                   row2::String,
                   row3::String,
                   row4::String,
                   row5::String,
                   row6::String,
                   row7::String,
                   row8::String,
                   row9::String} deriving Show
    
row0' = "rheagaehr"
row1' = "/9"
-- ersetze jede Zahl mit dem vielfachen dieser Zahl an Einsen
replaceDigitWithOnes :: Char  -> String
replaceDigitWithOnes '2' = "11"
replaceDigitWithOnes '3' = "111"
replaceDigitWithOnes '4' = "1111"
replaceDigitWithOnes '5' = "11111"
replaceDigitWithOnes '6' = "111111"
replaceDigitWithOnes '7' = "1111111"
replaceDigitWithOnes '8' = "11111111"
replaceDigitWithOnes '9' = "111111111"
replaceDigitWithOnes x = [x]

--applyNewStrings :: String -> String
--applyNewStrings [] = []
--applyNewStrings (x:xs) = replaceDigitWithOnes x : applyNewStrings xs

newList :: String
newList = concatMap replaceDigitWithOnes "1c5c1"

-- Aufgabe 5 Haskell Eiscreme
data Sorte = Vanille | Erdbeer | Schoko deriving Show
data Eis = Eis Sorte Eis | Waffel

-- a) erstelle ein Eis Vanille(oben) -> Schoko

zweiKugelnEis :: Eis
zweiKugelnEis = Eis Vanille (Eis Schoko Waffel)

-- b) Eis als String darstellen 
instance Show Eis where
    show = toString

toString :: Eis -> String
toString (Eis a b) = toStringSorte a ++ toString b
toString Waffel = ">>>"

toStringSorte :: Sorte -> String
toStringSorte Vanille = "(" ++ "V" ++ ")"
toStringSorte Erdbeer = "(" ++ "E" ++ ")"
toStringSorte Schoko = "(" ++ "S" ++ ")"

data Kunde = Kunde {name :: String, favoritSorte :: Sorte, purchaseHistory :: [Eis]}




split :: [a] -> ([a], [a])
split = recurse [] []
  where
  recurse odds evens [] = (reverse odds, reverse evens)
  recurse odds evens [x] = (reverse (x:odds), reverse evens)
  recurse odds evens (x:y:rest) = recurse (x:odds) (y:evens) rest


spreadInSphere :: (Float, Float) -> Float -> [(Float, Float)] -> Float
spreadInSphere center radius = 
  foldr (+)  0 .  filter  (<= radius) . map (distance center)
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)


data FormInput
  = FloatField Float (Float -> Bool)
  | TextArea String TextConfig
  | Dropdown (Maybe Int) Bool [String]

data TextConfig = TextCfg
  { minLength :: Int
  , maxLength :: Int
  }

isTextual :: FormInput -> Bool
isTextual (TextArea _ _) = True
isTextual _ = False


isValid :: FormInput -> Bool
isValid (FloatField _ _) = True
isValid (TextArea a b) = if length a >= minLength(b) then if length a <= maxLength(b) then True else False