module Roman (
  Roman,
  isRoman  
  ) where 

import Data.List
import Data.Char

data RomanNumeral = I
                  | V
                  | X
                  | L
                  | C
                  | D
                  | M
                  deriving (Eq, Show, Read)

newtype Roman = Roman [RomanNumeral]

instance Show Roman where
    show (Roman digits) = concatMap show digits

instance Read Roman where
    readsPrec _ n = let text = read' n in [(reduce $ Roman (parse text), drop (length text) n)]
        where 
            read' = takeWhile (\a -> isRoman a || isWhite a) . map toUpper
            parse = map read . map (\a -> [a]) . takeWhile isRoman . dropWhile isWhite

instance Eq Roman where
    (==) a b = toInteger a == toInteger b

instance Ord Roman where
    (<=) a b = toInteger a <= toInteger b

instance Num Roman where
    fromInteger i = if i < 0 then error "Nulla" else reduce $ Roman (genericTake i $ repeat I)
    n + m         = fromInteger (toInteger n + toInteger m)
    n * m         = fromInteger (toInteger n * toInteger m)
    n - m         = fromInteger (toInteger n - toInteger m)
    abs    (n)    = n
    signum (n)    = Roman [I] 
   
instance Integral Roman where
    toInteger n  = let Roman e = expand n in genericLength e
    div n m      = fromInteger (toInteger n `div` toInteger m)
    quotRem n m  = let (q,r) = (toInteger n) `quotRem` (toInteger m) in (fromInteger q, fromInteger r)

instance Real Roman where
    toRational = toRational . toInteger

instance Enum Roman where
    toEnum   = fromInteger . fromIntegral
    fromEnum = fromIntegral . toInteger


isRoman :: Char -> Bool
isRoman c = c `elem` "IVXLCDM"

isWhite :: Char -> Bool
isWhite c = c `elem` "\t \r\n"

-- | Replace all occurences of x with y in l
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ [] l = l
replace [] _ l = l
replace _ _ [] = []
replace x y l@(lh:ls) 
    | isPrefixOf x l = y ++ (replace x y $ drop (length x) l)
    | otherwise      = lh : replace x y ls

reduce :: Roman -> Roman
reduce (Roman n) = Roman (reduce' n) 
    where 
        reduce' = replace [D,C,D]     [C,M] .
                  replace [D,D]       [M]   .
                  replace [C,C,C,C]   [C,D] . 
                  replace [C,C,C,C,C] [D]   .
                  replace [L,X,L]     [X,C] .
                  replace [L,L]       [C]   .
                  replace [X,X,X,X]   [X,L] .
                  replace [X,X,X,X,X] [L]   .
                  replace [V,I,V]     [I,X] .
                  replace [V,V]       [X]   .
                  replace [I,I,I,I]   [I,V] .
                  replace [I,I,I,I,I] [V] 

expand :: Roman -> Roman
expand (Roman n) = Roman (expand' n)
    where 
        expand' = replace [V]   [I,I,I,I,I] .
                  replace [I,V] [I,I,I,I]   .
                  replace [X]   [V,V]       .
                  replace [I,X] [V,I,V]     .
                  replace [L]   [X,X,X,X,X] .
                  replace [X,L] [X,X,X,X]   .
                  replace [C]   [L,L]       .
                  replace [X,C] [L,X,L]     .
                  replace [D]   [C,C,C,C,C] .
                  replace [C,D] [C,C,C,C]   . 
                  replace [M]   [D,D]       .
                  replace [C,M] [D,C,D]




