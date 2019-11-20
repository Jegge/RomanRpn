module RpnEval where
  
eval :: (Num a, Fractional a, Read a) => String -> a 
eval s = eval' (words s) []
    where
        eval' :: (Num a, Fractional a, Read a) => [String] -> [a] -> a 
        eval' ("+":ss) (a:b:xs) = eval' ss (b + a:xs)
        eval' ("-":ss) (a:b:xs) = eval' ss (b - a:xs)
        eval' ("*":ss) (a:b:xs) = eval' ss (b * a:xs)
        eval' ("/":ss) (a:b:xs) = eval' ss (b / a:xs)
        eval' [] [a]            = a
        eval' (s:ss) xs         = eval' ss (read s:xs) 
