module Main where
  
import Roman 
import RpnEval

instance Fractional Roman where
  (/) a b        = fromIntegral (a `div` b)
  fromRational a = fromIntegral $ round a

eval' :: String -> Roman
eval' = eval 

main :: IO ()
main = interact $ show . eval'