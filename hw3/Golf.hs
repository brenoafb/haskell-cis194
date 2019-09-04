module Golf where
import Data.List
import qualified Data.Map as M

skips :: [a] -> [[a]]
skips xs = map ((flip f) xs) [1..length xs]
  where f n xs = case drop (n-1) xs of
                          (y:ys) -> y : f n ys
                          [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = let t = zip3 xs (tail xs) (tail $ tail xs)
  in map (\(_,y,_) -> y) $ filter (\(x,y,z) -> y > x && y > z) t

histogram :: [Integer] -> String
histogram ls = let ps = map (\p@(x:_) -> (x, length p)) . group $ sort ls
                   occ = M.fromList ps
                   get n = case M.lookup n occ of Nothing -> 0
                                                  Just x  -> x
                   f n = (show n) ++ "=" ++ (replicate (get n) '*')
               in (++ "\n") . concat . intersperse "\n" . reverse. transpose $ map f [0..9]
