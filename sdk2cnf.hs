import Control.Monad (guard)
import Data.Char

data Literal = Literal Bool Int Int Int deriving Show
type Clause  = [Literal]

readValue v
  | v `elem` ['1'..'9'] = (ord v) - (ord '1') + 1
  | v `elem` ['A'..'Z'] = (ord v) - (ord 'A') + 1
  | otherwise = error ("WTF? Cell value out of range: '" ++ (show v) ++ "'")

formatLiteral :: Int -> Literal -> Int
formatLiteral n (Literal b r c v) = if b then i else -i
  where i = v + (c-1) * (n^2) + (r-1) * (n^4)

formatClauses :: Int -> [Clause] -> [[Int]]
formatClauses n cs = map f cs
  where f c = (map (formatLiteral n) c) ++ [0]

printClauses :: Int -> [Clause] -> IO ()
printClauses n cs = mapM_ f (formatClauses n cs)
  where f c = putStrLn $ unwords $ map show c

c1 :: Int -> [Clause]
c1 n = do r <- [1..n^2]; c <- [1..n^2]
          return [(Literal True r c v) | v <- [1..n^2]]

c2 :: Int -> [Clause]
c2 n = do r <- [1..n^2]; c  <- [1..n^2]
          v <- [1..n^2]; v' <- [1..n^2]
          guard (v /= v')
          return [(Literal False r c v), (Literal False r c v')]

c3 :: Int -> [Clause]
c3 n = do r <- [1..n^2]; c  <- [1..n^2]
          v <- [1..n^2]; r' <- [1..n^2]
          guard (r /= r')
          return [(Literal False r c v), (Literal False r' c v)]

c4 :: Int -> [Clause]
c4 n = do r <- [1..n^2]; c  <- [1..n^2]
          v <- [1..n^2]; c' <- [1..n^2]
          guard (c /= c')
          return [(Literal False r c v), (Literal False r c' v)]

c5 :: Int -> [Clause]
c5 n = do r <- [1..n^2]; c  <- [1..n^2]; v <- [1..n^2]
          let i = (r-1) `div` n
          let j = (c-1) `div` n
          r' <- [(i*n+1)..(i*n+n)]
          c' <- [(j*n+1)..(j*n+n)]
          guard (r /= r')
          guard (c /= c')
          return [(Literal False r c v), (Literal False r' c' v)]

c6 :: Int -> String -> [Clause]
c6 n q = map toLit rcvs
   where rcvs = filter ((/= '_') . snd) $ zip rcs $ filter (/= '\n') q
         rcs  = [(r, c) | r <- [1..n^2], c <- [1..n^2]]
         toLit ((r, c), v) = [(Literal True r c (readValue v))]

main = do input <- getContents
          let nsqrd = length $ head $ lines input
          let n = floor $ sqrt $ (fromInteger (toInteger (nsqrd))::Float)
          let cs = (c1 n) ++ (c2 n) ++ (c3 n) ++ (c4 n) ++ (c5 n) ++ (c6 n input)
          putStrLn ("p cnf " ++ show (n^6) ++ " " ++ show (length cs))
          printClauses n cs
