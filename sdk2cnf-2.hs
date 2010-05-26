import Control.Monad (guard)
import Data.Char

data Literal = Literal Bool Int Int Int deriving Show
type Clause  = [Literal]

readValue v
  | v `elem` ['1'..'9'] = (ord v) - (ord '1') + 1
  | v `elem` ['A'..'Z'] = (ord v) - (ord 'A') + 1
  | otherwise = error ("WTF? Cell value out of range: '" ++ (show v) ++ "'")

-- TODO!!!
formatLiteral :: Int -> Literal -> Int
formatLiteral n (Literal b r c bi) = if b then i else -i
  where i = v + (c-1) * (n^2) + (r-1) * (n^4)

formatClauses :: Int -> [Clause] -> [[Int]]
formatClauses n cs = map f cs
  where f c = (map (formatLiteral n) c) ++ [0]

printClauses :: Int -> [Clause] -> IO ()
printClauses n cs = mapM_ f (formatClauses n cs)
  where f c = putStrLn $ unwords $ map show c

ithBit v i = -- TODO

validValues m n = --TODO

invalidValues m n = -- TODO

g :: Int -> (Int, Int, Int) -> Clause
g m (r, c, v) = [(Literal (ithBit v bi) r c bi) | bi <- [1..m]]

deMorgans clause = map f clause
  where f (Literal b r c bi) = (Literal (not b) r c bi)

c1 :: Int -> Int -> [Clause]
c1 m n = do r  <- [1..n^2]; c <- [1..n^2]
            r' <- [1..n^2]; v <- validValues m n
            guard (r /= r')
            return (deMorgans (g m (r, c, v))) ++ (deMorgans (g m (r', c, v)))

c2 :: Int -> Int -> [Clause]
c2 m n = do r  <- [1..n^2]; c <- [1..n^2]
            c' <- [1..n^2]; v <- validValues m n
            guard (c /= c')
            return (deMorgans (g m (r, c, v))) ++ (deMorgans (g m (r, c', v)))

c3 :: Int -> Int -> [Clause]
c3 m n = do r <- [1..n^2]; c <- [1..n^2];
            let i = (r-1) `div` n
            let j = (c-1) `div` n
            r' <- [(i*n+1)..(i*n+n)]
            c' <- [(j*n+1)..(j*n+n)]
            guard (r /= r')
            guard (c /= c')
            v <- validValues m n
            return (deMorgans (g m (r, c, v))) ++ (deMorgans (g m (r', c', v)))

c4 :: Int -> Int -> [Clause]
c4 m n = map (deMorgan . (g m)) rcvs
  where rcvs = [(r, c, v) | r <- [1..n^2], c <- [1..n^2], v <- invalidValues m n]

-- TODO
-- map (:[]) a
-- g m (r, c, v) returns a clause, but we want to break the clause into
-- many, single literal clauses and return those.
c5 :: Int -> Int -> String -> [Clause]
c5 m n q = map toLit rcvs
   where rcvs = filter ((/= '_') . snd) $ zip rcs $ filter (/= '\n') q
         rcs  = [(r, c) | r <- [1..n^2], c <- [1..n^2]]
         toLit ((r, c), v) = g m (r, c, (readValue v))

main = do input <- getContents
          let nsqrd = length $ head $ lines input
          let n = floor $ sqrt $ (fromInteger (toInteger (nsqrd))::Float)
          let m = ceiling $ logBase 2 (fromInteger (toInteger (n^2))::Float)
          let cs = (c1 m n) ++ (c2 m n) ++ (c3 m n) ++ (c4 m n) ++ (c5 m n input)
          putStrLn ("p cnf " ++ show (m*(n^4)) ++ " " ++ show (length cs))
          printClauses n cs
